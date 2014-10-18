{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances, GADTs, LambdaCase, LiberalTypeSynonyms     #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, PatternSynonyms, RankNTypes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators, ViewPatterns    #-}
module Text.InputMethod.SKK
       (module Text.InputMethod.SKK.Dictionary,
        toInput, parseDictionary, SKKCommand(..),
        -- * Converters
        romanConv, romanConvE, defKanaTable, defRomanConvE,
        -- * Data-types and lenses
        KanaEntry(..), ConvMode(..), KanaResult(..),
        _Converted, _NoHit, _InProgress, newInput,
        hiraConv, kataConv, hanKataConv, nextState,
        parseKanaTable, formatKanaTable, skkConv, skkConvE,
        KanaTable(..), kanaDic, SKKResult(..), defSKKConvE,
        -- * misc
        Pager, CandidateSelector, slice,  _Idle,
        _Converting, _Okuri, _ConvFound, _ConvNotFound,
        _Incoming, _Backspace, _Convert, _ToggleHankaku,
        _Undo, _Finish, _Complete, _ToggleKana,
        _CurrentState, _Refresh,
        defCSelector, defPager, isIdling, toggleKana
        ) where
import Text.InputMethod.SKK.Dictionary
import Text.InputMethod.SKK.Misc

import           Control.Applicative   (Applicative, pure, (<$>))
import           Control.Arrow         (second, (***), (>>>))
import           Control.Effect        hiding (select, swap)
import           Control.Lens          (anyOf, each, ix, makeLenses, makePrisms)
import           Control.Lens          (makeWrapped, to, traverse, use, uses)
import           Control.Lens          (view, (%=), (&), (.=), (<%=), (<<>=))
import           Control.Lens          ((<>=), (<?=), (?=), (^.), (^?), _2)
import           Control.Lens          (_Just)
import           Control.Lens          (isn't)
import           Control.Lens.Extras   (is)
import           Control.Monad         (unless)
import           Control.Zipper        ((:>>), Top, focus, fromWithin, leftmost)
import           Control.Zipper        (rightward, zipper)
import           Data.Attoparsec.Text  (parseOnly)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isAlpha, isAscii, isAsciiUpper, toLower)
import           Data.Data             (Data, Typeable)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (elemIndex, partition, unfoldr)
import           Data.Maybe            (fromMaybe, isJust)
import           Data.Maybe            (listToMaybe)
import           Data.Monoid           (Monoid (..), (<>))
import           Data.Reflection       (Given (..), give)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Trie             hiding (lookup, null)
import qualified Data.Trie             as Trie
import           Data.Tuple            (swap)
import           FRP.Ordrea            (Event, SignalGen, eventToBehavior)
import           FRP.Ordrea            (externalE, mapAccumE, newExternalEvent)
import           FRP.Ordrea            (start, triggerExternalEvent)
import           FRP.Ordrea            ((<@>))
import           FRP.Ordrea            (Behavior)
import           Language.Haskell.TH   (litE, runIO, stringL)
import           Prelude               hiding (lookup)

data KanaEntry = KanaEntry { _hiraConv    :: T.Text
                           , _kataConv    :: T.Text
                           , _hanKataConv :: T.Text
                           , _nextState   :: Maybe Char
                           } deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''KanaEntry

data KanaResult = Converted T.Text | NoHit | InProgress BS.ByteString
               deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''KanaResult
makePrisms ''KanaResult
makeLenses ''ConvMode
makePrisms ''ConvMode

newtype KanaTable = KanaTable { _kanaDic :: Trie KanaEntry }
                  deriving (Show, Eq, Typeable)
makeLenses ''KanaTable
makeWrapped ''KanaTable

toInput :: T.Text -> Input
toInput "" = Input "" Nothing
toInput str
  | T.length str == 1
    = Input str Nothing
  | isAlpha (T.last str) && isAscii (T.last str)
    = Input (T.init str) (Just $ T.last str)
  | otherwise =  Input str Nothing

parseDictionary :: T.Text -> Either String Dictionary
parseDictionary = parseOnly dictionary

convChar :: Applicative f => ConvMode
         -> (T.Text -> f T.Text) -> KanaEntry -> f KanaEntry
convChar Hiragana = hiraConv
convChar Katakana = kataConv
convChar HankakuKatakana = hanKataConv
convChar Ascii = const $ pure

data KanaState = KanaState { _tempResults :: [T.Text]
                           , _kanaBuf     :: BS.ByteString
                           , _dicts       :: [Trie KanaEntry]
                           } deriving (Show, Eq, Typeable)
makeLenses ''KanaState

newKanaState :: KanaTable -> KanaState
newKanaState table = KanaState { _tempResults = []
                               , _kanaBuf = ""
                               , _dicts   = [table ^. kanaDic]
                               }

isConverting :: KanaState -> Bool
isConverting a = not (null  $ a ^. tempResults) || not (BS.null $ a ^. kanaBuf)

mapAccumE' :: (s -> a -> (s, b)) -> s -> Event a -> SignalGen (Event b)
mapAccumE' fun s0 e = mapAccumE s0 (flip fun <$> e)

romanConvE :: KanaTable -> ConvMode -> Event Char -> SignalGen (Event [KanaResult])
romanConvE dic mode input = mapAccumE' (romanConv dic mode) (newKanaState dic) (Just <$> input)

runSW :: Monoid w => s -> Effect (Writer w :+ State s :+ Nil) () -> (s, w)
runSW s0 act = swap $ runEffect $ runState s0 $ snd <$> runWriter act

emit :: EffectWriter [a] l => a -> Effect l ()
emit = tell . (:[])

when :: EffectWriter [SKKResult] l => Bool -> Effect l () -> Effect l ()
when True  act = act
when False _   = emit $ Idle []

romanConv :: KanaTable -> ConvMode -> KanaState -> Maybe Char -> (KanaState, [KanaResult])
romanConv dic mode s = runSW s . go
  where
    inProgress t = emit . InProgress =<< kanaBuf <<>= t
    reset = put $ newKanaState dic
    converted t = emit (Converted t) >> reset
    go Nothing = do
      tmps <- use tempResults
      if null tmps
        then return ()
        else converted $ head tmps
    go (Just '\DEL') = do
      mt <- use kanaBuf
      if BS.null mt
        then reset >> emit NoHit
        else do
          buf <- kanaBuf <%= BS.init
          dicts %= tail
          tempResults %= tailL
          emit $ InProgress buf
    go (Just c) = do
      let inp = BS.singleton c
      (mans, ps') <- uses dicts (lookupBy (,) inp . head)
      pre <- use tempResults
      let emitPre = if null pre then return () else converted $ head pre
      case mans of
        Just a -> do
          let out   = a ^. convChar mode
              mnext = a ^. nextState
              app d = maybe (d :) (:) $ flip prefixes d . BS.singleton <$> a ^. nextState
          if Trie.null ps'
            then do
              converted out
              maybe (return ()) (go . Just) mnext
            else do
              tempResults %= (out :)
              dicts %= app ps'
              inProgress inp
        Nothing | Trie.null ps' -> do
                    emitPre >> reset
                    if Trie.null (submap inp $ dic ^. kanaDic)
                      then reset >> emit NoHit
                      else go (Just c)
                | otherwise     -> do
                    dicts      %= (ps' :)
                    tempResults %= tailL
                    inProgress inp

tailL :: [t] -> [t]
tailL [] = []
tailL (_:xs) = xs

prefixes :: BS.ByteString -> Trie a -> Trie a
prefixes = lookupBy ((snd .) . (,))

type Pager = forall a. [a] -> [[a]]
type CandidateSelector = forall a. Char -> [a] -> Maybe a

data SKKResult = Idle [KanaResult]
               | Converting T.Text
               | Okuri T.Text T.Text
               | Finished T.Text
               | ConvFound T.Text (Maybe (Char, T.Text)) [T.Text]
               | ConvNotFound T.Text (Maybe (Char, T.Text))
               deriving (Read, Show, Eq, Ord, Data, Typeable)

data SKKState = SKKState { _kanaState      :: Maybe KanaState
                         , _okuriState     :: Maybe (Char, T.Text)
                         , _convBuf        :: Maybe T.Text
                         , _slashed        :: Bool
                         , _compCandidates :: Maybe (Top :>> [T.Text] :>> T.Text)
                         } deriving (Typeable)
makeLenses ''SKKState
makePrisms ''SKKResult

isIdling :: SKKResult -> Bool
isIdling r =
  or [anyOf (_Idle.traverse) (isn't _InProgress) r
     ,r & is _Finished, r & is _ConvNotFound, r & is _ConvFound]

newSKKState :: SKKState
newSKKState = SKKState Nothing Nothing Nothing False Nothing

okuriBuf :: Applicative f => (T.Text -> f T.Text) -> SKKState -> f SKKState
okuriBuf = okuriState . _Just . _2

hasOkuri = okuriState . to isJust
converting = convBuf . to isJust
kanaing = kanaState . to isJust

data InputView = ConvertInput
               | OkuriInput
               | AndThen InputView InputView
               | NormalInput
               | StartConvert
               | StartOkuri
               | StartSlash
               | SlashInput
                 deriving (Read, Show, Eq, Ord)

viewS :: Char -> SKKState -> InputView
viewS c s
  | c == 'Q' && not (s ^. converting) = StartConvert
  | c == '/' && not (s ^. converting) && not (maybe False isConverting $ s ^. kanaState) = StartSlash
  | isAsciiUpper c && not (s ^. converting) = ConvertInput -- Start new conversion phase
  | isAsciiUpper c && s ^. converting && not (s ^. hasOkuri) &&
    (s ^. convBuf)  /= Just "" && not (s ^. slashed)  = StartOkuri `AndThen` OkuriInput
  | s ^. converting && s ^. slashed = SlashInput
  | s ^. converting && not (s ^. hasOkuri) = ConvertInput
  | s ^. converting && s ^. hasOkuri = OkuriInput
  | otherwise = NormalInput

nextCompletion :: (a :>> b) -> (a :>> b)
nextCompletion s = fromMaybe  (s & leftmost) (s & rightward)

data SKKCommand = Incoming Char
                | Backspace
                | Convert
                | ToggleHankaku
                | Undo
                | Finish
                | Complete
                | ToggleKana
                | CurrentState
                | Refresh
                  deriving (Read, Show, Eq, Ord)

data SKKEnv = SKKEnv { table_  :: KanaTable
                     , kana_   :: ConvMode
                     , skkDic_ :: Dictionary
                     } deriving (Show, Eq, Typeable)
makePrisms ''SKKCommand

table :: Given SKKEnv => KanaTable
table = table_ given

kana :: Given SKKEnv => ConvMode
kana = kana_ given

skkDic :: Given SKKEnv => Dictionary
skkDic = skkDic_ given

skkConv :: KanaTable -> ConvMode
        -> Dictionary
        -> SKKCommand -> SKKState -> (SKKState, [SKKResult])
skkConv tbl mode dict cmd s =
  give (SKKEnv tbl mode dict) $ runSW s $ skkConv0 cmd

type Machine = Effect (Writer [SKKResult] :+ State SKKState :+ Nil)

skkConv0 :: Given SKKEnv => SKKCommand -> Machine ()
skkConv0 cmd | Ascii <- kana =
  case cmd of
    Incoming c    -> emit $ Idle [Converted $ T.singleton c]
    _             -> emit $ Idle []

skkConv0 CurrentState = use convBuf >>= \case
  Nothing  -> use kanaState >>= \case
    Nothing -> emit $ Idle []
    Just ks -> emit $ Idle [InProgress (ks ^. kanaBuf)]
  Just buf -> do
    sl <- use slashed
    mok <- use okuriState
    kn <- uses kanaState $ maybe "" (T.decodeUtf8 . view kanaBuf)
    if | sl -> emit $ Converting buf
       | Just (_, ok) <- mok -> emit $ Okuri buf (ok <> kn)
       | otherwise -> emit $ Converting $ buf <> kn

skkConv0 Undo = do
  isConv <- use converting
  when isConv $ do
    maybe (put newSKKState) (const $ okuriState .= Nothing) =<< use okuriState

skkConv0 ToggleHankaku = do
  isConv <- use converting
  mbuf <- use convBuf
  when isConv $ do
    case mbuf of
      Just buf -> do
        lo <- use $ kanaState._Just.tempResults
        finish $ toHankaku $ buf <> fromMaybe "" (listToMaybe lo)
      Nothing -> emit $ Idle []

skkConv0 Refresh = put newSKKState

skkConv0 Convert = do
  conving <- use converting
  when conving $ do
    mok <- use okuriState
    lo <- either fst id <$> toKana Nothing
    case mok of
      Nothing -> do -- Convert without Okurigana
        body <- fromMaybe "" <$> (convBuf <<>= Just lo)
        let mcss = map (view tango) .
                   filter (is _Candidate) <$>
                   lookup (Input body Nothing) skkDic
        case mcss of
          Nothing  -> notFound
          Just css -> convFound css
      Just (okCh, okBuf0) -> do -- Convert with Okurigana
        let okBuf = okBuf0 <> lo
        okuriState ?= (okCh, okBuf)
        body <- uses convBuf (fromMaybe "")
        case lookup' (Input body $ Just $ toLower okCh) okBuf skkDic of
          Just css -> convFound css
          Nothing -> notFound

skkConv0 Backspace = do
  isConv <- use converting
  if isConv
    then use okuriState >>= \case
    Just (_, buf) -> do
      comping <- use kanaing
      if | comping && not (T.null buf) -> skkConv0 (Incoming '\DEL')
         | T.null buf -> do
             okuriState .= Nothing
             kanaState  .= Nothing
             convertingWith ""
         | otherwise -> do
             okuriState %= fmap (second T.init)
             okuriWith ""
    Nothing -> do
      compCandidates .= Nothing
      buf <- fromMaybe "" <$> use convBuf
      isKana <- uses kanaState (maybe False isConverting)
      if | isKana -> toKana (Just '\DEL') >>= \case
             Right comp -> do
               convBuf <>= Just comp
               convertingWith ""
             Left (comp, temp) -> do
               convBuf <>= Just comp
               convertingWith temp
         | T.null buf -> do
             convBuf .= Nothing
             put newSKKState
             emit $ Idle [NoHit]
         | otherwise -> do
             convBuf %= fmap T.init
             convertingWith ""
    else do
      isKana <- uses kanaState (maybe False isConverting)
      if isKana
        then do
          ks0 <- fromMaybe (newKanaState table) <$> use kanaState
          let (ks', rs) = romanConv table kana ks0 (Just '\DEL')
          kanaState ?= ks'
          emit $ Idle rs
        else emit $ Idle [NoHit]

skkConv0 Finish =
  maybe (skkConv0 $ Incoming '\n') finish =<< use convBuf

skkConv0  Complete = do
  trail <- trailingText
  kanaState .= Nothing
  convBuf <>= Just trail
  mcomps <- use compCandidates
  case mcomps of
    Nothing -> do
      buf <- fromMaybe "" <$> use convBuf
      unless (T.null buf) $ do
        let cs = filter (buf `T.isPrefixOf`) $
                 HM.keys $ skkDic ^. okuriNasiDic
        unless (null cs)$ do
          compCandidates ?= (zipper cs & fromWithin traverse)
          convBuf ?= head cs
          convertingWith ""
    Just cur -> do
      cur' <- compCandidates <?= (cur & nextCompletion)
      convBuf ?= cur' ^. focus
      convertingWith ""

skkConv0 ToggleKana = do
  lo <- trailingText
  buf <- fromMaybe "" <$> use convBuf
  finish $ toggleKana kana (buf <> lo)

skkConv0 (Incoming c) = go . viewS c  =<< get
  where
    go StartConvert = do
      mans <- toKana Nothing
      case mans of
        Right a | not (T.null a) -> emit $ Idle [ Converted a ]
        _ -> do compCandidates .= Nothing
                convBuf .= Just ""
                convertingWith ""
    go StartOkuri = do
      eans <- toKana Nothing
      case eans of
        Right a | not (T.null a) -> do
          convBuf <>= Just a
          return ()
        Left (comp, lo) | not (T.null lo) -> do
          convBuf <>= Just comp
          okuriState ?= (T.head lo, "")
          return ()
        _ -> return ()
    go SlashInput = do
      convBuf <>= Just (T.singleton c)
      convertingWith ""
    go StartSlash = do
      slashed .= True
      convBuf ?= ""
      convertingWith ""
    go (AndThen p q) = go p >> go q
    go ConvertInput = do
      compCandidates .= Nothing
      rslt <- toKana (Just (toLower c))
      case rslt of
        Right ans -> do
          convBuf <>= Just ans
          convertingWith ""
        Left (comp, temp) -> do
          convBuf <>= Just comp
          convertingWith temp
    go OkuriInput = do
      okr  <- use okuriState
      rslt <- toKana $ Just $ toLower c
      case rslt of
        Left (comp, temp) -> do
          let (okCh, okBuf) = maybe (c, comp) (second (<> comp)) okr
          okuriState ?= (okCh, okBuf)
          okuriWith temp
        Right str -> do
          let ok = maybe (c, str) (second (<> str)) okr
          okuriState ?= ok
          skkConv0 Convert
    go NormalInput = do
      ks0 <- fromMaybe (newKanaState table) <$> use kanaState
      let (ks', rs) = romanConv table kana ks0 (Just c)
      kanaState ?= ks'
      emit $ Idle rs

trailingText :: Given SKKEnv => Machine T.Text
trailingText = either fst id <$> toKana Nothing

finish :: Given SKKEnv => T.Text -> Machine ()
finish str = do
  a <- use okuriState
  put newSKKState
  emit $ Finished $ convKana Hiragana kana $ str <> maybe "" snd a

convertingWith :: Given SKKEnv => T.Text -> Machine ()
convertingWith temp = do
  buf <- use convBuf
  emit $ Converting $ convKana Hiragana kana $ (fromMaybe "" buf) <> temp

notFound :: Machine ()
notFound  = do
  body <- uses convBuf (fromMaybe "")
  mokuri <- use okuriState
  okuriState .= Nothing
  emit $ ConvNotFound body mokuri

convFound :: Given SKKEnv => [T.Text] -> Machine ()
convFound css = do
  mok  <- use okuriState
  body <- uses convBuf (fromMaybe "")
  okuriState .= Nothing
  emit $ ConvFound body (second (convKana Hiragana kana) <$> mok) css

okuriWith :: Given SKKEnv => T.Text -> Machine ()
okuriWith temp = do
  body  <- use convBuf
  okBuf <- use okuriBuf
  emit $ Okuri (convKana Hiragana kana $ fromMaybe "" body)
               (convKana Hiragana kana $ okBuf <> temp)

toKana :: Given SKKEnv => Maybe Char ->  Machine (Either (T.Text, T.Text) T.Text)
toKana mc = do
  ks <- fromMaybe (newKanaState table) <$> use kanaState
  let (ks', rs) = romanConv table Hiragana ks mc
      (prgs, cvd) = partition (is _InProgress) rs
      finished = T.concat $ map toT cvd
      toT NoHit          = maybe "" T.singleton mc
      toT (InProgress t) = T.decodeUtf8 t
      toT (Converted t)  = t
  if isConverting ks'
    then do
      kanaState ?= ks'
      return $ Left (finished, T.concat $ map toT prgs)
    else do
      kanaState .= Nothing
      return $ Right finished

defSKKConvE :: Event SKKCommand -> SignalGen (Event [SKKResult])
defSKKConvE = skkConvE defKanaTable Hiragana (pure sDictionary)

defPager :: Pager
defPager = splitAt 4 >>> map pure *** slice 7 >>> uncurry (++)

defCSelector :: CandidateSelector
defCSelector c cs = (cs ^?) . ix =<< elemIndex c "asdfjkl"

slice :: Int -> [a] -> [[a]]
slice n = unfoldr phi
  where
    phi [] = Nothing
    phi ys = Just $ splitAt n ys

skkConvE :: KanaTable -> ConvMode
         -> Behavior Dictionary
         -> Event SKKCommand -> SignalGen (Event [SKKResult])
skkConvE table mode dic input
  = mapAccumE newSKKState (skkConv table mode <$> dic <@> input)

newInput :: (Event b -> SignalGen (Event a)) -> IO (b -> IO a)
newInput toEv = do
  eev <- newExternalEvent
  step <- start $ do
    input <- externalE eev
    fmap head . eventToBehavior <$> toEv input
  return $ \inp -> triggerExternalEvent eev inp >> step

defKanaTable :: KanaTable
defKanaTable = parseKanaTable $(litE . stringL =<< runIO (readFile "data/kana-rule.conf"))

defRomanConvE :: Event Char -> SignalGen (Event [KanaResult])
defRomanConvE = romanConvE defKanaTable Hiragana

parseKanaTable :: T.Text -> KanaTable
parseKanaTable =
  KanaTable . fromList. map (build . T.splitOn ",") . filter (not . T.null) . T.lines
  where
    build [a, b, c, d] = (T.encodeUtf8 $ T.replace "&comma;" "," a, KanaEntry b c d Nothing)
    build [a, b, c, d, e] = (T.encodeUtf8 $ T.replace "&comma;" "," a, KanaEntry b c d $ Just $ T.head e)
    build _ = error "impossible!"

formatPair :: (BS.ByteString, KanaEntry) -> T.Text
formatPair (mid, ent) = T.intercalate "," [ T.replace "," "&comma;" $ T.decodeUtf8 mid, formatEntry ent ]

formatEntry :: KanaEntry -> T.Text
formatEntry (KanaEntry a b c md) =
  T.intercalate "," $ map (T.replace "," "&comma;") $
  a : b : c :maybe [] (return . T.singleton) md

formatKanaTable :: KanaTable -> T.Text
formatKanaTable = T.unlines . map formatPair . Trie.toList . view kanaDic
