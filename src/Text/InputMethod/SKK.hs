{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances, GADTs, LambdaCase, LiberalTypeSynonyms     #-}
{-# LANGUAGE MultiParamTypeClasses, NoMonomorphismRestriction              #-}
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
        _Converting, _Okuri, _Page, _ConvNotFound,
        defCSelector, defPager, isIdling, toggleKana
        ) where
import Text.InputMethod.SKK.Dictionary
import Text.InputMethod.SKK.Misc

import           Control.Applicative   (Applicative, pure, (<$>))
import           Control.Arrow         (second, (***), (>>>))
import           Control.Effect        hiding (select, swap)
import           Control.Lens          (ix, makeLenses, makePrisms, makeWrapped)
import           Control.Lens          (to, traverse, use, uses, view, (%=))
import           Control.Lens          ((&), (.=), (<%=), (<<>=), (<>=), (<?=))
import           Control.Lens          ((?=), (^.), (^?), _2, _Just, _head)
import           Control.Lens          (anyOf)
import           Control.Lens          (each)
import           Control.Lens.Extras   (is)
import           Control.Monad         (unless)
import           Control.Zipper        ((:>>), Top, focus, fromWithin, leftmost)
import           Control.Zipper        (leftward, rightward, zipper)
import           Data.Attoparsec.Text  (parseOnly)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isAlpha, isAscii, isAsciiUpper, toLower)
import           Data.Data             (Data, Typeable)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (elemIndex, partition, unfoldr)
import           Data.Maybe            (fromJust, fromMaybe, isJust)
import           Data.Monoid           (Monoid (..), (<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Trie             hiding (lookup, null)
import qualified Data.Trie             as Trie
import           Data.Tuple            (swap)
import           FRP.Ordrea            (Event, SignalGen, eventToBehavior)
import           FRP.Ordrea            (externalE, mapAccumE, newExternalEvent)
import           FRP.Ordrea            (start, triggerExternalEvent)
import           Language.Haskell.TH   (litE, runIO, stringL)
import           Prelude               hiding (lookup)

data KanaEntry = KanaEntry { _hiraConv    :: T.Text
                           , _kataConv    :: T.Text
                           , _hanKataConv :: T.Text
                           , _nextState   :: Maybe Char
                           } deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''KanaEntry

data KanaResult = Converted T.Text | NoHit | InProgress BS.ByteString | Reset
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

data KanaState = KanaState { _tempResult :: Maybe T.Text
                           , _kanaBuf    :: BS.ByteString
                           , _dicts      :: [Trie KanaEntry]
                           } deriving (Show, Eq, Typeable)
makeLenses ''KanaState

newKanaState :: KanaTable -> KanaState
newKanaState table = KanaState { _tempResult = Nothing
                               , _kanaBuf = ""
                               , _dicts   = [table ^. kanaDic]
                               }

isConverting :: KanaState -> Bool
isConverting a = isJust (a ^. tempResult) || not (BS.null $ a ^. kanaBuf)

mapAccumE' :: (s -> a -> (s, b)) -> s -> Event a -> SignalGen (Event b)
mapAccumE' fun s0 e = mapAccumE s0 (flip fun <$> e)

romanConvE :: KanaTable -> ConvMode -> Event Char -> SignalGen (Event [KanaResult])
romanConvE dic mode input = mapAccumE' (romanConv dic mode) (newKanaState dic) (Just <$> input)

runSW :: Monoid w => s -> Effect (Writer w :+ State s :+ Nil) () -> (s, w)
runSW s0 act = swap $ runEffect $ runState s0 $ snd <$> runWriter act

emit :: EffectWriter [a] l => a -> Effect l ()
emit = tell . (:[])

romanConv :: KanaTable -> ConvMode -> KanaState -> Maybe Char -> (KanaState, [KanaResult])
romanConv dic mode s = runSW s . go
  where
    inProgress t = emit . InProgress =<< kanaBuf <<>= t
    reset = put $ newKanaState dic
    converted t = emit (Converted t) >> reset
    go Nothing = maybe (return ()) converted =<< use tempResult
    go (Just '\DEL') = do
      mt <- use kanaBuf
      if BS.null mt
        then reset >> emit NoHit
        else do
          buf <- kanaBuf <%= BS.init
          dicts %= tail
          emit $ InProgress buf
    go (Just c) = do
      let inp = BS.singleton c
      (mans, ps') <- uses dicts (lookupBy (,) inp . head)
      pre <- use tempResult
      let emitPre = maybe (return ()) converted pre
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
              tempResult ?= out
              dicts %= app ps'
              inProgress inp
        Nothing | Trie.null ps' -> do
                    emitPre >> reset
                    if Trie.null (submap inp $ dic ^. kanaDic)
                      then reset >> emit NoHit
                      else go (Just c)
                | otherwise     -> do
                    dicts      %= (ps' :)
                    tempResult .= Nothing
                    inProgress inp

prefixes :: BS.ByteString -> Trie a -> Trie a
prefixes = lookupBy ((snd .) . (,))

type Pager = forall a. [a] -> [[a]]
type CandidateSelector = forall a. Char -> [a] -> Maybe a

data SKKResult = Idle [KanaResult]
               | Converting T.Text
               | Okuri T.Text T.Text
               | Completed T.Text
               | Page [T.Text] (Maybe T.Text)
               | ConvNotFound T.Text (Maybe (Char, T.Text))
               deriving (Read, Show, Eq, Ord, Data, Typeable)

data SKKState = SKKState { _kanaState      :: Maybe KanaState
                         , _okuriState     :: Maybe (Char, T.Text)
                         , _convBuf        :: Maybe T.Text
                         , _selection      :: Maybe (Top :>> [[T.Text]] :>> [T.Text])
                         , _slashed        :: Bool
                         , _compCandidates :: Maybe (Top :>> [T.Text] :>> T.Text)
                         } deriving (Typeable)
makeLenses ''SKKState
makePrisms ''SKKResult

isIdling :: SKKResult -> Bool
isIdling r =
  or [anyOf (_Idle.each) (\a -> is _NoHit a || is _Converted a) r
     ,r & is _Completed, r & is _ConvNotFound]

newSKKState :: SKKState
newSKKState = SKKState Nothing Nothing Nothing Nothing False Nothing

okuriBuf :: Applicative f => (T.Text -> f T.Text) -> SKKState -> f SKKState
okuriBuf = okuriState . _Just . _2

hasOkuri = okuriState . to isJust
converting = convBuf . to isJust
selecting = selection . to isJust
kanaing = kanaState . to isJust

data View = NextPage (Top :>> [[T.Text]] :>> [T.Text])
          | PrevPage (Top :>> [[T.Text]] :>> [T.Text])
          | TakeFirst (Top :>> [[T.Text]] :>> [T.Text])
          | Select Char (Top :>> [[T.Text]] :>> [T.Text])
          | ConvertInput
          | Convert T.Text (Maybe (Char, T.Text))
          | OkuriInput
          | AndThen View View
          | NormalInput
          | StartConvert
          | StartOkuri
          | DeleteOkuri
          | DeleteConvert
          | Complete
          | StartSlash
          | SlashInput
          | Noop
          | DoCompletion
          | ToggleKana

viewS :: Char -> SKKState -> View
viewS c s
  | s ^. selecting =            -- In candidate selection...
    let curPage = fromJust (s ^. selection)
    in if c == ' '              -- Go next page
    then NextPage curPage
    else if c == '\DEL' || c == 'x' -- Go back page
    then PrevPage curPage
    else if c == '\n'           -- Take first candidate
    then TakeFirst curPage
    else if isAsciiUpper c && length (curPage ^. focus) == 1 && not (s ^. kanaing)
    then TakeFirst curPage `AndThen` StartConvert `AndThen` ConvertInput
    else if length (curPage ^. focus) == 1 && c == '/'
    then TakeFirst curPage `AndThen` StartSlash
    else if length (curPage ^. focus) == 1
    then TakeFirst curPage `AndThen` NormalInput
    else Select c curPage       -- Select candidate
  | c == 'Q' && not (s ^. converting) = StartConvert
  | c == '/' && not (s ^. converting) && not (maybe False isConverting $ s ^. kanaState) = StartSlash
  | c == '\n' && s ^. converting = Complete
  | c == '\DEL' && s ^. converting =
      if s ^. hasOkuri
      then DeleteOkuri
      else DeleteConvert
  | isAsciiUpper c && not (s ^. converting) = ConvertInput -- Start new conversion phase
  | c == ' ' && s ^. converting        = Convert (s ^. convBuf._Just) (s ^. okuriState)
                                         -- Convert inputted somethings
  | c == 'q' && s ^. converting && not (s ^. hasOkuri) = ToggleKana
  | isAsciiUpper c && s ^. converting && not (s ^. hasOkuri) &&
    (s ^. convBuf)  /= Just "" &&
    not (s ^. selecting) &&  not (s ^. slashed)  = StartOkuri `AndThen` OkuriInput
  | s ^. converting && s ^. slashed = SlashInput
  | s ^. converting && not (s ^. hasOkuri) && c == '\t' = DoCompletion
  | s ^. converting && not (s ^. hasOkuri) = ConvertInput
  | s ^. converting && s ^. hasOkuri = OkuriInput
  | c == '\DEL' && not (s ^. kanaing) = Noop
  | otherwise = NormalInput

nextCompletion :: (a :>> b) -> (a :>> b)
nextCompletion s = fromMaybe  (s & leftmost) (s & rightward)

data SKKCommand = Incoming Char
                | ToggleHankaku
                  deriving (Read, Show, Eq, Ord)

skkConv :: KanaTable -> ConvMode -> Dictionary
        -> Pager -> CandidateSelector
        -> SKKState -> SKKCommand -> (SKKState, [SKKResult])
skkConv _ Ascii _ _ _ s ToggleHankaku = (s, [Idle []])
skkConv _ _     _ _ _ s ToggleHankaku
  | s ^. converting = runSW s $ do
    mbuf <- use convBuf
    case mbuf of
      Just buf -> do
        lo <- use $ kanaState._Just.tempResult
        put newSKKState
        emit $ Completed $ toHankaku $ buf <> fromMaybe "" lo
      Nothing -> emit $ Idle []
skkConv _ _     _ _ _ s ToggleHankaku = (s, [Idle []])
skkConv _ Ascii _ _ _ s (Incoming c) = (s, [Idle [Converted $ T.singleton c]])
skkConv table kana dic pager select s0 (Incoming c) = runSW s0 (go (viewS c s0))
  where
    showPage page = do
      mokBuf <- use okuriState
      emit $ Page page (convKana Hiragana kana . snd <$> mokBuf)
    notFound body mokuri = do
      put newSKKState
      emit $ ConvNotFound body mokuri
    complete str = do
      a <- use okuriState
      put newSKKState
      emit $ Completed $ convKana Hiragana kana $ str <> maybe "" snd a
    okuriWith temp = do
      body  <- use convBuf
      okBuf <- use okuriBuf
      emit $ Okuri (convKana Hiragana kana $ fromMaybe "" body) (convKana Hiragana kana $ okBuf <> temp)
    convertingWith temp = do
      buf <- use convBuf
      emit $ Converting $ convKana Hiragana kana $ (fromMaybe "" buf) <> temp
    go Complete = do
      buf <- fromMaybe "" <$> use convBuf
      complete buf
    go DoCompletion = do
      trail <- either fst id <$> toKana Nothing
      kanaState .= Nothing
      convBuf <>= Just trail
      mcomps <- use compCandidates
      case mcomps of
        Nothing -> do
          buf <- fromMaybe "" <$> use convBuf
          unless (T.null buf) $ do
            let cs = filter (buf `T.isPrefixOf`) $
                     HM.keys $ dic ^. okuriNasiDic
            unless (null cs)$ do
              compCandidates ?= (zipper cs & fromWithin traverse)
              convBuf ?= head cs
              convertingWith ""
        Just cur -> do
          cur' <- compCandidates <?= (cur & nextCompletion)
          convBuf ?= cur' ^. focus
          convertingWith ""
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
    go ToggleKana = do
      lo <- either fst id <$> toKana Nothing
      buf <- fromMaybe "" <$> use convBuf
      complete $ toggleKana kana (buf <> lo)
    go SlashInput = do
      convBuf <>= Just (T.singleton c)
      convertingWith ""
    go StartSlash = do
      slashed .= True
      convBuf ?= ""
      convertingWith ""
    go (AndThen p q) = go p >> go q
    go Noop = emit $ Idle [NoHit]
    go DeleteConvert = do
      compCandidates .= Nothing
      buf <- fromMaybe "" <$> use convBuf
      isKana <- uses kanaState (maybe False isConverting)
      if isKana
        then toKana (Just '\DEL') >>= \case
          Right comp -> do
            convBuf <>= Just comp
            convertingWith ""
          Left (comp, temp) -> do
            convBuf <>= Just comp
            convertingWith temp
        else if T.null buf
             then do
               convBuf .= Nothing
               put newSKKState
               emit $ Idle [NoHit]
             else do
               convBuf %= fmap T.init
               convertingWith ""
    go DeleteOkuri = do
      buf <- use okuriBuf
      kdic <- use kanaState
      case kdic of
        Just _
          | not (T.null buf) -> go OkuriInput
        _ ->
          if T.null buf
            then do
              okuriState .= Nothing
              kanaState  .= Nothing
              convertingWith ""
            else do
              okuriState %= fmap (second T.init)
              okuriWith ""
    go (NextPage curPage) =
      case curPage & rightward of
        Nothing -> showPage $ curPage ^. focus
        Just pg -> do
           selection ?= pg
           showPage $ pg ^. focus
    go (PrevPage curPage) =
      case curPage & leftward of
        Nothing -> do
          selection .= Nothing
          okuriState .= Nothing
          convertingWith ""
        Just pg -> do
          selection ?= pg
          showPage $ pg ^. focus
    go (TakeFirst curPage) = do
      complete $ curPage ^. focus._head
    go (Select n curPage) =
      case select n $ curPage ^. focus of
        Nothing -> showPage $ curPage ^. focus
        Just t  -> complete t
    go ConvertInput = do
      compCandidates .= Nothing
      rslt <- toKana $ Just (toLower c)
      case rslt of
        Right ans -> do
          convBuf <>= Just ans
          convertingWith ""
        Left (comp, temp) -> do
          convBuf <>= Just comp
          convertingWith temp
    go (Convert body0 mok) = do
      elo <- toKana Nothing
      let lo = either fst id elo
      case mok of
        Nothing ->
          let body = body0 <> lo
              mcss = pager . map (view tango) .
                     filter (is _Candidate) <$>
                     lookup (Input body Nothing) dic
          in case mcss of
            Nothing -> notFound body Nothing
            Just css -> do
              selection ?= (zipper css & fromWithin traverse)
              showPage $ head css
        Just (okCh, okBuf0) ->
          let body = body0
              okBuf = okBuf0 <> lo
          in case pager <$> lookup' (Input body $ Just $ toLower okCh) okBuf dic of
            Just css -> do
              okuriState ?= (okCh, okBuf)
              selection  ?= (zipper css & fromWithin traverse)
              showPage $ head css
            Nothing -> do
              okuriState ?= (okCh, okBuf)
              notFound body (Just (okCh, okBuf))
    go OkuriInput = do
      body <- use $ convBuf._Just
      okr  <- use okuriState
      rslt <- toKana $ Just $ toLower c
      case rslt of
        Left (comp, temp) -> do
          let (okCh, okBuf) = maybe (c, comp) (second (<> comp)) okr
          okuriState ?= (okCh, okBuf)
          okuriWith temp
        Right str -> do
          let ok = maybe (c, str) (second (<> str)) okr
          go $ Convert body $ Just ok
    go NormalInput = do
      ks0 <- fromMaybe (newKanaState table) <$> use kanaState
      let (ks', rs) = romanConv table kana ks0 (Just c)
      kanaState ?= ks'
      emit $ Idle rs
    toKana mc = do
      ks <- fromMaybe (newKanaState table) <$> use kanaState
      let (ks', rs) = romanConv table Hiragana ks mc
          (prgs, cvd) = partition (is _InProgress) rs
          finished = T.concat $ map toT cvd
          toT NoHit          = T.singleton c
          toT Reset          = ""
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
defSKKConvE = skkConvE defKanaTable Hiragana sDictionary pager sel
  where
    pager = splitAt 4 >>> map pure *** slice 7 >>> uncurry (++)
    sel c cs = (cs ^?) . ix =<< elemIndex c "asdfjkl"

defPager :: Pager
defPager = splitAt 4 >>> map pure *** slice 7 >>> uncurry (++)

defCSelector :: CandidateSelector
defCSelector c cs = (cs ^?) . ix =<< elemIndex c "asdfjkl"

slice :: Int -> [a] -> [[a]]
slice n = unfoldr phi
  where
    phi [] = Nothing
    phi ys = Just $ splitAt n ys

skkConvE :: KanaTable -> ConvMode -> Dictionary
         -> Pager -> CandidateSelector
         -> Event SKKCommand -> SignalGen (Event [SKKResult])
skkConvE table mode dic pag sel input
  = mapAccumE' (skkConv table mode dic pag sel) newSKKState input

newInput :: (Event Char -> SignalGen (Event a)) -> IO (Char -> IO a)
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

