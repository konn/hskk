{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs                #-}
{-# LANGUAGE LiberalTypeSynonyms, MultiParamTypeClasses                  #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, TemplateHaskell, TypeFamilies  #-}
{-# LANGUAGE TypeOperators, ViewPatterns                                 #-}
module Text.InputMethod.SKK
       (module Text.InputMethod.SKK.Dictionary,
        toInput, parseDictionary,
        -- * Converters
        romanConv, romanConvE, defKanaTable, convertTest,
        -- * Data-types and lenses
        KanaEntry(..), ConvMode(..), KanaResult(..),
        _Converted, _NoHit, _InProgress, newInput,
        hiraConv, kataConv, hanKataConv, nextState,
        parseKanaTable, formatKanaTable, skkConv, skkConvE,
        KanaTable(..), kanaDic, SKKResult(..), defSKKConvE,
        -- * misc
        Pager, CandidateSelector, slice,
        ) where
import Text.InputMethod.SKK.Dictionary
import Text.InputMethod.SKK.Misc

import           Control.Applicative   ((<$>))
import           Control.Applicative   (pure)
import           Control.Applicative   (Applicative)
import           Control.Applicative   ((<*>))
import           Control.Arrow         (second)
import           Control.Arrow         ((>>>))
import           Control.Arrow         ((***))
import           Control.Lens          (makeLenses, makePrisms)
import           Control.Lens          (makeWrapped, to, view, (%~), _1, _2)
import           Control.Lens          ((&), (.~), (^.), (^?), _head)
import           Control.Lens          ((%=), _Just)
import           Control.Lens          (traverse)
import           Control.Lens          (ix)
import           Control.Lens          ((?=))
import           Control.Lens          ((<>=))
import           Control.Lens          ((.=))
import           Control.Lens          (use)
import           Control.Lens.Extras   (is)
import           Control.Monad.State
import           Control.Zipper
import           Data.Attoparsec.Text  (parseOnly)
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isAlpha, isAscii)
import           Data.Char             (isUpper)
import           Data.Char             (toLower)
import           Data.Data             (Data, Typeable)
import           Data.List             (partition)
import           Data.List             (elemIndex)
import           Data.List             (unfoldr)
import           Data.Maybe            (isJust)
import           Data.Maybe            (fromJust)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (Monoid (..), (<>))
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Trie             hiding (lookup, null)
import qualified Data.Trie             as Trie
import           Data.Tuple            (swap)
import           FRP.Ordrea            (Event, SignalGen)
import           FRP.Ordrea            (eventFromList, newExternalEvent,
                                        triggerExternalEvent)
import           FRP.Ordrea            (eventToBehavior, externalE, justE,
                                        mapAccumE, networkToList, start)
import           Language.Haskell.TH   (litE, runIO, stringL)
import           Prelude               hiding (lookup)

data KanaEntry = KanaEntry { _hiraConv    :: T.Text
                           , _kataConv    :: T.Text
                           , _hanKataConv :: T.Text
                           , _nextState   :: Maybe Char
                           } deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''KanaEntry

data KanaResult = Converted T.Text | NoHit | InProgress BS.ByteString | Deleted
               deriving (Read, Show, Eq, Ord, Data, Typeable)

makeLenses ''KanaResult
makePrisms ''KanaResult

instance Monoid KanaResult where
  mempty = NoHit
  mappend NoHit a = a
  mappend a NoHit = a
  mappend c@Converted{} Converted{}  = c
  mappend c@Converted{} InProgress{} = c
  mappend InProgress{} c@Converted{} = c
  mappend (InProgress c) (InProgress d) = InProgress (c <> d)
  mappend Deleted a = a
  mappend a Deleted = a

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

convChar :: Functor f => ConvMode
         -> (T.Text -> f T.Text) -> KanaEntry -> f KanaEntry
convChar Hiragana = hiraConv
convChar Katakana = kataConv
convChar HankakuKatakana = hanKataConv

data KanaState = KanaState { _leftover :: T.Text
                           , _previous :: [KanaResult]
                           , _dicts    :: [Trie KanaEntry]
                           } deriving (Show, Eq, Typeable)
makeLenses ''KanaState

newKanaState :: KanaTable -> KanaState
newKanaState = KanaState "" [] . (:[]) . view kanaDic

mapAccumE' :: (s -> a -> (s, b)) -> s -> Event a -> SignalGen (Event b)
mapAccumE' fun s0 e = mapAccumE s0 (flip fun <$> e)

romanConvE :: KanaTable -> ConvMode -> Event Char -> SignalGen (Event [KanaResult])
romanConvE dic mode input = mapAccumE' (romanConv dic mode) (newKanaState dic) (Just <$> input)

romanConv :: KanaTable -> ConvMode -> KanaState -> Maybe Char -> (KanaState, [KanaResult])
romanConv table _ s Nothing
  | not (T.null $ s ^. leftover) = mkKanaState "" [table ^. kanaDic] [Converted $ s^. leftover]
  | otherwise = (s, s ^. previous)
romanConv _ _ s (Just '\DEL')
  | [_] <- s ^. dicts = (s & previous .~ [NoHit], [NoHit])
  | otherwise         = (s & dicts %~ tail & previous .~ [Deleted], [Deleted])
romanConv d0@(KanaTable dic) mode s (BS.singleton . fromJust -> inp) =
  let (mans, ps') = lookupBy (,) inp $ s ^. dicts. to head
      pre = s ^. leftover
      addPre | T.null pre = id
             | otherwise  = (Converted pre :)
  in case mans of
    Just a ->
      let out   = a ^. convChar mode
          app d = maybe (d :) (:) $ flip prefixes d . BS.singleton <$> a ^. nextState
      in if Trie.null ps'
         then mkKanaState "" (app dic []) [Converted out]
         else (s & leftover .~ out & dicts %~ app ps' & previous .~ [InProgress inp],
               [InProgress inp])
    Nothing | Trie.null ps' ->
                if Trie.null (submap inp dic)
                then mkKanaState "" [dic] $ addPre [NoHit]
                else (previous %~ addPre) *** addPre $
                     romanConv d0 mode (newKanaState $ KanaTable dic) $
                     Just $ BS.head inp
            | otherwise     -> (s & leftover .~ "" & dicts %~ (ps':) & previous .~ [InProgress inp],
                                [InProgress inp])

mkKanaState :: T.Text -> [Trie KanaEntry] -> [KanaResult] -> (KanaState, [KanaResult])
mkKanaState str hoge out = (KanaState str out hoge, out)

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

data SKKState = SKKState { _kanaState  :: Maybe KanaState
                         , _okuriState :: Maybe (Char, T.Text)
                         , _convBuf    :: Maybe T.Text
                         , _selection  :: Maybe (Top :>> [[T.Text]] :>> [T.Text])
                         , _slashed    :: Bool
                         } deriving (Typeable)
makeLenses ''SKKState

newSKKState :: SKKState
newSKKState = SKKState Nothing Nothing Nothing Nothing False

okuriBuf :: Applicative f => (T.Text -> f T.Text) -> SKKState -> f SKKState
okuriBuf = okuriState . _Just . _2

okuriChar :: Applicative f => (Char -> f Char) -> SKKState -> f SKKState
okuriChar = okuriState . _Just . _1

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
    else if isUpper c && length (curPage ^. focus) == 1 && not (s ^. kanaing)
    then TakeFirst curPage `AndThen` ConvertInput
    else if length (curPage ^. focus) == 1
    then TakeFirst curPage `AndThen` NormalInput
    else Select c curPage       -- Select candidate
  | c == '/' && not (s ^. converting) = StartSlash
  | c == '\n' && s ^. converting = Complete
  | c == '\DEL' && s ^. converting =
      if s ^. hasOkuri
      then DeleteOkuri
      else DeleteConvert
  | isUpper c && not (s ^. converting) = ConvertInput -- Start new conversion phase
  | c == ' ' && s ^. converting        = Convert (s ^. convBuf._Just) (s ^. okuriState)
                                         -- Convert inputted somethings
  | c == 'q' && s ^. converting && not (s ^. hasOkuri) = ToggleKana
  | isUpper c && s ^. converting && not (s ^. hasOkuri) &&
    (s ^. convBuf)  /= Just "" &&
    not (s ^. selecting) &&  not (s ^. slashed)  = StartOkuri `AndThen` OkuriInput
  | s ^. converting && s ^. slashed = SlashInput
  | s ^. converting && not (s ^. hasOkuri) = ConvertInput
  | s ^. converting && s ^. hasOkuri = OkuriInput
  | otherwise = NormalInput

(<||>) :: Monoid t1 => Maybe (t, t1) -> Maybe (t, t1) -> Maybe (t, t1)
Just (c, str) <||> Just (_, str') = Just (c, str <> str')
Nothing <||> a = a
a <||> Nothing = a

skkConv :: KanaTable -> ConvMode -> Dictionary
        -> Pager -> CandidateSelector
        -> SKKState -> Char -> (SKKState, [SKKResult])
skkConv table kana dic pager select s0 c = swap $ runState (go (viewS c s0)) s0
  where
    showPage str = do
      mokBuf <- use okuriState
      return [Page str (snd <$> mokBuf)]
    notFound body mokuri = do
      put newSKKState
      return [ConvNotFound body mokuri]
    complete str = do
      a <- use okuriState
      put newSKKState
      return [Completed $ str <> maybe "" snd a]
    okuriWith temp = do
      body  <- use convBuf
      okBuf <- use okuriBuf
      return [Okuri (fromMaybe "" body) (okBuf <> temp)]
    convertingWith temp = do
      buf <- use convBuf
      return [Converting $ (fromMaybe "" buf) <> temp]
    go Complete = do
      buf <- fromMaybe "" <$> use convBuf
      complete buf
    go StartConvert = do
      mans <- toKana Nothing
      case mans of
        Right a | not (T.null a) -> return [ Idle [ Converted a ] ]
        _ -> return []
    go StartOkuri = do
      mans <- toKana Nothing
      case mans of
        Right a | not (T.null a) -> do
          convBuf <>= Just a
          return []
        Left (comp, lo) | not (T.null lo) -> do
          convBuf <>= Just comp
          okuriState ?= (T.head lo, "")
          return []
        _ -> return []
    go ToggleKana = do
      buf <- fromMaybe "" <$> use convBuf
      complete $ toggleKana kana buf
    go SlashInput = do
      convBuf <>= Just (T.singleton c)
      convertingWith ""
    go StartSlash = do
      slashed .= True
      convBuf ?= ""
      convertingWith ""
    go (AndThen p q) = (++) <$> go p <*> go q
    go Noop = return [Idle [NoHit]]
    go DeleteConvert = do
      buf <- fromMaybe "" <$> use convBuf
      kdic <- use kanaState
      case kdic of
        Just _
          | not (T.null buf) -> go ConvertInput
          | otherwise -> do
            convBuf ?= ""
            convertingWith ""
        Nothing ->
          if T.null buf
            then do
              convBuf .= Nothing
              put newSKKState
              return [Idle [NoHit]]
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
      return [Idle rs]
    toKana mc = do
      ks <- fromMaybe (newKanaState table) <$> use kanaState
      let (ks', rs) = romanConv table kana ks mc
          (prgs, cvd) = partition (is _InProgress) rs
          finished = T.concat $ map toT cvd
          toT NoHit          = T.singleton c
          toT Deleted        = ""
          toT (InProgress _) = T.singleton c
          toT (Converted t)  = t
      if null prgs && all (\k -> k `notElem` [Deleted]) cvd
        then do
          kanaState .= Nothing
          return $ Right finished
        else do
          kanaState ?= ks'
          return $ Left (finished, T.decodeUtf8 (BS.concat $ map (view _InProgress) prgs))

defSKKConvE :: Event Char -> SignalGen (Event [SKKResult])
defSKKConvE = skkConvE defKanaTable Hiragana sDictionary pager sel
  where
    pager = splitAt 4 >>> map pure *** slice 7 >>> uncurry (++)
    sel c cs = (cs ^?) . ix =<< elemIndex c "asdfjkl"

slice :: Int -> [a] -> [[a]]
slice n = unfoldr phi
  where
    phi [] = Nothing
    phi ys = Just $ splitAt n ys

skkConvE :: KanaTable -> ConvMode -> Dictionary
         -> Pager -> CandidateSelector
         -> Event Char -> SignalGen (Event [SKKResult])
skkConvE table mode dic pag sel input
  = mapAccumE' (skkConv table mode dic pag sel) newSKKState input

convertTest :: ConvMode -> String -> IO T.Text
convertTest mode inp
  = liftM (T.concat . head) . networkToList (length inp) $
    eventToBehavior <$> inputText mode inp

inputText :: ConvMode -> String -> SignalGen (Event T.Text)
inputText mode inp = do
  inps <- eventFromList [inp]
  ev <- romanConvE defKanaTable mode inps
  return $ justE $ (^? _Converted) . mconcat <$> ev

newInput :: (Event Char -> SignalGen (Event a)) -> IO (Char -> IO a)
newInput toEv = do
  eev <- newExternalEvent
  step <- start $ do
    input <- externalE eev
    fmap head . eventToBehavior <$> toEv input
  return $ \inp -> triggerExternalEvent eev inp >> step

defKanaTable :: KanaTable
defKanaTable = parseKanaTable $(litE . stringL =<< runIO (readFile "data/kana-rule.conf"))

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

{-
prettyKanaTable :: KanaTable -> T.Text
prettyKanaTable =
  T.unlines . (++ ["]"]) . (_tail.each %~ (T.cons ',')) .
  (_head  %~ (T.cons '[')) . map prettyEntry . toList . view kanaDic


prettyEntry :: Show a => (a, KanaEntry) -> T.Text
prettyEntry (mid, KanaEntry a b c d) =
  T.concat ["(", show' mid, ", KanaEntry ", T.unwords $ map prettyText [a, b, c], " $ ", show' d, ")"]
-}

prettyText :: T.Text -> T.Text
prettyText str = "\"" <> str <> "\""

prettyState :: KanaResult -> String
prettyState (Converted a) = T.unpack $ "Converted " <> prettyText a
prettyState w             = show w
