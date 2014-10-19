{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell                  #-}
module Text.InputMethod.SKK.Dictionary
       (Dictionary(..), Input(..), Candidate(..),
        gokan, okurigana, emptyDic, lookup,
        tango, chushaku, dictionary, mergeDic,
        sDictionary, okuriAriDic, okuriNasiDic, insert, unregister,
        formatDictionary, _Candidate, formatCandidate)
       where
import           Control.Applicative  ((*>), (<$), (<$>), (<*), (<*>), (<|>))
import           Control.Arrow        ((***))
import           Control.Lens
import           Control.Monad        (guard)
import           Data.Attoparsec.Text
import           Data.Char            (isSpace)
import           Data.Data            (Data, Typeable)
import           Data.Either          (partitionEithers)
import           Data.Hashable
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import           Data.List            (nub, sortBy)
import           Data.List            (partition)
import           Data.List            (find)
import qualified Data.List            as L
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Ord             (comparing)
import           Data.String          (IsString (..))
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Language.Haskell.TH  (litE, runIO, stringL)
import           Prelude              hiding (lookup, takeWhile)

data Input = Input { _gokan     :: T.Text
                   , _okurigana :: Maybe (Char, Maybe T.Text)
                   } deriving (Read, Show, Eq, Ord,
                               Data, Typeable, Generic)
data Candidate = Candidate { _tango    :: T.Text
                           , _chushaku :: T.Text
                           }
               deriving (Read, Show, Eq, Ord,
                         Data, Typeable, Generic)
instance Hashable Input
instance IsString Candidate where
  fromString = flip Candidate "" . T.pack

type Conditional = (T.Text, [Candidate])
type Conditionals = HashMap T.Text [Candidate]

data GuardedCandidates
   = GuardedCandidates { _candidates   :: [Candidate]
                       , _conditionals :: Conditionals
                       } deriving (Show, Eq, Typeable, Data)

type AriDic = HashMap (T.Text, Char) GuardedCandidates
type NasiDic = HashMap T.Text [Candidate]


data Dictionary = Dict { _okuriAriDic  :: AriDic
                       , _okuriNasiDic :: NasiDic
                       }
                   deriving (Show, Eq, Data, Typeable)

emptyDic :: Dictionary
emptyDic = Dict HM.empty HM.empty

instance Hashable Candidate

makeLenses ''Input

makeLenses ''Dictionary
makeLenses ''GuardedCandidates

makeLenses ''Candidate
makePrisms ''Candidate

data Okuri = Ari | Nasi
           deriving (Read, Show, Eq, Ord)

mergeDic :: Dictionary -> Dictionary -> Dictionary
mergeDic d d' =
  d' & okuriNasiDic %~ HM.unionWith unionCands (d ^. okuriNasiDic)
     & okuriAriDic  %~ HM.unionWith mergeGCands (d ^. okuriAriDic)

lookup :: Input -> Dictionary -> Maybe [Candidate]
lookup (Input g Nothing) (Dict _ noDic) = HM.lookup g noDic
lookup (Input g (Just (o, Nothing))) (Dict oDic _)
  = view  candidates <$> HM.lookup (g, o) oDic
lookup (Input g (Just (o, Just txt))) (Dict oDic _)
  = HM.lookup (g, o) oDic <&> \d ->
      let biased = fromMaybe [] $ HM.lookup txt (d ^. conditionals)
          css0   = d ^. candidates
      in unionCands biased css0

unregister :: Input -> T.Text -> Dictionary -> Dictionary
unregister (Input g Nothing) txt =
  okuriNasiDic . at g %~ maybe Nothing (ensureNonEmpty . filter ((/= txt) . view tango))
unregister (Input g (Just (ok, mokb))) txt =
  okuriAriDic . ix (g, ok) %~ annihilate txt mokb

ensureNonEmpty :: [t] -> Maybe [t]
ensureNonEmpty [] = Nothing
ensureNonEmpty xs = Just xs

annihilate :: T.Text -> Maybe T.Text -> GuardedCandidates -> GuardedCandidates
annihilate txt Nothing gcs =
  gcs & candidates %~ L.filter ((/= txt) . view tango)
      & conditionals . each %~ L.filter ((/= txt) . view tango)
annihilate txt (Just ok) gcs =
  gcs & conditionals . ix ok %~ L.filter ((/= txt) . view tango)

insert :: Input -> Candidate -> Dictionary -> Dictionary
insert (Input k Nothing) v d =
  d & okuriNasiDic %~  HM.insertWith unionCands k [v]
insert (Input k (Just (o, Nothing))) v d =
  d & okuriAriDic %~ HM.insertWith mergeGCands (k, o)
                     (GuardedCandidates [v] HM.empty)
insert (Input k (Just (o, Just t))) v d =
  d & okuriAriDic %~ HM.insertWith mergeGCands (k, o)
                     (GuardedCandidates [v] $ HM.singleton t [v])

formatInput :: Input -> T.Text
formatInput (Input g mo) = g <> maybe "" T.singleton (fst <$> mo)

formatCandidate :: Candidate -> T.Text
formatCandidate (Candidate wd "") = wd
formatCandidate (Candidate wd n)
  = wd <> ";" <> n

formatCands :: [Candidate] -> T.Text
formatCands = flip T.snoc '/' . T.cons '/' . T.intercalate "/" . map formatCandidate

formatDictionary :: Dictionary -> T.Text
formatDictionary dic =
  T.unlines $
  ";; okuri-ari entries." :
  [ formatInput (Input inp (Just (o, Nothing))) <> " " <> formatGCands cands
  | ((inp, o), cands) <- sortBy (comparing fst) $ HM.toList $ dic ^. okuriAriDic ]
  ++ "":
  ";; okuri-nasi entries." :
  [ formatInput (Input inp Nothing) <> " " <> formatCands cands
  | (inp, cands) <- sortBy (comparing fst) $ HM.toList $ dic ^. okuriNasiDic ]

formatGCands :: GuardedCandidates -> T.Text
formatGCands (GuardedCandidates cands gd)
  | HM.null gd = formatCands cands
  | otherwise  = formatCands cands <> T.intercalate "/" (map formatConditional $ HM.toList gd) <> "/"

formatConditional :: Conditional -> T.Text
formatConditional (ok, subs) = T.concat ["[", ok, formatCands subs, "]"]

comment :: Parser ()
comment = do
  cmt <- char ';' *> manyTill anyChar endOfLine
  guard $ T.pack (';':cmt) `notElem` [okuriAriStr, okuriNasiStr]
  return ()

line :: Parser ()
line = endOfLine <|> comment

nasiMidashi :: Parser T.Text
nasiMidashi = takeWhile1 (not . isSpace)

ariMidashi :: Parser (T.Text, Char)
ariMidashi =  do
  mid <- takeWhile1 (not . isSpace)
  return (T.init mid, T.last mid)

nasiEntry :: Parser (T.Text, [Candidate])
nasiEntry = (,) <$> nasiMidashi <*  skipSpace <*> candidatesP

ariEntry:: Parser ((T.Text, Char), GuardedCandidates)
ariEntry  = (,) <$> ariMidashi <*  skipSpace <*> gCands

gCands :: Parser GuardedCandidates
gCands =
  uncurry GuardedCandidates . (nub *** HM.fromList) . partitionEithers
  <$> slashedP (eitherP candidate (try conditional))

candidatesP :: Parser [Candidate]
candidatesP = nub <$> slashedP candidate

converted :: Parser T.Text
converted = do
  mans <- peekChar
  guard $ maybe True (`notElem` "[]") mans
  takeWhile1 (`notElem` "\r\n/;")

note :: Parser T.Text
note = takeWhile (`notElem` "\r\n/")

slashedP :: Parser a -> Parser [a]
slashedP p = char '/' *> p `sepBy1` char '/' <* char '/'

candidate :: Parser Candidate
candidate = simpleCandidate

conditional :: Parser Conditional
conditional = (,) <$  char '['
                  <*> takeWhile1 (`notElem` ";/\r\n")
                  <*> (nub <$> slashedP simpleCandidate)
                  <*  char ']'

simpleCandidate :: Parser Candidate
simpleCandidate = do { b <- peekChar
                     ; case b of
                       Just '/' -> return $ Candidate "" ""
                       _ -> fail "not empty"
                     }
              <|> Candidate <$> converted <*> option "" (char ';' *> note)

okuriPragma :: Parser Okuri
okuriPragma = try okuriAriPragma
          <|>     okuriNasiPragma

okuriAriStr :: T.Text
okuriAriStr = ";; okuri-ari entries."

okuriNasiStr :: T.Text
okuriNasiStr = ";; okuri-nasi entries."

okuriAriPragma :: Parser Okuri
okuriAriPragma = Ari <$ string okuriAriStr <* endOfLine

okuriNasiPragma :: Parser Okuri
okuriNasiPragma = Nasi <$ string okuriNasiStr <* endOfLine

okuriDic :: Parser (Either [(T.Text, [Candidate])] [((T.Text, Char), GuardedCandidates)])
okuriDic = do
  okr <- okuriPragma
  skipMany line
  case okr of
    Ari  -> Right <$> ariEntry  `sepBy` skipMany line
    Nasi -> Left  <$> nasiEntry `sepBy` skipMany line

dictionary :: Parser Dictionary
dictionary = do
  skipMany line
  dics <- okuriDic `sepBy` skipMany line
  let nds = [ (top, cands)
            | Left d <- dics
            , (top, cands) <- d]
      ods = [ ((top, ok), cands)
            | Right d <- dics
            , ((top, ok), cands) <- d]
      dic = Dict (HM.fromListWith mergeGCands ods) (HM.fromListWith unionCands nds)
  skipMany line
  endOfInput
  return dic

mergeGCands :: GuardedCandidates -> GuardedCandidates -> GuardedCandidates
mergeGCands a b = b & candidates   %~ unionCands (a ^. candidates)
                    & conditionals %~ HM.unionWith unionCands (a ^. conditionals)

unionCands :: [Candidate] -> [Candidate] -> [Candidate]
unionCands cs ds = foldr insertCand ds cs

insertCand :: Candidate -> [Candidate] -> [Candidate]
insertCand c cs =
  let (targs, rest) = partition ((== (c ^. tango)) . view tango) cs
      mchu = find (not . T.null) $ map (view chushaku) targs
      chu  | T.null (c ^. chushaku) = fromMaybe "" mchu
           | otherwise = c ^. chushaku
  in (c & chushaku .~ chu) : rest

sDictionary :: Dictionary
Right sDictionary = parseOnly dictionary $(litE . stringL . tail =<< runIO (readFile "data/SKK-JISYO.S"))
