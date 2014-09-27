{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell                  #-}
module Text.InputMethod.SKK.Dictionary
       (Dictionary(..), Input(..), Candidate(..),
        gokan, okurigana, emptyDic, lookup, lookup',
        tango, chushaku, okuri, subCands, dictionary,
        sDictionary, okuriAriDic, okuriNasiDic, insert,
        formatDictionary, _Candidate, _OkuriSub, formatCandidate) where
import           Control.Applicative  ((*>), (<$), (<$>), (<*), (<*>), (<|>))
import           Control.Lens
import           Control.Lens.Extras  (is)
import           Control.Monad        (guard)
import           Data.Attoparsec.Text
import           Data.Char            (isAlpha, isAscii, isLower, isSpace)
import           Data.Data            (Data, Typeable)
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import           Data.List            (delete, sortBy)
import           Data.List            (partition)
import           Data.List            (find)
import           Data.List            (nub)
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          ((<>))
import           Data.Ord             (comparing)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)
import           Language.Haskell.TH
import           Prelude              hiding (lookup, takeWhile)

data Input = Input { _gokan     :: T.Text
                   , _okurigana :: Maybe Char
                   } deriving (Read, Show, Eq, Ord,
                               Data, Typeable, Generic)
data Candidate = Candidate { _tango    :: T.Text
                           , _chushaku :: T.Text
                           }
               | OkuriSub { _okuri    :: T.Text
                          , _subCands :: [Candidate]}
               deriving (Read, Show, Eq, Ord,
                         Data, Typeable, Generic)
instance Hashable Input

type AriDic = HM.HashMap (T.Text, Char) [Candidate]
type NasiDic = HM.HashMap T.Text [Candidate]

data Dictionary = Dict { _okuriAriDic  :: AriDic
                       , _okuriNasiDic :: NasiDic
                       }
                   deriving (Show, Eq, Data, Typeable)

emptyDic :: Dictionary
emptyDic = Dict HM.empty HM.empty

instance Hashable Candidate

makeLenses ''Input

makeLenses ''Dictionary

makeLenses ''Candidate
makePrisms ''Candidate

data Okuri = Ari | Nasi
           deriving (Read, Show, Eq, Ord)

lookup :: Input -> Dictionary -> Maybe [Candidate]
lookup (Input g Nothing) (Dict _ noDic) = HM.lookup g noDic
lookup (Input g (Just o)) (Dict oDic _) = HM.lookup (g, o) oDic

lookup' :: Input -> T.Text -> Dictionary -> Maybe [T.Text]
lookup' i@(Input _ Nothing) _ d
  = map (view tango) . filter (is _Candidate) <$> lookup i d
lookup' i@(Input _ Just{}) t d =
  case lookup i d of
    Nothing -> Nothing
    Just cands ->
      let (subs, ords) = partition (is _OkuriSub) cands
          mtargs = filter (is _Candidate) . view subCands <$>
                   find ((== t) . view okuri) subs
          targs = fromMaybe [] mtargs
          ans = nub $ map (view tango) $ targs ++ ords
      in if null ans then Nothing else Just ans

insert :: Input -> Candidate -> Dictionary -> Dictionary
insert (Input k Nothing) v d =
  d & okuriNasiDic %~  HM.insertWith (\ [a] b -> a : delete a b) k [v]
insert (Input k (Just o)) v d =
  d & okuriAriDic %~ HM.insertWith (\ [a] b -> a : delete a b) (k, o) [v]

formatInput :: Input -> T.Text
formatInput (Input g mo) = g <> maybe "" T.singleton mo

formatCandidate :: Candidate -> T.Text
formatCandidate (OkuriSub ok subs) =
  T.concat ["[", ok, formatCands subs, "]"]
formatCandidate (Candidate wd "") = wd
formatCandidate (Candidate wd n)
  = wd <> ";" <> n

formatCands :: [Candidate] -> T.Text
formatCands = flip T.snoc '/' . T.cons '/' . T.intercalate "/" . map formatCandidate

formatDictionary :: Dictionary -> T.Text
formatDictionary dic =
  T.unlines $
  ";; okuri-ari entries." :
  [ formatInput (Input inp (Just o)) <> " " <> formatCands cands
  | ((inp, o), cands) <- sortBy (comparing fst) $ HM.toList $ dic ^. okuriAriDic ]
  ++ "":
  ";; okuri-nasi entries." :
  [ formatInput (Input inp Nothing) <> " " <> formatCands cands
  | (inp, cands) <- sortBy (comparing fst) $ HM.toList $ dic ^. okuriNasiDic ]


isOkuri :: Char -> Bool
isOkuri c = isAlpha c && isLower c && isAscii c

comment :: Parser ()
comment = do
  cmt <- char ';' *> manyTill anyChar endOfLine
  guard $ T.pack (';':cmt) `notElem` [okuriAriStr, okuriNasiStr]
  return ()

line :: Parser ()
line = endOfLine <|> comment

midashi :: Okuri -> Parser Input
midashi Nasi = do
  mid <- takeWhile1 (not . isSpace)
  return $ Input mid Nothing
midashi Ari =  do
  mid <- takeWhile1 (not . isSpace)
  return $ Input (T.init mid) (Just $ T.last mid)

entry :: Okuri -> Parser (Input, [Candidate])
entry okr =
  (,) <$> midashi okr <*  skipSpace <*> candidates

candidates :: Parser [Candidate]
candidates = slashed candidate

converted :: Parser T.Text
converted = do
  mans <- peekChar
  guard $ maybe True (`notElem` "[]") mans
  takeWhile1 (`notElem` "\r\n/;")

note :: Parser T.Text
note = takeWhile (`notElem` "\r\n/")

slashed :: Parser a -> Parser [a]
slashed p = char '/' *> p `sepBy1` char '/' <* char '/'

candidate :: Parser Candidate
candidate = try okuriCand
        <|> simpleCandidate

okuriCand :: Parser Candidate
okuriCand = OkuriSub <$  char '['
                     <*> takeWhile1 (`notElem` ";/\r\n")
                     <*> slashed simpleCandidate
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

okuriDic :: Parser (Okuri, [(Input, [Candidate])])
okuriDic = do
  okr <- okuriPragma
  skipMany line
  ents <- entry okr `sepBy` skipMany line
  return (okr, ents)

dictionary :: Parser Dictionary
dictionary = do
  skipMany line
  dics <- okuriDic `sepBy` skipMany line
  let nds = [ (top, cands)
            | (Nasi, d) <- dics
            , (Input top _, cands) <- d]
      ods = [ ((top, ok), cands)
            | (Ari, d) <- dics
            , (Input top (Just ok), cands) <- d]
      dic = Dict (HM.fromListWith (++) ods) (HM.fromListWith (++) nds)
  skipMany line
  endOfInput
  return dic

sDictionary :: Dictionary
Right sDictionary = parseOnly dictionary $(litE . stringL . tail =<< runIO (readFile "data/SKK-JISYO.S"))
