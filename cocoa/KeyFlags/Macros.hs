{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses                     #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings, PatternGuards #-}
{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies                  #-}
module KeyFlags.Macros where
import           Control.Arrow       ((***), (>>>))
import           Data.Bits           (Bits (..), (.&.))
import           Data.Char           (isAlpha, toLower)
import           Data.Char           (isDigit)
import           Data.Data           (Data, Typeable)
import           Data.Function       (on)
import           Data.List           (nubBy)
import           Data.Maybe          (maybeToList)
import qualified Data.Text           as T
import           Language.Haskell.TH (ConQ, DecsQ, ExpQ, MatchQ, Strict (..))
import           Language.Haskell.TH (Type (ConT), TypeQ, appE, charL, clause)
import           Language.Haskell.TH (conE, conP, conT, dataD, funD, integerL)
import           Language.Haskell.TH (Lit (..), lamCaseE, litE, litP, match,
                                      mkName)
import           Language.Haskell.TH (normalB, normalC, normalC, sigD, wildP)
import           Language.Haskell.TH (listE)
import           Numeric             (readHex)

type CodeTable = [(Either (T.Text, Lit) T.Text, Integer)]

camelize :: T.Text -> T.Text
camelize = T.concat . map T.toTitle  . T.splitOn "_"

class (Bits (Mask a), Eq (Mask a)) => Masked a where
  type Mask a
  toMask :: a -> Mask a

compatible :: Masked a => a -> a -> Bool
compatible a b = testCode a $ toMask b

testCode :: Masked a => a -> Mask a -> Bool
testCode a m = toMask a .&. m == toMask a

procLR :: CodeTable -> CodeTable
procLR table =
  let ls = collectSuffix "L"
      rs = collectSuffix "R"
      lrs = [ (Right n, b .&. b')
            | (n, b) <- ls
            , b' <- maybeToList $ lookup n rs
            ]
  in table ++ lrs
  where
    standalones = [(t, n) | (Right t, n) <- table ]
    collectSuffix suf = nubBy ((==) `on` fst)
      [ (pre, n)
      | (t, n) <- standalones
      , pre <- maybeToList $ T.stripSuffix suf t
      ]

defineKeyCode :: String -> TypeQ -> CodeTable -> DecsQ
defineKeyCode name0 typ table = do
  let name = T.unpack $ T.toTitle $ T.pack name0
      enumName = mkName $ toLower (head name) : tail name ++ "s"
      funName = mkName $ "decode" ++ name
  ddef <- dataD (return []) (mkName name) [] (buildCons table) [''Show, ''Eq, ''Ord, ''Data, ''Typeable]
  fsig <- sigD funName [t| $typ -> Maybe $(conT $ mkName name) |]
  let decs = flip map table $ \ (val, num) ->
             clause [litP $ integerL num] (normalB $ [| Just $(buildVal val) |]) []
      maskBody = lamCaseE $ map buildMatch table
  fdef <- funD funName $ decs ++ [clause [wildP] (normalB [| Nothing |]) []]
  inst <- [d| instance Masked $(conT $ mkName name) where
                 type Mask $(conT $ mkName name) = $(typ)
                 toMask = $(maskBody)
            |]
  esig <- sigD enumName [t| [$(conT $ mkName name)] |]
  enums <- funD enumName [ clause [] (normalB $ listE $ map (buildVal . fst) table) []]
  return $ [ddef, fsig, fdef, esig, enums] ++ inst


buildMatch :: (Either (T.Text, Lit) T.Text, Integer) -> MatchQ
buildMatch (Left (txt, ch), n) =
  match (conP (mkName $ T.unpack txt) [litP ch]) (normalB $ litE $ integerL n) []
buildMatch (Right txt, n) =
  match (conP (mkName $ T.unpack txt) []) (normalB $ litE $ integerL n) []

buildVal :: Either (T.Text, Lit) T.Text -> ExpQ
buildVal (Left (txt, c)) = conE (mkName $ T.unpack txt) `appE` litE c
buildVal (Right txt) = conE $ mkName $ T.unpack txt

buildCons :: CodeTable -> [ConQ]
buildCons = map (uncurry normalC) . nubBy ((==) `on` fst) . map (toCon . fst)
  where
    toCon i =
      let n = mkName $ T.unpack $ either fst id i
          sts = case i of
            Left (_, CharL _) -> [return (IsStrict, ConT ''Char)]
            Left (_, IntegerL _) -> [return (IsStrict, ConT ''Int)]
            _ -> []
      in (n, sts)

toConName :: T.Text -> Either (T.Text, Lit) T.Text
toConName str
  | T.length str == 1 && isAlpha (T.head str) = Left ("Char", charL $ toLower $ T.head str)
  | Just num <- prefixed "KEY_" str = Left ("Char", num)
  | Just num <- prefixed "JIS_" str = Left ("JIS", num)
  | Just num <- prefixed "KEYPAD_" str = Left ("Keypad", num)
  | Just num <- readFn str = Left ("Fn", num)
  | otherwise = Right $ camelize str

readFn :: T.Text -> Maybe Lit
readFn t = case T.stripPrefix "F" t of
  Just s | T.all isDigit s -> Just $ integerL $ read $ T.unpack s
  _ -> Nothing

prefixed :: T.Text -> T.Text -> Maybe Lit
prefixed pre str = T.stripPrefix pre str >>= go
  where
    go "UNDERSCORE"    = Just $ charL '_'
    go "ATMARK"        = Just $ charL '@'
    go "BRACKET_LEFT"  = Just $ charL '['
    go "BRACKET_RIGHT" = Just $ charL ']'
    go "COLON"         = Just $ charL ':'
    go "HAT"           = Just $ charL '^'
    go "YEN"           = Just $ charL '¥'
    go "DOLLAR"        = Just $ charL '$'
    go "MINUS"         = Just $ charL '-'
    go "EQUAL"         = Just $ charL '='
    go t | T.length t == 1 = Just $ charL $ T.head t
         | otherwise = Nothing

parse :: T.Text -> CodeTable
parse = T.lines
    >>> filter (\a -> not (T.null a) && not ("//" `T.isPrefixOf` a))
    >>> map (T.breakOn " " >>> toConName *** (runReadS readHex . T.drop 2 . T.strip))

runReadS :: ReadS a -> T.Text -> a
runReadS rd str =
  case rd $ T.unpack str of
    [(a, "")] -> a
    _ -> error $ "runReadS: " ++ show str
