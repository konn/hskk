{-# LANGUAGE DataKinds, DeriveDataTypeable, FlexibleInstances, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes   #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TupleSections      #-}
{-# LANGUAGE TypeOperators                                           #-}
module Messaging.Macros (defineClass, definePhantomClass, idMarshaller,
                         defineSelector, SelectorDef(..), newSelector, Argument(..)) where
import           Control.Applicative    ((<$>))
import           Control.Monad          (liftM, replicateM)
import           Data.Char              (isLower, isUpper, toLower, toUpper)
import           Data.Maybe             (fromMaybe)
import           Data.Typeable          (Typeable)
import           GHC.TypeLits           (Symbol)
import           Language.C.Inline.ObjC
import qualified Language.C.Quote       as QC
import           Language.Haskell.TH
import           Messaging.Core

defineClass :: String -> Maybe Name -> DecsQ
defineClass = definePhantomClass 0

definePhantomClass :: Int -> String -> Maybe Name -> DecsQ
definePhantomClass n name super = do
  let sym = mkName $ name ++ "Class"
  vs <- replicateM n (newName "x")
  empDec <- dataD (return []) sym [] [] [''Typeable]
  tyDec <- tySynD (mkName name) (map PlainTV vs) [t| Object $(conT sym) |]
  mClsDec <- genSubtypes (conT sym) (ConT . mkName . (++ "Class") . nameBase <$> super)
  let tyCls = foldl1 appT (conT (mkName name) : map varT vs)
  let mName = mkName $ "marshal" ++ name
  marshalD <- sequence [sigD mName $ forallT (map PlainTV vs) (return []) [t| $tyCls -> IO $tyCls |]
                       ,valD (varP mName) (normalB [| return |]) []]
  return $ empDec : tyDec : mClsDec ++ marshalD

idMarshaller :: Name -> DecsQ
idMarshaller name = do
  VarI mName _ _ _ <- reify (mkName $ "marshal" ++ nameBase name)
  objc_marshaller mName mName

genSubtypes :: TypeQ -> Maybe Type -> DecsQ
genSubtypes _    Nothing      = return []
genSubtypes name (Just super) = do
  let geq = ''(:>)
      geqT = AppT (AppT (ConT geq) (ConT ''Symbol)) (ConT ''Symbol)
  ClassI _ insts <- reify ''(:>)
  let supers = [ typ | InstanceD [] (AppT (AppT geqT0 typ) sub) _ <- insts
                     , sub == super, geqT0 == geqT]
  concat <$> mapM (\s -> [d| instance $(return s) :> $name |]) (super:supers)

data Argument = Defined Name
              | String :>:  TypeQ
              | String :>>: Name
              | String :>.>: Name

getName :: Argument -> Name
getName (Defined name) = name
getName (str :>: _)    = mkName str
getName (str :>>: _)   = mkName str
getName (str :>.>: _)  = mkName str

toAnnotation :: Argument -> Annotated Name
toAnnotation (Defined name) = Typed name
toAnnotation (str :>: a)    = mkName str :> a
toAnnotation (str :>>: a)   = mkName str :> a
toAnnotation (str :>.>: a)   = mkName str :> Class a

argType :: Argument -> TypeQ
argType (Defined name) = do
  VarI _ typ _ _ <- reify name
  return typ
argType (_ :>: a)  = a
argType (_ :>.>: a)  = conT a
argType (_ :>>: a) = conT a

data SelectorDef = Selector { selector :: String
                            , reciever :: (Name, String)
                            , arguments :: [Argument]
                            , environment :: [Annotated Name]
                            , returnType  :: Maybe TypeQ
                            , definition  :: QC.Exp
                            }

newSelector :: SelectorDef
newSelector = Selector "" (undefined, "recv") [] [] Nothing undefined

defineSelector :: SelectorDef -> DecsQ
defineSelector (Selector sel (cls, recv) args env mret expr) = do
  let typs = map (liftM (NotStrict, ) . argType) args
      msgDec = dataInstD (return []) ''Message [toSym sel] [normalC conName typs] []
      body = objc ((recName :> conT cls) : map toAnnotation args ++ env) $ maybe void (<:) mret expr
      sendDec =
        funD 'send' [clause [varP recName, conP conName $ map (varP . getName) args]
                     (normalB body) [] ]
  aliasSig <- sigD funName $ foldr funT [t| Message $(toSym sel) |] $ map (liftM snd) typs
  aliasDec <- funD funName [ clause [] (normalB $ conE conName) []]
  ds <- head <$> [d| type instance Returns $(toSym sel) = IO $(fromMaybe [t| () |] mret) |]
  inst <- instanceD (return []) [t| Selector $(conT $ mkName $ nameBase cls ++ "Class") $(toSym sel) |] $
          [msgDec, return ds, sendDec]

  return [inst, aliasSig, aliasDec]
  where
    recName = mkName recv
    toSym   = litT . strTyLit
    funName = mkName $ camelCase sel
    conName = mkName $ strictCamelCase sel

funT :: TypeQ -> TypeQ -> TypeQ
funT a b = appT (appT arrowT a) b

camelCase :: String -> String
camelCase "" = ""
camelCase (c : cs)
  | isLower c = c : cs
  | otherwise =
    case span isUpper cs of
      (us, rest@(_:_)) | not $ null us -> map toLower (c : init us) ++ last us : rest
      (us, rest) -> map toLower (c:us) ++ rest

strictCamelCase :: String -> String
strictCamelCase "" = ""
strictCamelCase xs@(x:_)
  | isUpper x = xs
  | otherwise =
  case camelCase xs of
    c : cs -> toUpper c : cs
    _ -> xs

{- Macro Design

defineSelector "setIntValue" ''NSControl "ctrl" ["i" :> ''Int] $
  ''Int <: [cexp| [ctrl setIntValue: i] |]
-}
