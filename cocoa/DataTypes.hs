{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module DataTypes (DictionarySet(..), userDic, mergedDic) where
import Control.Lens         (makeLenses)
import Data.Typeable        (Typeable)
import Text.InputMethod.SKK

data DictionarySet = DictionarySet { _userDic   :: Dictionary
                                   , _mergedDic :: Dictionary
                                   } deriving (Show, Eq, Typeable)

makeLenses ''DictionarySet

