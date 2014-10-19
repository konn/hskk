{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs          #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
module AppDelegate (objc_initialise) where
import Constants
import Messaging
import Text.InputMethod.SKK

import           Codec.Text.IConv         (Fuzzy (..), convertFuzzy)
import           Control.Applicative      ((<$>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async)
import           Control.Concurrent.Async (Async, asyncBound, cancel)
import           Control.Concurrent.STM   (TVar, atomically, modifyTVar')
import           Control.Concurrent.STM   (newTVarIO, readTVarIO, writeTVar)
import           Control.Concurrent.STM   (TVar, atomically)
import           Control.Exception        (SomeException (..), handle)
import           Control.Lens             hiding (act, ( # ))
import           Control.Monad            (forever)
import           Control.Monad            (forM)
import           Control.Monad            (when)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import           Language.C.Inline.ObjC
import           Language.C.Quote.ObjC
import           Network.HTTP.Conduit     (simpleHttp)
import           System.Directory         (doesFileExist, getHomeDirectory)
import           System.Directory         (removeFile, renameFile)
import           System.FilePath          (joinPath, splitPath)

objc_import ["Cocoa/Cocoa.h",
             "<InputMethodKit/InputMethodKit.h>",
             "HsFFI.h"]

defineClass "NSObject" Nothing
idMarshaller ''NSObject

definePhantomClass 1 "NSArray" (Just ''NSObject)
definePhantomClass 1 "NSMutableArray" (Just ''NSObject)

defineClass "NSDictionary" (Just ''NSObject)
idMarshaller ''NSDictionary

defineClass "AppDelegate" (Just ''NSObject)
idMarshaller ''AppDelegate

defineClass "NSUserDefaults" (Just ''NSObject)
idMarshaller ''NSUserDefaults

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

objc_interface [cunit|
@interface AppDelegate : NSObject<NSApplicationDelegate>
@property (weak) typename NSMenu *menu;
@property (assign) typename HsStablePtr session;

-(typename HsStablePtr)skkDic;
@end
|]

data Session = Session { _self       :: AppDelegate
                       , _saveThread :: Async ()
                       , _skkDic     :: TVar Dictionary
                       }
makeLenses ''Session

viewDic :: Session -> IO (TVar Dictionary)
viewDic = return . view skkDic

data DictKind = Remote | Local
              deriving (Read, Show, Eq, Ord)

kind2Tag :: DictKind -> Int
kind2Tag Remote = 0
kind2Tag Local = 1

tag2Kind :: Int -> DictKind
tag2Kind 0 = Remote
tag2Kind 1 = Local
tag2Kind d = error $ "tag2Kind: out of bound: " ++ show d

data DictConf = DictConf { _userDicPath   :: FilePath
                         , _otherDicConfs :: [MiscDict]
                         } deriving (Read, Show, Eq, Ord)

data MiscDict = MiscDict { _dictKind     :: DictKind
                         , _dictLocation :: String
                         } deriving (Read, Show, Eq, Ord)

miscDic2NSDict :: MiscDict -> IO NSDictionary
miscDic2NSDict (MiscDict k loc) =
  let tag = kind2Tag k
  in $(objc ['loc :> ''String, 'tag :> ''Int] $
      Class ''NSDictionary <: [cexp| @{@$string:(miscDicKindKey): [NSNumber numberWithInt: tag],
                                       @$string:(miscDicLocationKey): loc} |])

nsDict2MiscDic :: NSDictionary -> IO MiscDict
nsDict2MiscDic dic = do
  tag <- $(objc ['dic :> Class ''NSDictionary] $
           ''Int <: [cexp| [[dic valueForKey: @$string:(miscDicKindKey)] intValue] |] )
  loc <- $(objc ['dic :> Class ''NSDictionary] $
           ''String <: [cexp| [dic valueForKey: @$string:(miscDicLocationKey)] |] )
  return $ MiscDict (tag2Kind tag) loc

objc_marshaller 'miscDic2NSDict 'nsDict2MiscDic

listOfMiscDic2Array :: [MiscDict] -> IO (NSArray NSDictionary)
listOfMiscDic2Array dics = do
  let count = length dics
  arr <- $(objc ['count :> ''Int] $
           Class ''NSMutableArray <: [cexp| [NSMutableArray arrayWithCapacity: count] |])
  iforM_ dics $ \i obj ->
    $(objc ['arr :> Class ''NSMutableArray, 'i :> ''Int, 'obj :> ''MiscDict] $
      void [cexp| [arr insertObject:obj atIndex:i] |])
  $(objc ['arr :> Class ''NSMutableArray] $
         Class ''NSArray <: [cexp| (typename NSArray *)arr |])

array2ListOfMiscDict :: NSArray NSDictionary -> IO [MiscDict]
array2ListOfMiscDict arr = do
  count <- $(objc ['arr :> Class ''NSArray] $ ''Int <: [cexp| [arr count] |])
  forM [0..count] $ \i ->
    $(objc ['arr :> Class ''NSArray, 'i :> ''Int] $
           ''MiscDict <: [cexp| [arr objectAtIndex: i] |])

objc_marshaller 'listOfMiscDic2Array 'array2ListOfMiscDict

defineSelector newSelector { selector = "otherDictsConf"
                           , reciever = (''NSUserDefaults, "def")
                           , returnType = Just [t| [MiscDict] |]
                           , definition = [cexp| [def arrayForKey: @"otherDics"] |]
                           }

data DictionarySet = DictionarySet { userDic   :: Dictionary
                                   , otherDics :: [(String, Dictionary)]
                                   } deriving (Show, Eq)

makeLenses ''DictConf

getUserDicPath :: IO FilePath
getUserDicPath = do
  udef <- stdUserDefaults
  expandHome =<< $(objc ['udef :> Class ''NSUserDefaults] $
                         ''String <: [cexp| [udef stringForKey: @$string:(userDicKey)] |])

getDictConf :: IO DictConf
getDictConf = do
  udef <- stdUserDefaults
  udic <- getUserDicPath
  odic <- udef # otherDictsConf
  return $ DictConf udic odic

buildDics :: IO [Dictionary]
buildDics = do
  conf <- getDictConf
  mapMaybe (^? _Right) <$> mapM getDic (conf ^. otherDicConfs)

getDic :: MiscDict -> IO (Either String Dictionary)
getDic (MiscDict k loc) = do
  src <- case k of
    Local  -> T.readFile =<< expandHome loc
    Remote -> do
      bs <- simpleHttp $ "http://openlab.ring.gr.jp/skk/skk/dic/" ++ loc
      path <- expandHome $ "~/Library/Application Support/hSKK/" ++ loc
      let src = convertFuzzy Transliterate "EUC-JP" "UTF-8" bs
      LBS.writeFile path src
      return $ T.decodeUtf8 $ LBS.toStrict src
  return $ parseDictionary src

expandHome :: FilePath -> IO FilePath
expandHome path = joinPath <$>
  case splitPath path of
    ("~/" : rest) -> getHomeDirectory <&> (:rest)
    xs -> return xs

saveDictionary :: TVar Dictionary -> IO ()
saveDictionary ref = handle handler $ do
  dic <- readTVarIO ref
  udef <- stdUserDefaults
  path <- getUserDicPath
  nsLog "saving dictionary..."
  T.writeFile (path ++ ".rotate") $ formatDictionary dic
  doRemove <- doesFileExist path
  when doRemove $ removeFile path
  renameFile (path ++ ".rotate") path
  where
    handler (SomeException exc) = nsLog $ "error during saving: " ++ show exc


stdUserDefaults :: IO NSUserDefaults
stdUserDefaults =
  $(objc [] $ Class ''NSUserDefaults <:
    [cexp| [[NSUserDefaults alloc] initWithSuiteName: @$string:(userDefaultName)] |])

newSession :: AppDelegate -> IO Session
newSession app = do
  path <- getUserDicPath
  available <- doesFileExist path
  dic <- if available
    then either (const $ return sDictionary) return . parseDictionary . T.pack
           =<< readFile path
    else do
      T.writeFile path $ formatDictionary sDictionary
      return sDictionary
  dicRef <- newTVarIO dic
  as <- asyncBound (forever $ threadDelay (10*60*10^6) >> saveDictionary dicRef)
  return $ Session app as dicRef

objc_implementation [Typed 'viewDic, Typed 'newSession] [cunit|
@implementation AppDelegate

@synthesize menu = _menu;

- (void)applicationDidFinishLaunching:(typename NSNotification *)notification
{
  if (self) {
    self.session = newSession(self);
  }
}

-(typename HsStablePtr)skkDic
{
  return viewDic(self.session);
}

-(void) awakeFromNib
{
  typename NSMenuItem* preferences = [_menu itemWithTag: 1];
  if (preferences) {
    [preferences setAction:@selector(showPreferences:)];
  }
}
@end
|]

objc_emit
