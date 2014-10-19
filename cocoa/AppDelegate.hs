{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs          #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
module AppDelegate (objc_initialise) where
import Constants
import DataTypes
import Messaging
import Text.InputMethod.SKK

import           Codec.Text.IConv         (Fuzzy (..), convertFuzzy)
import           Control.Applicative      ((<$>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent       (forkOS)
import           Control.Concurrent.Async (Async, asyncBound, cancel)
import           Control.Concurrent.STM   (TVar, atomically, newTVarIO)
import           Control.Concurrent.STM   (readTVarIO, writeTVar)
import           Control.Exception        (SomeException (..), handle)
import           Control.Lens             hiding (act, ( # ))
import           Control.Monad            (forM, forever, when)
import           Control.Monad            (forM_)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import           Language.C.Inline.ObjC
import           Language.C.Quote.ObjC
import           Network.HTTP.Conduit     (HttpException, simpleHttp)
import           System.Directory         (doesFileExist, getHomeDirectory)
import           System.Directory         (removeFile, renameFile)
import           System.FilePath          (joinPath, splitPath)

objc_import ["Cocoa/Cocoa.h",
             "<InputMethodKit/InputMethodKit.h>",
             "HsFFI.h"]

defineClass "NSObject" Nothing
idMarshaller ''NSObject

definePhantomClass 1 "NSArray" (Just ''NSObject)
definePhantomClass 1 "NSMutableArray" (Just ''NSArray)

defineClass "NSDictionary" (Just ''NSObject)
idMarshaller ''NSDictionary

defineClass "AppDelegate" (Just ''NSObject)
idMarshaller ''AppDelegate

defineClass "NSUserDefaults" (Just ''NSObject)
idMarshaller ''NSUserDefaults

defineSelector newSelector { selector = "toDict"
                           , reciever = (''NSUserDefaults, "def")
                           , returnType = Just [t| NSDictionary |]
                           , definition = [cexp| [def dictionaryRepresentation] |]
                           }


defineSelector newSelector { selector = "sync"
                           , reciever = (''NSUserDefaults, "def")
                           , definition = [cexp| [def synchronize] |]
                           }

inspect :: String -> Object t -> IO ()
inspect ind obj = $(objc ['ind :> ''String, 'obj :> Class ''NSObject] $
                    void [cexp| NSLog(@"%@: %@", ind, [obj description]) |])

defineClass "NSString" (Just ''NSObject)
idMarshaller ''NSString

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

objc_interface [cunit|
@interface AppDelegate : NSObject<NSApplicationDelegate>
@property (weak) typename NSMenu *menu;
@property (assign) typename HsStablePtr session;

-(typename HsStablePtr)skkDic;
@end
|]

data Session = Session { _self         :: AppDelegate
                       , _saveThread   :: Async ()
                       , _reloadThread :: Async ()
                       , _skkDic       :: TVar DictionarySet
                       }
makeLenses ''Session

viewDic :: Session -> IO (TVar DictionarySet)
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

makeLenses ''DictConf

miscDic2NSDict :: MiscDict -> IO NSDictionary
miscDic2NSDict (MiscDict k loc) = do
  nsLog $ "marshaling MiscDict..."
  let tag = kind2Tag k
  $(objc ['loc :> ''String, 'tag :> ''Int] $
      Class ''NSDictionary <: [cexp| @{@$string:(miscDicKindKey): [NSNumber numberWithInt: tag],
                                       @$string:(miscDicLocationKey): loc} |])

fromNSString :: NSString -> IO String
fromNSString str = $(objc ['str :> Class ''NSString] $ ''String <: [cexp| str |])

nsDict2MiscDic :: NSDictionary -> IO MiscDict
nsDict2MiscDic dic = do
  nsLog $ "demarshaling NSDictionary..."
  tag <- $(objc ['dic :> Class ''NSDictionary] $
           ''Int <: [cexp| [[dic valueForKey: @$string:(miscDicKindKey)] intValue] |] )
  nsLog $ "demarshaled tag: " ++ show tag
  loc <- $(objc ['dic :> Class ''NSDictionary] $
           ''String <: [cexp| (typename NSString *)[dic objectForKey: @$string:(miscDicLocationKey)] |] )
  nsLog $ "demarshaled loc: " ++ loc
  return $ MiscDict (tag2Kind tag) loc

objc_marshaller 'miscDic2NSDict 'nsDict2MiscDic

listOfMiscDic2Array :: [MiscDict] -> IO (NSArray NSDictionary)
listOfMiscDic2Array dics = do
  let count = length dics
  arr <- $(objc ['count :> ''Int] $
           Class ''NSMutableArray <: [cexp| [NSMutableArray arrayWithCapacity: count] |])
  forM_ dics $ \obj -> do
    nsLog $ "marshaling conf element: " ++ show obj
    $(objc ['arr :> Class ''NSMutableArray, 'obj :> ''MiscDict] $
      void [cexp| [arr addObject:obj] |])
  return $ cast (arr :: NSMutableArray NSDictionary)

array2ListOfMiscDict :: NSArray NSDictionary -> IO [MiscDict]
array2ListOfMiscDict arr = do
  count <- $(objc ['arr :> Class ''NSArray] $ ''Int <: [cexp| [arr count] |])
  nsLog $ "count: " ++ show count
  inspect "arr" arr
  forM [0..count - 1] $ \i -> do
    $(objc ['arr :> Class ''NSArray, 'i :> ''Int] $
           ''MiscDict <: [cexp| [arr objectAtIndex: i] |])

objc_marshaller 'listOfMiscDic2Array 'array2ListOfMiscDict

{-
dic2ListOfMiscDict :: NSDictionary -> IO [MiscDict]
dic2ListOfMiscDict dic = do
  inspect "dic" dic
  kinds <- $(objc ['dic :> Class ''NSDictionary] $
                  Class ''NSArray <: [cexp| [dic objectForKey: @$string:miscDicKindKey] |])
  locs  <- $(objc ['dic :> Class ''NSDictionary] $
                  Class ''NSArray <: [cexp| [dic objectForKey: @$string:miscDicLocationKey] |])
  count <- $(objc ['kinds :> Class ''NSArray] $
             ''Int <: [cexp| [kinds count] |])
  forM [0..count - 1]$ \i -> do
    t   <- $(objc ['kinds :> Class ''NSArray, 'i :> ''Int] $
             ''Int <: [cexp| [[kinds objectAtIndex: i] intValue] |])
    loc <- $(objc ['locs :> Class ''NSArray, 'i :> ''Int] $
             ''String <: [cexp| [locs objectAtIndex: i] |])
    return $ MiscDict (tag2Kind t) loc


listOfMiscDict2Dic :: [MiscDict] -> IO NSDictionary
listOfMiscDict2Dic mds = do
  let count = length mds
  locs  <- $(objc ['count :> ''Int] $ Class ''NSMutableArray <:
             [cexp| [NSMutableArray arrayWithCapacity: count] |])
  kinds <- $(objc ['count :> ''Int] $ Class ''NSMutableArray <:
             [cexp| [NSMutableArray arrayWithCapacity: count] |])
  forM_ mds $ \(MiscDict k l) -> do
    let tag = kind2Tag k
    $(objc ['locs :> Class ''NSMutableArray, 'l :> ''String] $ void [cexp| [locs addObject: l] |])
    $(objc ['kinds :> Class ''NSMutableArray, 'tag :> ''Int] $ void [cexp| [kinds addObject: [NSNumber numberWithInt: tag]] |])
  $(objc ['locs :> Class ''NSArray, 'kinds :> Class ''NSArray] $
    Class ''NSDictionary
    <: [cexp| @{ @$string:miscDicKindKey : kinds, @$string:miscDicLocationKey : locs} |])

objc_marshaller 'listOfMiscDict2Dic 'dic2ListOfMiscDict
-}
defineSelector newSelector { selector = "otherDicsConf"
                           , reciever = (''NSUserDefaults, "def")
                           , returnType = Just [t| [MiscDict] |]
                           , definition = [cexp| [def arrayForKey: @$string:(otherDicsKey)] |]
                           }

getUserDicPath :: IO FilePath
getUserDicPath = do
  udef <- stdUserDefaults
  expandHome =<< $(objc ['udef :> Class ''NSUserDefaults] $
                         ''String <: [cexp| [udef stringForKey: @$string:(userDicKey)] |])

getDictConf :: IO DictConf
getDictConf = do
  udef <- stdUserDefaults
  udic <- getUserDicPath
  odic <- udef # otherDicsConf
  return $ DictConf udic odic

miscDics :: DictConf -> IO [Dictionary]
miscDics conf = mapMaybe (^? _Right) <$> mapM getDic (conf ^. otherDicConfs)

getDic :: MiscDict -> IO (Either String Dictionary)
getDic (MiscDict k loc) = handle httpHandler $ do
  src <- case k of
    Local  -> T.readFile =<< expandHome loc
    Remote -> do
      bs <- simpleHttp $ "http://openlab.ring.gr.jp/skk/skk/dic/" ++ loc
      path <- expandHome $ "~/Library/Application Support/hSKK/" ++ loc
      let src = convertFuzzy Discard "EUC-JP" "UTF-8" bs
      forkOS $ LBS.writeFile path src
      return $ T.decodeUtf8 $ LBS.toStrict src
  return $ parseDictionary src

httpHandler :: HttpException -> IO (Either String a)
httpHandler exc = return $ Left $ show exc

expandHome :: FilePath -> IO FilePath
expandHome path = joinPath <$>
  case splitPath path of
    ("~/" : rest) -> getHomeDirectory <&> (:rest)
    xs -> return xs

saveDictionary :: TVar DictionarySet -> IO ()
saveDictionary ref = handle handler $ do
  dicSet <- readTVarIO ref
  path <- getUserDicPath
  nsLog "saving dictionary..."
  T.writeFile (path ++ ".rotate") $ formatDictionary (dicSet ^. userDic)
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
  udef <- stdUserDefaults
  defaultsPath <- $(objc [] $ Class ''NSString <:
                    [cexp| [[NSBundle mainBundle] pathForResource:@"UserDefaults"
                            ofType:@"plist"] |])
  dic <- $(objc ['defaultsPath :> Class ''NSString] $ Class ''NSDictionary <:
                 [cexp| [NSDictionary dictionaryWithContentsOfFile:defaultsPath] |])
  $(objc ['udef :> Class ''NSUserDefaults, 'dic :> Class ''NSDictionary] $
           void [cexp| [udef registerDefaults:dic] |])
  udef # sync
  inspect "UserDefaults" =<< udef # toDict
  dicRef <- newTVarIO =<< buildDicSet
  saver <- asyncBound $ forever $ do
    threadDelay (10*60*10^6)
    saveDictionary dicRef

  reloader <- asyncBound $ forever $ do
    threadDelay (60*60*10^6)
    nsLog "reloading dictionaries..."
    atomically . writeTVar dicRef =<< buildDicSet
  return $ Session app saver reloader dicRef

buildDicSet :: IO DictionarySet
buildDicSet = do
  nsLog $ "building dics"
  conf <- getDictConf
  nsLog $ "conf: " ++ show conf
  let path = conf ^. userDicPath
  available <- doesFileExist path
  dic <- if available
    then either (const $ return sDictionary) return . parseDictionary . T.pack
           =<< readFile path
    else do
      T.writeFile path $ formatDictionary sDictionary
      return sDictionary
  others <- miscDics conf
  return $ DictionarySet dic (foldr1 mergeDic (dic:others))

finalize :: Session -> IO ()
finalize sess = do
  cancel (sess ^. saveThread)
  cancel (sess ^. reloadThread)
  saveDictionary $ sess ^. skkDic

undefinedDic :: IO DictionarySet
undefinedDic = return $ DictionarySet emptyDic emptyDic

objc_implementation [Typed 'undefinedDic, Typed 'finalize,
                     Typed 'viewDic, Typed 'newSession] [cunit|
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
  if (self.session) {
    return viewDic(self.session);
  } else {
    return undefinedDic();
  }
}

-(void) awakeFromNib
{
  typename NSMenuItem* preferences = [_menu itemWithTag: 1];
  if (preferences) {
    [preferences setAction:@selector(showPreferences:)];
  }
}

- (void)applicationWillTerminate:(typename NSNotification *)aNotification {
    finalize(self.session);
}
@end
|]

-- objc_initialise = undefined
objc_emit
