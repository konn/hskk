{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, GADTs          #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-orphans #-}
module AppDelegate (objc_initialise) where
import Constants
-- import DataTypes
import Messaging
import Text.InputMethod.SKK

import           Codec.Text.IConv         (Fuzzy (..), convertFuzzy)
import           Control.Applicative      ((<$>))
import           Control.Arrow            ((>>>))
import           Control.Concurrent       (threadDelay)
import           Control.Concurrent       (forkOS)
import           Control.Concurrent       (readMVar)
import           Control.Concurrent       (newMVar)
import           Control.Concurrent       (swapMVar)
import           Control.Concurrent       (modifyMVar_)
import           Control.Concurrent       (modifyMVar)
import           Control.Concurrent.Async (Async, asyncBound, cancel, poll)
import           Control.Concurrent.MVar  (MVar)
import           Control.Exception        (SomeException (..), handle)
import           Control.Exception        (throw)
import           Control.Exception        (throwIO)
import           Control.Exception        (evaluate)
import           Control.Lens             hiding (act, ( # ))
import           Control.Lens.Extras      (is)
import           Control.Monad            (forM, forever, when)
import           Control.Monad            (forM_)
import           Control.Monad            (join)
import           Control.Monad            ((<=<))
import qualified Data.ByteString.Lazy     as LBS
import           Data.Either              (lefts)
import           Data.List                (partition)
import           Data.Maybe               (mapMaybe)
import           Data.Maybe               (fromJust)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.IO             as T
import           Data.Typeable            (Typeable)
import           Language.C.Inline.ObjC
import           Language.C.Quote.ObjC
import           Network.HTTP.Conduit     (HttpException, simpleHttp)
import           System.Directory         (doesFileExist, getHomeDirectory)
import           System.Directory         (removeFile, renameFile)
import           System.FilePath          (joinPath, splitPath)

objc_import ["Cocoa/Cocoa.h",
             "<InputMethodKit/InputMethodKit.h>",
             "HsFFI.h"]

data AsyncDicSet = ADicSet { _udic   :: Dictionary
                           , _adics  :: [Async Dictionary]
                           , _merged :: Dictionary
                           } deriving (Typeable)

makeLenses ''AsyncDicSet

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
-(typename HsStablePtr)
   registerCandidate: (typename HsStablePtr) cand
            forInput:(typename HsStablePtr) input;
-(typename HsStablePtr)
   unregisterCandidate: (typename HsStablePtr) cand
              forInput:(typename HsStablePtr) input;
@end
|]

data Session = Session { _self         :: AppDelegate
                       , _saveThread   :: Async ()
                       , _reloadThread :: Async ()
                       , _skkDic       :: MVar AsyncDicSet
                       }
makeLenses ''Session


viewDic :: Session -> IO Dictionary
viewDic sess =  modifyMVar (sess ^. skkDic) $ \dics -> do
  lrs <- mapM poll (dics ^. adics)
  let (done0, lo0) = partition (is (_Just._Right) . fst) $ zip lrs (dics ^. adics)
      lo = map snd lo0
      done = map (^?! _1._Just._Right) done0
  if null done
    then return (dics, dics ^. merged)
    else do
    let m' = foldl mergeDic (dics ^. merged) done
    return (dics & adics .~ lo & merged .~ m', m')

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
  ud <- getUserDicPath
  odic <- udef # otherDicsConf
  return $ DictConf ud odic

miscDics :: DictConf -> IO [Async Dictionary]
miscDics conf = mapM getDic (conf ^. otherDicConfs)

getDic :: MiscDict -> IO (Async Dictionary)
getDic (MiscDict k loc) = asyncBound $ handle (httpHandler loc) $ do
  src <- case k of
    Local  -> T.readFile =<< expandHome loc
    Remote -> do
      nsLog $ "Downloading Dictionary: "++ loc
      bs <- simpleHttp $ "http://openlab.ring.gr.jp/skk/skk/dic/" ++ loc
      nsLog $ "Downloaded: "++ loc
      path <- expandHome $ "~/Library/Application Support/hSKK/" ++ loc
      let src = convertFuzzy Discard "EUC-JP" "UTF-8" bs
      LBS.writeFile path src
      nsLog $ "Parsed: "++ loc
      return $ T.decodeUtf8 $ LBS.toStrict src
  either (throw . userError) return $ parseDictionary src

httpHandler :: String -> HttpException -> IO a
httpHandler loc exc = do
  nsLog $ "HTTP error during loading " ++ loc ++ ": " ++ show exc
  throw exc


expandHome :: FilePath -> IO FilePath
expandHome path = joinPath <$>
  case splitPath path of
    ("~/" : rest) -> getHomeDirectory <&> (:rest)
    xs -> return xs

saveDictionary :: MVar AsyncDicSet -> IO ()
saveDictionary ref = handle handler $ do
  dicSet <- readMVar ref
  path <- getUserDicPath
  nsLog "saving dictionary..."
  T.writeFile (path ++ ".rotate") $ formatDictionary (dicSet ^. udic)
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
  dicRef <- newMVar =<< buildDicSet
  saver <- asyncBound $ forever $ do
    threadDelay (10*60*10^6)
    saveDictionary dicRef

  reloader <- asyncBound $ forever $ do
    threadDelay (60*60*10^6)
    nsLog "reloading dictionaries..."
    swapMVar dicRef =<< buildDicSet
  return $ Session app saver reloader dicRef

buildDicSet :: IO AsyncDicSet
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
  return $ ADicSet dic others dic

finalize :: Session -> IO ()
finalize sess = do
  cancel (sess ^. saveThread)
  cancel (sess ^. reloadThread)
  saveDictionary $ sess ^. skkDic

undefinedDic :: IO AsyncDicSet
undefinedDic = return $ ADicSet emptyDic [] emptyDic

registerWord :: Session -> Input -> Candidate -> IO ()
registerWord sess inp cand =
  modifyMVar_ (sess ^. skkDic) (return . insertADicSet inp cand)

insertADicSet :: Input -> Candidate -> AsyncDicSet -> AsyncDicSet
insertADicSet inp cand = udic %~ insert inp cand
                     >>> merged %~ insert inp cand

unregisterWord :: Session -> Input -> T.Text -> IO ()
unregisterWord  sess inp cand =
  modifyMVar_ (sess ^. skkDic) (return . delADicSet inp cand)

delADicSet :: Input -> T.Text -> AsyncDicSet -> AsyncDicSet
delADicSet inp txt = udic   %~ unregister inp txt
                 >>> merged %~ unregister inp txt

objc_implementation [Typed 'undefinedDic, Typed 'finalize,
                     Typed 'viewDic, Typed 'newSession,
                     Typed 'registerWord, Typed 'unregisterWord] [cunit|
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

-(void)
   registerCandidate: (typename HsStablePtr) cand
            forInput:(typename HsStablePtr) input
{
  registerWord(self.session, input, cand);
}

-(void)
   unregisterCandidate: (typename HsStablePtr) cand
              forInput:(typename HsStablePtr) input;
{
  unregisterWord(self.session, input, cand);
}
@end
|]

-- objc_initialise = undefined
objc_emit
