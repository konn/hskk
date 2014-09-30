{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls              #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes, RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TupleSections         #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
module HSKKController ( objc_initialise ) where
import Constants
import KeyFlags
import Messaging

import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Lens           (noneOf, (%~), (&), (.~), (^.), _Just)
import           Control.Lens           (traverse)
import           Control.Lens           (makeLenses)
import           Control.Lens.Extras    (is)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS
import           Data.Char              (isControl)
import           Data.Char              (toUpper)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Typeable          (Typeable)
import           FRP.Ordrea
import           Language.C.Inline.ObjC
import           Language.C.Quote.ObjC

objc_import ["<Cocoa/Cocoa.h>",
             "<InputMethodKit/InputMethodKit.h>",
             "HsFFI.h"]

defineClass "NSObject"    Nothing
idMarshaller ''NSObject

defineClass "IMKServer" (Just ''NSObject)
idMarshaller ''IMKServer

defineClass "IMKInputController" (Just ''NSObject)
idMarshaller ''IMKInputController

defineClass "HSKKController" (Just ''IMKInputController)
idMarshaller ''HSKKController

defineClass "NSString"    (Just ''NSObject)
idMarshaller ''NSString

defineSelector
  newSelector { selector = "insertText"
              , reciever = (''NSObject, "sender")
              , arguments = ["string" :>>: ''String]
              , definition = [cexp| [(id)sender insertText: string
                                      replacementRange: NSMakeRange(NSNotFound, NSNotFound)]|]
              }

defineSelector
  newSelector { selector = "setMarkedText"
              , reciever = (''NSObject, "sender")
              , arguments = ["string" :>>: ''String, "start" :>>: ''Int, "length" :>>: ''Int]
              , definition = [cexp| [(id)sender setMarkedText: string
                                      selectionRange: NSMakeRange(start, length)
                                      replacementRange: NSMakeRange(NSNotFound, NSNotFound)]|]
              }

type Client = NSObject

data ClientState  = ClientState { pushCmd :: SKKCommand -> IO Bool
                                , mode    :: ConvMode
                                } deriving (Typeable)

data Session = Session { _dict     :: M.Map Client ClientState
                       , _self     :: HSKKController
                       , _convMode :: ConvMode
                       } deriving (Typeable)

makeLenses ''Session

objc_interface [cunit|
@interface HSKKController : IMKInputController
@property (assign) typename HsStablePtr session;
@end
|]

objc_typecheck

updateSession :: Session -> IO ()
updateSession sess =
  let myself = sess ^. self
  in $(objc ['sess :> ''Session, 'myself :> ''HSKKController] $
       void [cexp| myself.session = sess |])

changeMode :: Session -> String -> IO ()
changeMode sess str = do
  case lookupMode str of
    Nothing -> return ()
    Just kana -> do
      updateSession $ sess & convMode .~ kana
      return ()

pushKey :: Session -> NSObject -> [SKKCommand] -> IO Bool
pushKey session client input = do
  upd <- getClientState client session
  accepted <- and <$> mapM upd input
  return accepted

pushKey2 :: Session -> NSObject -> String -> CLong -> CULong -> IO Bool
pushKey2 session client input keyCode flags = do
  let modifs = decodeModifiers flags
      key = decodeKeyboard keyCode
      shifted = all (compatible Shift) modifs
  -- nsLog $ "input: " ++ show (input, key, decodeModifiers flags)
  if not (isControl $ head input) && all isAlphabeticModifier modifs &&
     (key & is (_Just . _Char))
    then pushKey session client $ map Incoming input
    else case key of
    Just Delete -> pushKey session client [Incoming '\DEL']
    Just Enter  -> pushKey session client [Incoming '\n']
    Just Return -> pushKey session client [Incoming '\n']
    Just Tab    -> pushKey session client [Incoming '\t']
    Just Space  -> pushKey session client [Incoming ' ']
    Just (Char 'q') | all (compatible Control) modifs ->
      pushKey session client [ToggleHankaku]
    Just (JIS 'q') | all (compatible Control) modifs ->
      pushKey session client [ToggleHankaku]
    Just JisEisuu -> return True
    Just JisKana  -> return True
    Just (Char c)
      | all (`elem` [Alternate, Shift]) modifs ->
        let c' = if shifted then toUpper c else c
        in pushKey session client [Incoming c']
    Just (JIS c)
      | all (`elem` [Alternate, Shift]) modifs ->
        let c' = if shifted then toUpper c else c
        in pushKey session client [Incoming c']
--     Just Undo -> pushKey session client "\b"
    _ -> return False

getClientState :: Client -> Session
           -> IO (SKKCommand -> IO Bool)
getClientState sender sess0
  | Just st <- M.lookup sender $ sess0 ^. dict
  , sess0 ^. convMode == mode st = return $ pushCmd st
  | otherwise = do
    let kana = sess0 ^. convMode
    eev <- newExternalEvent
    step <- start $ do
      input <- externalE eev
      ev <- skkConvE defKanaTable kana sDictionary defPager defCSelector input
      _ <- generatorE $ mapM edited <$> ev
      return $ all (noneOf (_Idle . traverse) (is _NoHit)) <$> eventToBehavior (flattenE ev)
    let push inp = triggerExternalEvent eev inp >> step
        st = ClientState push kana
        sess = sess0 & dict %~ M.insert sender st
    updateSession sess
    return push
  where
    edited (Idle eds) = mapM_ plainEdited eds
    edited (Page cands mokuri) = do
      let msg | [a] <- cands = "▼" <> a <> fromMaybe "" mokuri
              | otherwise    = "[候補: " <> T.intercalate " / "
                               (zipWith (\a b -> T.singleton a <> ": " <> b) "asdfjkl"  cands) <> "]"
                                <> fromMaybe "" mokuri
      liftIO $ sender # setMarkedText (T.unpack msg) 0 (T.length msg)
    edited (ConvNotFound _ _) = liftIO $ sender # setMarkedText "" 0 0
    edited (Completed t) = do
      liftIO $ sender # insertText (T.unpack t)
    edited (Okuri body o) = do
      let msg = "▽" <> body <> "*" <> o
      liftIO $ sender # setMarkedText (T.unpack msg) 0 (T.length msg)
    edited (Converting str) = do
      let ans = "▽" <> str
      liftIO $ sender # setMarkedText (T.unpack ans) 0 (T.length ans)
    plainEdited (Converted text) =
      liftIO $ sender # insertText (T.unpack text)
    plainEdited (InProgress buf) =
      liftIO $ sender # setMarkedText (BS.unpack buf) 0 (BS.length buf)
    plainEdited NoHit = return ()

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

newSession :: HSKKController -> IO Session
newSession ctrl = return $ Session M.empty ctrl Hiragana

objc_implementation [ Typed 'pushKey2, Typed 'changeMode, Typed 'newSession ]
  [cunit|
@implementation HSKKController
- (id)initWithServer:(typename IMKServer *)server
            delegate:(id)delegate
              client:(id)inputClient
{
  self = [super initWithServer: server delegate: delegate client: inputClient];
  if (self != nil) {
    self.session = newSession(self);
  }
  return self;
}

-(void)setValue: (id)value forTag:(long)tag client:(id)sender
{
  changeMode(self.session, (typename NSString *)value);
}

- (typename BOOL) inputText:(typename NSString *)string
                        key:(long)keyCode
                  modifiers:(unsigned long)flags
                     client:(id)sender
{
   return pushKey2(self.session, sender, string, keyCode, flags);
}

@end
|]

-- objc_initialise = undefined

objc_emit
