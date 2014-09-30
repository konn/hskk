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
import           Control.Lens           (makeLenses, noneOf, to, traverse, (%~))
import           Control.Lens           ((&), (.~), (^.), _Just, _last)
import           Control.Lens           ((^?))
import           Control.Lens.Extras    (is)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS
import           Data.Char              (isControl, toUpper)
import           Data.IORef             (IORef, newIORef, writeIORef)
import           Data.IORef             (readIORef)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            (Last (..), mconcat, (<>))
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

{-

marshalConvMode :: ConvMode -> IO NSString
marshalConvMode Hiragana = $(objc [Typed 'hiraganaModeKey] $ ''NSString <: [cexp| hiraganaModeKey |])
marshalConvMode Katakana = $(objc [Typed 'katakanaModeKey] $ ''NSString <: [cexp| katakanaModeKey |])
marshalConvMode HankakuKatakana = $(objc [Typed 'hankakuModeKey] $ ''NSString <: [cexp| hankakuModeKey |])
marshalConvMode Ascii = $(objc [Typed 'asciiModeKey] $ ''NSString <: [cexp| asciiModeKey |])

demarhsalConvMode :: NSString -> IO ConvMode
demarhsalConvMode str = $(objc [Typed 'lookupMode_, 'str :> ''NSString] $
                          ''ConvMode <: [cexp| lookupMode_(str) |])

objc_marshaller 'marshalConvMode 'demarhsalConvMode

-}


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

defineSelector
  newSelector { selector = "selectInputMode"
              , reciever = (''NSObject, "sender")
              , arguments = ["mode" :>>: ''String]
              , definition = [cexp| [(id)sender selectInputMode: mode]|]
              }

type Client = NSObject

data ClientState  = ClientState { pushCmd :: SKKCommand -> IO Bool
                                , mode    :: ConvMode
                                , idling  :: IORef Bool
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
changeMode sess str = maybe (return ()) (changeMode' sess) $ lookupMode str

changeMode' :: Session -> ConvMode -> IO ()
changeMode' sess kana = do
  updateSession $ sess & convMode .~ kana
  return ()

pushKey :: Session -> NSObject -> [SKKCommand] -> IO Bool
pushKey session client input = do
  cst <- getClientState client session
  accepted <- and <$> mapM (pushCmd cst) input
  return accepted

pushKey2 :: Session -> NSObject -> String -> CLong -> CULong -> IO Bool
pushKey2 session client input keyCode flags = do
  cst <- getClientState client session
  idle <- readIORef $ idling cst
  let modifs = decodeModifiers flags
      key = decodeKeyboard keyCode
      shifted = all (compatible Shift) modifs
      mode = session ^. convMode
  if null modifs && idle && input == "q"
  then case mode of
        HankakuKatakana -> client # selectInputMode (modeToString Hiragana) >> return True
        Hiragana -> client # selectInputMode (modeToString Katakana) >> return True
        Katakana -> client # selectInputMode (modeToString Hiragana) >> return True
        Ascii    -> pushKey session client [Incoming 'q']
  else if null modifs && idle && input == "l"
  then case mode of
        HankakuKatakana -> client # selectInputMode (modeToString Ascii) >> return True
        Hiragana -> client # selectInputMode (modeToString Ascii) >> return True
        Katakana -> client # selectInputMode (modeToString Ascii) >> return True
        Ascii    -> pushKey session client [Incoming 'l']
  else if not (isControl $ head input) && all isAlphabeticModifier modifs &&
     (key & is (_Just . _Char))
    then pushKey session client $ map Incoming input
    else case key of
    Just Delete -> pushKey session client [Incoming '\DEL']
    Just Enter  -> pushKey session client [Incoming '\n']
    Just Return -> pushKey session client [Incoming '\n']
    Just Tab    -> pushKey session client [Incoming '\t']
    Just Space  -> pushKey session client [Incoming ' ']
    Just (Char 'q') | all (compatible Control) modifs && not (null modifs) ->
      if idle && mode /= Ascii
      then client # selectInputMode (modeToString HankakuKatakana) >> return True
      else pushKey session client [ToggleHankaku]
    Just (JIS 'q') | all (compatible Control) modifs && not (null modifs) ->
      if idle && mode /= Ascii
      then client # selectInputMode (modeToString HankakuKatakana) >> return True
      else pushKey session client [ToggleHankaku]
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

getCurrentMode :: Session -> String
getCurrentMode sess = sess ^. convMode . to modeToString

getClientState :: Client -> Session -> IO ClientState
getClientState sender sess0
  | Just st <- M.lookup sender $ sess0 ^. dict
  , sess0 ^. convMode == mode st = return st
  | otherwise = do
    let kana = sess0 ^. convMode
    isIn <- newIORef True
    eev <- newExternalEvent
    step <- start $ do
      input <- externalE eev
      ev <- skkConvE defKanaTable kana sDictionary defPager defCSelector input
      _ <- generatorE $ mapM edited <$> ev
      return $ eventToBehavior (flattenE ev)
    let push inp = do
          triggerExternalEvent eev inp
          ans <- step
          writeIORef isIn $ maybe True isIdling $ ans ^? _last
          return $ all (noneOf (_Idle . traverse) (is _NoHit)) ans
        st = ClientState push kana isIn
        sess = sess0 & dict %~ M.insert sender st
    updateSession sess
    return st
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

objc_implementation [ Typed 'pushKey2, Typed 'changeMode
                    , Typed 'newSession, Typed 'getCurrentMode ]
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

- (id)valueForTag:(long) tag client:(id)sender
{
  return (id)getCurrentMode(self.session);
}

@end
|]

-- objc_initialise = undefined

objc_emit
