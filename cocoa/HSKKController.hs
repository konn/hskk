{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls              #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes, RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TupleSections         #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
module HSKKController ( objc_initialise ) where
import KeyFlags
import Messaging

import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Lens           (noneOf)
import           Control.Lens           (traverse)
import           Control.Lens.Extras    (is)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8  as BS
import           Data.Char              (ord)
import           Data.Char              (isControl)
import           Data.Char              (toUpper)
import qualified Data.Map               as M
import           Data.Maybe             (fromMaybe)
import           Data.Monoid            ((<>))
import           Data.Monoid            (Any (..))
import           Data.Monoid            (mconcat)
import qualified Data.Text              as T
import           Data.Typeable          (Typeable)
import           Data.Word              (Word)
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

data Session = Session { _dict :: M.Map Client ClientState
                       , _self :: HSKKController
                       } deriving (Typeable)

data KeyResult = KeyResult { curSession   :: Session
                           , stateChanged :: Bool
                           } deriving (Typeable)

newKeyResult :: Session -> Bool -> KeyResult
newKeyResult = KeyResult

objc_record "HSKK" "KeyResult" ''KeyResult [Typed 'newKeyResult]
  [[objcprop| @property (readonly) typename HsStablePtr session; |] --> 'curSession
  ,[objcprop| @property (readonly) typename BOOL changed; |] --> 'stateChanged
  ]
  [objcifdecls| + (instancetype) keyResultWithSession: (typename HsStablePtr) session
                                              changed: (typename BOOL) changed; |]
  [objcimdecls|
+ (instancetype) keyResultWithSession: (typename HsStablePtr) session
  changed: (typename BOOL) changed
  {
    return [[HSKKKeyResult alloc] initWithKeyResultHsPtr: newKeyResult(session, changed)];
  }
  |]

defineClass "HSKKKeyResult" (Just ''NSObject)
idMarshaller ''HSKKKeyResult

demarshalKeyResult :: HSKKKeyResult -> IO KeyResult
demarshalKeyResult hsk
  = $(objc ['hsk :> Class ''HSKKKeyResult] $
            ''KeyResult <: [cexp| newKeyResult(hsk.session, hsk.changed) |])

marshalKeyResult :: KeyResult -> IO HSKKKeyResult
marshalKeyResult hsk
  = $(objc ['hsk :> ''KeyResult] $
            Class ''HSKKKeyResult <: [cexp| [[HSKKKeyResult alloc] initWithKeyResultHsPtr: hsk] |])

objc_marshaller 'demarshalKeyResult 'marshalKeyResult

objc_interface [cunit|
@interface HSKKController : IMKInputController
@property (assign) typename HsStablePtr session;
@end
|]

objc_typecheck

changeMode :: Session -> NSObject -> String -> IO ()
changeMode sess obj str =
  return ()

pushKey :: Session -> NSObject -> [SKKCommand] -> IO Bool
pushKey session client input = do
  (upd, dict') <- getClientState client $ _dict session
  accepted <- and <$> mapM (pushCmd upd) input
  let myself = _self session
      sess   = session { _dict = dict' }
  $(objc ['myself :> ''HSKKController, 'sess :> ''Session] $
          void $ [cexp| myself.session = sess |])
  return accepted

pushKey2 :: Session -> NSObject -> String -> CLong -> CULong -> IO Bool
pushKey2 session client input key flags = do
  let modifs = decodeModifiers flags
      shifted = all (compatible Shift) modifs
  nsLog $ "input: " ++ show (input, decodeKeyboard key, decodeModifiers flags)
  if not (isControl $ head input) && all isAlphabeticModifier modifs
    then pushKey session client $ map Incoming input
    else case decodeKeyboard key of
    Just Delete -> pushKey session client [Incoming '\DEL']
    Just Enter  -> pushKey session client [Incoming '\n']
    Just Return -> pushKey session client [Incoming '\n']
    Just Tab    -> pushKey session client [Incoming '\t']
    Just Space  -> pushKey session client [Incoming ' ']
    Just (Char 'q') | all (compatible Control) modifs ->
      pushKey session client [ToggleHankaku]
    Just (JIS 'q') | all (compatible Control) modifs ->
      pushKey session client [ToggleHankaku]
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

getClientState :: Client -> M.Map Client ClientState
           -> IO (ClientState, M.Map Client ClientState)
getClientState sender dic
  | Just upd <- M.lookup sender dic = return (upd, dic)
  | otherwise = do
    eev <- newExternalEvent
    step <- start $ do
      input <- externalE eev
      ev <- defSKKConvE input
      _ <- generatorE $ mapM edited <$> ev
      return $ all (noneOf (_Idle . traverse) (is _NoHit)) <$> eventToBehavior (flattenE ev)
    let push inp = triggerExternalEvent eev inp >> step
        st = ClientState push Hiragana
    return $ (st, M.insert sender st dic)
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
newSession ctrl = return $ Session M.empty ctrl

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

-(void)setValue: (id)value forTag:(unsigned long)tag client:(id)sender
{
  changeMode(self.session, sender, (typename NSString *)value);
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
