{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls                 #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards       #-}
{-# LANGUAGE QuasiQuotes, RecursiveDo, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TypeFamilies, TypeOperators                                   #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
module HSKKController ( objc_initialise ) where
import Messaging

import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Lens           (isn't)
import           Control.Lens.Extras    (is)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Map               as M
import           Data.Monoid            (mconcat)
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

type Client = NSObject

data Session = Session { _dict :: M.Map Client (Char -> IO Bool)
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

objc_typecheck

pushKey :: Session -> NSObject -> String -> IO KeyResult
pushKey session client input = do
  (upd, dict') <- updaterFor client $ _dict session
  accepted <- and <$> mapM upd input
  return $ KeyResult session { _dict = dict' } accepted

updaterFor :: Client -> M.Map Client (Char -> IO Bool)
           -> IO (Char -> IO Bool, M.Map Client (Char -> IO Bool))
updaterFor sender dic
  | Just upd <- M.lookup sender dic = return (upd, dic)
  | otherwise = do
    eev <- newExternalEvent
    step <- start $ do
      input <- externalE eev
      ev <- romanConv defKanaTable Katakana input
      _ <- generatorE $ liftIO . nsLog . ("input: " ++) . show <$> ev
      _ <- generatorE $ liftIO . edited . mconcat <$> ev
      return $ isn't _NoHit . head <$> eventToBehavior (mconcat <$> ev)
    let push inp = triggerExternalEvent eev inp >> step
    return $ (push, M.insert sender push dic)
  where
    edited (Converted text) = sender # insertText (T.unpack text)
    edited _ = return ()

nsLog :: String -> IO ()
nsLog msg = $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

newSession :: HSKKController -> IO Session
newSession ctrl = return $ Session M.empty ctrl

objc_interface [cunit|
@interface HSKKController : IMKInputController
@property (assign) typename HsStablePtr session;
@end
|]

objc_implementation [ Typed 'pushKey, Typed 'newSession ]
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
- (typename BOOL)inputText:(typename NSString *)string client:(id)sender
{
    typename HSKKKeyResult *tuple =
      [[HSKKKeyResult alloc] initWithKeyResultHsPtr: pushKey(self.session, sender, string)];
    self.session = tuple.session;
    return tuple.changed;
}
@end
|]

--objc_initialise = undefined

objc_emit
