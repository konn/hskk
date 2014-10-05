{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls                #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses   #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternGuards, QuasiQuotes    #-}
{-# LANGUAGE RankNTypes, RecursiveDo, StandaloneDeriving, TemplateHaskell #-}
{-# LANGUAGE TupleSections, TypeFamilies, TypeOperators, ViewPatterns     #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
module HSKKController ( objc_initialise ) where
import Constants
import KeyFlags
import Messaging

import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Arrow          ((>>>))
import           Control.Effect         (Effect, Lift, Row (..), State, Writer)
import           Control.Effect         (runEffect, runLift, runState)
import           Control.Effect         (runWriter, tell)
import           Control.Exception      (SomeException (..), handle)
import           Control.Lens           (isn't, makeLenses, makePrisms, noneOf)
import           Control.Lens           (to, traverse, use, uses, (%=), (%~))
import           Control.Lens           ((&), (.~), (<>=), (<>~), (^.), (^?))
import           Control.Lens           ((^?!), _Just, _last)
import           Control.Lens.Extras    (is)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Zipper         ((:>>), Top, focus, fromWithin)
import           Control.Zipper         (leftward, rightward, zipper)
import qualified Data.ByteString.Char8  as BS
import           Data.Char              (isControl, toUpper)
import           Data.IORef             (IORef, modifyIORef', newIORef)
import           Data.IORef             (readIORef, writeIORef)
import           Data.List              (find)
import qualified Data.Map               as M
import           Data.Monoid            (Monoid (..), (<>))
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import           Data.Tuple             (swap)
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

defineSelector
  newSelector { selector = "selectInputMode"
              , reciever = (''NSObject, "sender")
              , arguments = ["mode" :>>: ''String]
              , definition = [cexp| [(id)sender selectInputMode: mode]|]
              }

type StateMachine s = Effect (State s :+ Lift IO :+ Nil)

runSW :: Monoid w => s -> Effect (Writer w :+ State s :+ Nil) () -> (s, w)
runSW s0 act = swap $ runEffect $ runState s0 $ snd <$> runWriter act

type Client = NSObject

data ClientState = Composing { _pushCmd    :: SKKCommand -> IO [SKKResult]
                             , _clientMode :: ConvMode
                             , _idling     :: IORef Bool
                             }
                 | Selecting { _clientMode :: ConvMode
                             , _selection  :: Top :>> [[T.Text]] :>> [T.Text]
                             , _selGokan   :: T.Text
                             , _selOkuri   :: Maybe (Char, T.Text)
                             , _super      :: ClientState
                             }
                 | Registering { _pushCmd     :: SKKCommand -> IO [SKKResult]
                               , _clientMode  :: ConvMode
                               , _idling      :: IORef Bool
                               , _registerBuf :: T.Text
                               , _unknown     :: T.Text
                               , _clOkuri     :: Maybe (Char, T.Text)
                               , _super       :: ClientState
                               }
                 deriving (Typeable)

instance Show ClientState where
  showsPrec d (Composing _ mode _) =
    showParen (d > 10) $ showString "Composing " . showsPrec 11 mode
  showsPrec d (Registering _ mode _ buf unk ok sup) =
    showParen (d > 10) $
    showString "Registering " . showsPrec 11 mode . showChar ' ' .
    showsPrec 11 buf . showChar ' ' . showsPrec 11 unk . showChar ' ' .
    showsPrec 11 ok . showChar ' ' . showsPrec 11 sup
  showsPrec d (Selecting mode cur gok oku sup) =
    showParen (d > 10) $
    showString "Selecting " . showsPrec 11 mode . showChar ' ' .
    showsPrec 11 gok . showChar ' ' . showsPrec 11 oku .
    showsPrec 11 (cur ^. focus) . showChar ' ' . showsPrec 11 sup

makeLenses ''ClientState
makePrisms ''ClientState

data Session = Session { _clientMap :: M.Map Client ClientState
                       , _self      :: HSKKController
                       , _convMode  :: ConvMode
                       , _skkDic    :: IORef Dictionary
                       } deriving (Typeable)

makeLenses ''Session

objc_interface [cunit|
@interface HSKKController : IMKInputController
@property (assign) typename HsStablePtr session;
@end
|]

objc_typecheck

updateSession :: Session -> IO ()
updateSession sess = do
  let myself = sess ^. self
  $(objc ['sess :> ''Session, 'myself :> ''HSKKController] $
       void [cexp| myself.session = sess |])

changeMode :: Session -> String -> IO ()
changeMode sess str = maybe (return ()) (changeMode' sess) $ lookupMode str

changeMode' :: Session -> ConvMode -> IO ()
changeMode' sess kana = do
  updateSession $ sess & convMode .~ kana
  return ()

mapAccumEM' :: (s -> a -> IO (s, b)) -> s -> Event a -> SignalGen (Event b)
mapAccumEM' fun s0 e = mapAccumEM s0 ((liftIO .) . flip fun <$> e)

newPusher :: KanaTable -> ConvMode -> IORef Dictionary
          -> IO (SKKCommand -> IO [SKKResult], IORef Bool)
newPusher table kana dicR = do
  isIn <- newIORef True
  eev <- newExternalEvent
  step <- start $ do
    input <- externalE eev
    dicB <- externalB $ readIORef dicR
    ev <- skkConvE table kana dicB input
    return $ eventToBehavior (flattenE ev)
  let push inp = do
        triggerExternalEvent eev inp
        ans <- step
        writeIORef isIn $ maybe True isIdling $ ans ^? _last
        return  ans
  return (push, isIn)

withSession :: Session -> Effect (State Session :+ (Lift IO :+ Nil)) b -> IO b
withSession session talk = do
  (a, sess) <- runLift (runState session talk)
  updateSession sess
  return a

inputText :: Session -> NSObject -> String -> CLong -> CULong -> IO Bool
inputText sess0 client input keyCode flags =
  handle errorLogger $ withSession sess0 $ do
    let modifs = decodeModifiers flags
        key = decodeKeyboard keyCode
        shifted = all (compatible Shift) modifs && not (null modifs)
    cst <- getClientState client
    case cst of
      Selecting {} -> doSelection (head input) key modifs client cst
      _ -> do
        idle <- liftIO $ readIORef $ cst ^?! idling
        mode <- use convMode
        dic <- use skkDic
        if | null modifs && idle && input == "q" ->
              case mode of
               HankakuKatakana -> do
                 client # selectInputMode (modeToString Hiragana)
                 return True
               Hiragana -> do
                 client # selectInputMode (modeToString Katakana)
                 return True
               Katakana -> do
                 client # selectInputMode (modeToString Hiragana)
                 return True
               Ascii    -> pushKey client [Incoming 'q']
           | null modifs && input == "q" -> pushKey client [ToggleKana]
           | null modifs && idle && input == "l" ->
               case mode of
                 HankakuKatakana -> do
                   client # selectInputMode (modeToString Ascii)
                   return True
                 Hiragana -> do
                   client # selectInputMode (modeToString Ascii)
                   return True
                 Katakana -> do
                   client # selectInputMode (modeToString Ascii)
                   return True
                 Ascii    -> pushKey client [Incoming 'l']
           | not (isControl $ head input) && all isAlphabeticModifier modifs &&
                       (key & is (_Just . _Char)) ->
               pushKey client $ map Incoming input
           | modifs == [Control] && key == Just (Char 'g') -> do
               cst' <- maybe (do{ (push, ref) <- liftIO $ newPusher defKanaTable mode dic
                                ; return (Composing push mode ref) })
                             return (cst ^? super)
               clientMap %= M.insert client cst'
               client # setMarkedText "" 0 0
               nsLog "registeration canceled"
               return True
           | otherwise -> case key of
               Just Delete
                 | cst & isn't _Registering -> pushKey client [Backspace]
                 | otherwise -> do
                   clientMap %= M.insert client (cst & registerBuf %~ initT)
                   return True
               Just (isNewline -> True) ->
                 if (cst & is _Registering) && idle
                 then do
                   let parent = cst ^?! super
                       result = cst ^?! registerBuf
                       midashi = cst ^?! unknown
                       upd = insert (Input midashi (fst <$> cst ^?! clOkuri)) $ Candidate result ""
                   liftIO $ modifyIORef' dic upd
                   clientMap %= M.insert client parent
                   pushKey client [Refresh]
                   else pushKey client [Finish]
               Just Tab    -> pushKey client [Complete]
               Just Space | idle      -> pushKey client [Incoming ' ']
                          | otherwise -> pushKey client [Convert]
               Just (Char 'q') | all (compatible Control) modifs && not (null modifs) ->
                 if idle && mode /= Ascii
                 then client # selectInputMode (modeToString HankakuKatakana) >> return True
                 else pushKey client [ToggleHankaku]
               Just (JIS 'q') | all (compatible Control) modifs && not (null modifs) ->
                 if idle && mode /= Ascii
                 then client # selectInputMode (modeToString HankakuKatakana) >> return True
                 else pushKey client [ToggleHankaku]
               Just JisEisuu -> return True
               Just JisKana  -> return True
               Just (Char c)
                 | all (`elem` [Alternate, Shift]) modifs ->
                   let c' = if shifted then toUpper c else c
                   in pushKey client [Incoming c']
               Just (JIS c)
                 | all (`elem` [Alternate, Shift]) modifs ->
                   let c' = if shifted then toUpper c else c
                   in pushKey client [Incoming c']
               _ -> return False

errorLogger :: MonadIO m => SomeException -> m Bool
errorLogger (SomeException exc) = do
  nsLog $ "***Exception: " ++ show exc
  return False

initT :: T.Text -> T.Text
initT "" = ""
initT t  = T.init t

doSelection :: Char -> Maybe Keyboard -> [Modifier]
            -> NSObject -> ClientState -> StateMachine Session Bool
doSelection ch key modifs sender cst =
  let z = cst ^?! selection
      body = cst ^?! selGokan
      mode = cst ^. clientMode
      mok  = cst ^?! selOkuri
      parent = cst ^?! super
      cur = z ^. focus
  in if | maybe False isNewline key && null modifs -> do
            finishSelection sender cst (head cur)
        | Just Space == key && null modifs -> do
           case z & rightward of
             Just pg -> do
               clientMap %= M.insert sender (cst & selection .~ pg)
               let msg = showPage mok (pg ^. focus)
               sender # setMarkedText (T.unpack msg) 0 (T.length msg)
             Nothing -> startRegisteration sender cst body mok
           return True
        | Just Delete == key || null modifs && ch == 'x' -> do
           case z & leftward of
             Just pg -> do
               clientMap %= M.insert sender (cst & selection .~ pg)
               let msg = showPage mok (pg ^. focus)
               sender # setMarkedText (T.unpack msg) 0 (T.length msg)
             Nothing -> do
               clientMap %= M.insert sender parent
               liftIO $ edited sender $ maybe (Converting body) (Okuri body . snd) mok
           return True
        | null modifs, Just t <- defCSelector ch cur -> do
            finishSelection sender cst t
        | [a] <- cur, null modifs -> do
            _ <- finishSelection sender cst a
            pushKey sender [Incoming ch]
        | otherwise -> return True

finishSelection :: NSObject
                -> ClientState
                -> T.Text
                -> StateMachine Session Bool
finishSelection sender (Selecting _ _ _ mok parent@Registering{}) txt = do
  clientMap %= M.insert sender (parent & registerBuf <>~ (txt <> maybe "" snd mok))
  displayReg sender
  return True
finishSelection sender (Selecting _ _ _ mok parent) txt = do
  sender # setMarkedText "" 0 0
  sender # insertText (T.unpack $ txt <> maybe "" snd mok)
  clientMap %= M.insert sender parent
  pushKey sender [Refresh]
finishSelection _ cst _ = error $ "finishSelection: " ++ show cst

showPage ::  Maybe (Char, T.Text) -> [T.Text] -> T.Text
showPage mok [a]   = "▼" <> a <> maybe "" (T.cons '*' . snd) mok
showPage mok cands =
  T.concat ["[候補: "
           , T.intercalate " / " $ zipWith
             (\a b -> T.singleton a <> ": " <> b)
             "asdfjkl"  cands
           , "]", maybe "" (T.cons '*' . snd) mok]

isNewline :: Keyboard -> Bool
isNewline Return = True
isNewline Enter  = True
isNewline _      = False

pushKey :: NSObject -> [SKKCommand] -> StateMachine Session Bool
pushKey client input = do
  cst <- getClientState client
  ans <- concat <$> mapM (liftIO . (cst ^?! pushCmd)) input
  let accepted = all (noneOf (_Idle . traverse) (is _NoHit)) ans
  if cst & is _Registering
    then do
    let (cst'0, buf) = runSW cst $ mapM_ extractTxt $ filter (isn't _ConvFound) ans
    clientMap %= M.insert client cst'0
    displayReg client
    else mapM_ (liftIO . edited client) $ filter (isn't _ConvFound) ans
  if | Just (ConvNotFound mid mok) <- find (is _ConvNotFound) ans -> do
        cst' <- uses clientMap (M.! client)
        startRegisteration client cst' mid mok
     | Just (ConvFound body mokuri cands) <- find (is _ConvFound) ans -> do
        let css  = defPager cands
            cst' = Selecting (cst ^. clientMode) (zipper css & fromWithin traverse) body mokuri cst
            msg  = showPage mokuri $ head css
        clientMap %= M.insert client cst'
        client # setMarkedText (T.unpack msg) 0 (T.length msg)
     | otherwise ->  return ()
  return accepted

startRegisteration :: NSObject
                   -> ClientState
                   -> T.Text
                   -> Maybe (Char, T.Text)
                   -> StateMachine Session ()
startRegisteration client old body mok = do
  liftIO $ nsLog $ "start registration for: " <> T.unpack (prettyOkuri body mok)
  mode <- use convMode
  dic  <- use skkDic
  (push, isIn) <- liftIO $ newPusher defKanaTable mode dic
  let cst' = Registering push (old ^. clientMode) isIn "" body mok old
  clientMap %= M.insert client cst'
  displayReg client

displayReg :: Client -> StateMachine Session ()
displayReg sender = do
  cst <- getClientState sender
  let body = cst ^?! unknown
      mok  = cst ^?! clOkuri
      buf  = cst ^?! registerBuf
      msg  = "[単語登録：" <> prettyOkuri body mok <> "]" <> buf
  sender # setMarkedText "" 0 0
  sender # setMarkedText (T.unpack msg) 0 (T.length msg)
  return ()

prettyOkuri :: T.Text -> Maybe (Char, T.Text) -> T.Text
prettyOkuri t Nothing = t
prettyOkuri t (Just (_, c)) = t <> "*" <> c

extractTxt :: SKKResult -> Effect (Writer T.Text :+ State ClientState :+ Nil) ()
extractTxt (Idle eds) = mapM_ plainExtractTxt eds
extractTxt (Finished t) = do
  tell t
  registerBuf <>= t
extractTxt (Okuri body o) = do
  let msg = "▽" <> body <> "*" <> o
  tell msg
extractTxt (Converting str) = do
  let ans = "▽" <> str
  tell ans
extractTxt r = error $ "Unhandled input: " ++ show r

plainExtractTxt :: KanaResult -> Effect (Writer T.Text :+ State ClientState :+ Nil) ()
plainExtractTxt (Converted text) = do
  tell text
  registerBuf <>= text
plainExtractTxt (InProgress buf) = tell $ T.decodeUtf8 buf
plainExtractTxt NoHit = return ()

edited :: (NSObjectClass :> b) => Object b -> SKKResult -> IO ()
edited client (Idle eds) = mapM_ (plainEdited client) eds
edited client (ConvNotFound _ _) = do
  client # setMarkedText "" 0 0
edited client (Finished t) = do
  client # insertText (T.unpack t)
edited client (Okuri body o) = do
  let msg = "▽" <> body <> "*" <> o
  client # setMarkedText (T.unpack msg) 0 (T.length msg)
edited client (Converting str) = do
  let ans = "▽" <> str
  client # setMarkedText (T.unpack ans) 0 (T.length ans)
edited _ conv = nsLog $ "unhandled result: " ++ show conv

plainEdited :: (NSObjectClass :> b, MonadIO m)
            => Object b -> KanaResult -> m ()
plainEdited client (Converted text) =
  client # insertText (T.unpack text)
plainEdited client (InProgress buf) =
  client # setMarkedText (BS.unpack buf) 0 (BS.length buf)
plainEdited _ NoHit = return ()

commit :: Session -> NSObject -> IO ()
commit sess sender = withSession sess $ do
  _ <- pushKey sender [Finish]
  return ()

getCurrentMode :: Session -> String
getCurrentMode sess = sess ^. convMode . to modeToString

getClientState :: Client -> StateMachine Session ClientState
getClientState sender = do
  mst <- uses clientMap (M.lookup sender)
  kana <- use convMode
  case mst of
    Just st | kana == st ^. clientMode -> return st
    _ -> do
      dic <- use skkDic
      (push, isIn) <- liftIO $ newPusher defKanaTable kana dic
      let st = maybe (Composing push kana isIn)
               (pushCmd .~ push >>> clientMode .~ kana >>> idling .~ isIn)
               mst
      clientMap %= M.insert sender st
      return st

nsLog :: MonadIO m => String -> m ()
nsLog msg = liftIO $(objc ['msg :> ''String] $ void [cexp| NSLog(@"%@", msg) |])

newSession :: HSKKController -> IO Session
newSession ctrl = Session M.empty ctrl Hiragana <$> newIORef sDictionary

objc_implementation [ Typed 'inputText, Typed 'changeMode, Typed 'commit
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
   return inputText(self.session, sender, string, keyCode, flags);
}

- (void)commitComposition: (id)sender
{
  commit(self.session, sender);
}

- (id)valueForTag:(long) tag client:(id)sender
{
  return (id)getCurrentMode(self.session);
}

@end
|]

-- objc_initialise = undefined

objc_emit
