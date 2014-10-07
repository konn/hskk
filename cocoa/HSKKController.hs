{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase      #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, OverloadedStrings #-}
{-# LANGUAGE PatternGuards, QuasiQuotes, RankNTypes, RecursiveDo  #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TupleSections   #-}
{-# LANGUAGE TypeFamilies, TypeOperators, ViewPatterns            #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
module HSKKController ( objc_initialise ) where
import Constants
import KeyFlags
import Messaging

import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Applicative    ((<*))
import           Control.Effect         (Coroutine, Effect, EffectLift)
import           Control.Effect         (EffectState, Iterator (..), Lift)
import           Control.Effect         (Row (..), State, Writer, extend)
import           Control.Effect         (runCoroutine, runEffect, runLift)
import           Control.Effect         (runState, runWriter, suspend, tell)
import           Control.Effect         (get)
import           Control.Effect         (put)
import           Control.Effect         (EffectCoroutine)
import           Control.Exception      (SomeException (..), handle)
import           Control.Lens           (isn't, makeLenses, makePrisms, noneOf)
import           Control.Lens           (to, traverse, use, uses, (%=), (%~))
import           Control.Lens           ((&), (.~), (<%~), (<>=), (^.), (^?))
import           Control.Lens           ((<<>~), (^?!), _Just, _last)
import           Control.Lens.Extras    (is)
import           Control.Monad          (forM, liftM)
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

type StateMachine s = Effect (Coroutine Waiting (Maybe T.Text) :+ State s :+ Lift IO :+ Nil)
data Waiting = Registration T.Text (Maybe T.Text)
             | Selection T.Text (Maybe (Char, T.Text)) [T.Text]
type Continuation = Maybe T.Text ->
                    Effect (State Session :+ Lift IO :+ Nil)
                    (Iterator Waiting (Maybe T.Text) (State Session :+ Lift IO :+ Nil) Bool)

runSW :: Monoid w => s -> Effect (Writer w :+ State s :+ Nil) () -> (s, w)
runSW s0 act = swap $ runEffect $ runState s0 $ snd <$> runWriter act

data MarkedText = Marked T.Text | Normal T.Text
                deriving (Read, Show, Eq, Ord, Typeable)


type Client = NSObject

data Session = Session { _clientMap :: M.Map Client ClientState
                       , _self      :: HSKKController
                       , _convMode  :: ConvMode
                       , _skkDic    :: IORef Dictionary
                       } deriving (Typeable)

data ClientState = Composing { _pushCmd    :: SKKCommand -> IO [SKKResult]
                             , _clientMode :: ConvMode
                             , _idling     :: IORef Bool
                             }
                 | Selecting { _clientMode :: ConvMode
                             , _selection  :: Top :>> [[T.Text]] :>> [T.Text]
                             , _selGokan   :: T.Text
                             , _selOkuri   :: Maybe (Char, T.Text)
                             , _continue   :: Continuation
                             }
                 | Registering { _pushCmd       :: SKKCommand -> IO [SKKResult]
                               , _clientMode    :: ConvMode
                               , _idling        :: IORef Bool
                               , _registerBuf   :: T.Text
                               , _registerGokan :: T.Text
                               , _registerOkuri :: Maybe T.Text
                               , _continue      :: Continuation
                               }
                 deriving (Typeable)

makeLenses ''Session
makePrisms ''MarkedText

formatMarkedText :: (NSObjectClass :> b, MonadIO m) => Object b -> MarkedText -> m ()
formatMarkedText sender (Marked txt)
  = sender # setMarkedText (T.unpack txt) 0 (T.length txt)
formatMarkedText sender (Normal txt)
  = sender # insertText (T.unpack txt)

instance Show ClientState where
  showsPrec d (Composing _ mode _) =
    showParen (d > 10) $ showString "Composing " . showsPrec 11 mode
  showsPrec d (Registering _ mode _ buf gok oku _) =
    showParen (d > 10) $
    showString "Registering " . showsPrec 11 mode . showChar ' ' .
    showsPrec 11 buf . showChar ' ' . showsPrec 11 gok .
    showChar ' ' . showsPrec 11 oku
  showsPrec d (Selecting mode cur gok oku _) =
    showParen (d > 10) $
    showString "Selecting " . showsPrec 11 mode . showChar ' ' .
    showsPrec 11 gok . showChar ' ' . showsPrec 11 oku .
    showsPrec 11 (cur ^. focus) . showChar ' '

makeLenses ''ClientState
makePrisms ''ClientState

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

newClientState :: (EffectState Session l, EffectLift IO l) =>Effect l ClientState
newClientState = do
  dic <- use skkDic
  mode <- use convMode
  (push, ref) <- liftIO $ newPusher defKanaTable mode dic
  return $ Composing push mode ref

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

withSession :: Session -> Effect (State Session ':+ (Lift IO ':+ 'Nil)) b -> IO b
withSession session talk = do
  (a, sess) <- runLift (runState session talk)
  updateSession sess
  return a

inputText :: Session -> NSObject -> String -> CLong -> CULong -> IO Bool
inputText sess0 client input keyCode flags =
  handle errorLogger $ withSession sess0 $
  handleIt client =<< runCoroutine body
  where
    body = do
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
                         (key & is (_Just . _Char)) -> do
                 pushKey client $ map Incoming input
             | modifs == [Control] && key == Just (Char 'g') ->
                 case cst ^? continue of
                   Nothing -> do
                     (push, ref) <- liftIO $ newPusher defKanaTable mode dic
                     let cst' = Composing push mode ref
                     clientMap %= M.insert client cst'
                     client # setMarkedText "" 0 0
                     nsLog "registeration canceled"
                     return True
                   Just p -> do
                     nsLog $ "continuing with Nothing"
                     extend $ handleIt client =<< p Nothing
             | otherwise -> case key of
                 Just Delete
                   | cst & isn't _Registering -> pushKey client [Backspace]
                   | otherwise -> do
                     let (msg, cst') = cst & registerBuf <%~ initT
                     clientMap %= M.insert client cst'
                     extend $ relay client [Normal msg]
                     return True
                 Just (isNewline -> True) ->
                   if (cst & is _Registering) && idle
                   then do
                     let push = cst ^?! continue
                     nsLog "continuing with Nothing"
                     extend $ handleIt client =<< push Nothing
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
                     in  pushKey client [Incoming c']
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

doSelection :: Char -> Maybe Keyboard -> [a] -> Client -> ClientState
            -> StateMachine Session Bool
doSelection ch key modifs sender cst@(Selecting mode z body mok push) =
  let cur = z ^. focus
  in if | maybe False isNewline key && null modifs -> do
            extend $ finishSelection sender cst (head cur)
        | Just Space == key && null modifs ->
           case z & rightward of
             Just pg -> do
               clientMap %= M.insert sender (cst & selection .~ pg)
               let msg = showPage mok (pg ^. focus)
               extend $ relay sender [Marked msg]
             Nothing -> startRegistration sender body mok
        | Just Delete == key || null modifs && ch == 'x' -> do
           case z & leftward of
             Just pg -> do
               clientMap %= M.insert sender (cst & selection .~ pg)
               let msg = showPage mok (pg ^. focus)
               extend $ relay sender [Marked msg]
             Nothing -> do
               nsLog "giving up registering"
               extend $ handleIt sender =<< push Nothing
        | null modifs, Just t <- defCSelector ch cur -> do
            extend $ finishSelection sender cst t
        | [a] <- cur, null modifs -> do
            _ <- extend $ finishSelection sender cst a
            nsLog . ("current state: "<>) . show =<< getClientState sender
            nsLog "fnished."
            pushKey sender [Incoming ch]
        | otherwise -> return True


finishSelection :: Client -> ClientState -> T.Text -> Effect (State Session :+ Lift IO :+ Nil) Bool
finishSelection sender (Selecting _ _ _ _ cont) "" = do
  nsLog $ "finish selection with Nothing"
  handleIt sender =<< cont Nothing
finishSelection sender (Selecting _ _ _ mok cont) txt = do
  nsLog $ "finish selection with " ++ T.unpack txt
  handleIt sender =<< cont (Just $ txt <> maybe "" snd mok)
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

suspend' :: (EffectState s l, EffectCoroutine i b l) => i -> Effect l b
suspend' a = do
  old <- get
  suspend a <* put old

pushKey :: Client -> [SKKCommand] -> Effect (Coroutine Waiting (Maybe T.Text) :+ State Session :+ Lift IO :+ Nil) Bool
pushKey client input = do
  cst <- getClientState client
  ans <- concat <$> mapM (liftIO . (cst ^?! pushCmd)) input
  let accepted = all (noneOf (_Idle . traverse) (is _NoHit)) ans
  let buf = snd $ runSW cst $ mapM_ extractTxt ans
  if | Just (ConvNotFound mid mok) <- find (is _ConvNotFound) ans -> do
        startRegistration client mid mok
     | Just (ConvFound body mokuri cands) <- find (is _ConvFound) ans -> do
        let css  = defPager cands
            msg  = showPage mokuri $ head css
        _ <- extend $ relay client [Marked msg]
        nsLog $ "Waiting for selection..."
        mans <- suspend' $ Selection body mokuri cands
        case mans of
          Just st -> do
            clientMap %= M.insert client cst
            extend $ relay client [Normal st]
          Nothing  -> return True
     | otherwise -> extend (relay client buf) >> return accepted

startRegistration :: NSObject -> T.Text -> Maybe (Char, T.Text)
                   -> Effect (Coroutine Waiting (Maybe T.Text) :+ State Session :+ Lift IO :+ Nil) Bool
startRegistration client mid mok = do
  let msg = "[単語登録："  <> prettyOkuri mid (snd <$> mok) <> "]"
  _ <- extend $ relay client [Marked msg]
  nsLog "Waiting for registration..."
  mans <- suspend' $ Registration mid (snd <$> mok)
  nsLog $ "Register: " ++ show mans
  case mans of
    Just str | not (T.null str) -> do
      dic <- use skkDic
      liftIO $ modifyIORef' dic $ insert (Input mid (fst <$> mok)) $ Candidate str ""
      extend $ relay client [Normal str]
    _ -> extend $ relay client []

relay :: Client -> [MarkedText] -> Effect (State Session ':+ (Lift IO ':+ 'Nil)) Bool
relay sender [] = ((^? continue) <$> getClientState sender) >>= \case
  Nothing -> do
    ns <- newClientState
    clientMap %= M.insert sender ns
    return False
  Just cont -> do
    nsLog "relay: continuing with Nothing"
    handleIt sender =<< cont Nothing
relay sender mts = do
  cst0 <- getClientState sender
  case cst0 of
    Composing {} -> liftM or $ forM (catMText mts) $ \case
      Marked txt -> do
        sender # setMarkedText (T.unpack txt) 0 (T.length txt)
        return True
      Normal txt -> do
        sender # insertText (T.unpack txt)
        st <- newClientState
        clientMap %= M.insert sender st
        return True
    Registering _ _ _ _ body mok _ -> do
      let (buf, cst') = cst0 & registerBuf <<>~ T.concat [txt | Normal txt <- mts]
          msg = T.concat ["[単語登録：", prettyOkuri body mok, "]", buf]
                <> T.concat [txt | Marked txt <- mts]
      sender # setMarkedText (T.unpack msg) 0 (T.length msg)
      return True
    Selecting {} -> return True

handleIt :: (EffectState Session l, EffectLift IO l)
         => Client -> Iterator Waiting (Maybe T.Text) (State Session :+ (Lift IO :+ Nil)) Bool
         -> Effect l Bool
handleIt _ (Done b) = return b
handleIt sender (Next cont (Registration body mok)) = do
  mode <- use convMode
  dic  <- use skkDic
  (push, ref) <- liftIO $ newPusher defKanaTable mode dic
  let cst' = Registering push mode ref "" body mok cont
  clientMap %= M.insert sender cst'
  return True
handleIt sender (Next cont (Selection body mok cands)) = do
  mode <- use convMode
  let cst' = Selecting mode (zipper (defPager cands) & fromWithin traverse) body mok cont
  clientMap %= M.insert sender cst'
  return True

unmark :: MarkedText -> T.Text
unmark (Marked t) = t
unmark (Normal t) = t

catMText :: [MarkedText] -> [MarkedText]
catMText (Marked txt : Marked txt' : ts) = catMText (Marked (txt <> txt') : ts)
catMText (t : ts) = t : catMText ts
catMText [] = []

displayReg :: (EffectLift IO l, EffectState Session l) => Client -> Effect l ()
displayReg sender = do
  cst <- getClientState sender
  let body = cst ^?! registerGokan
      mok  = cst ^?! registerOkuri
      buf  = cst ^?! registerBuf
      msg  = "[単語登録：" <> prettyOkuri body mok <> "]" <> buf
  sender # setMarkedText "" 0 0
  sender # setMarkedText (T.unpack msg) 0 (T.length msg)
  return ()

prettyOkuri :: T.Text -> Maybe T.Text -> T.Text
prettyOkuri t Nothing = t
prettyOkuri t (Just c) = t <> "*" <> c

extractTxt :: SKKResult -> Effect (Writer [MarkedText] :+ State ClientState :+ Nil) ()
extractTxt (Idle eds) = mapM_ plainExtractTxt eds
extractTxt (Finished t) = do
  tell [Normal t]
  registerBuf <>= t
extractTxt (Okuri body o) = do
  let msg = "▽" <> body <> "*" <> o
  tell [Marked msg]
extractTxt (Converting str) = do
  let ans = "▽" <> str
  tell [Marked ans]
extractTxt _ = return ()

plainExtractTxt :: KanaResult -> Effect (Writer [MarkedText] :+ State ClientState :+ Nil) ()
plainExtractTxt (Converted text) = do
  tell [Normal text]
  registerBuf <>= text
plainExtractTxt (InProgress buf) = tell [Marked $ T.decodeUtf8 buf]
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
  sender # setMarkedText "" 0 0
  nsLog "commiting edition"
  _ <- handleIt sender =<< runCoroutine (pushKey sender [Finish])
  return ()

getCurrentMode :: Session -> String
getCurrentMode sess = sess ^. convMode . to modeToString

getClientState :: (EffectState Session l, EffectLift IO l)
               => Client -> Effect l ClientState
getClientState sender = do
  mst <- uses clientMap (M.lookup sender)
  kana <- use convMode
  case mst of
    Just st | kana == st ^. clientMode -> return st
    _ -> do
      st <- newClientState
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
