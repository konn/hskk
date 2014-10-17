{-# LANGUAGE DataKinds, DeriveDataTypeable, EmptyDataDecls                #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses, MultiWayIf, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, PatternGuards, QuasiQuotes, RankNTypes    #-}
{-# LANGUAGE RecursiveDo, StandaloneDeriving, TemplateHaskell             #-}
{-# LANGUAGE TupleSections, TypeFamilies, TypeOperators, ViewPatterns     #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-orphans #-}
module HSKKController ( objc_initialise ) where
import Constants
import KeyFlags
import Messaging

import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Arrow          (first)
import           Control.Effect
import           Control.Exception      (SomeException (..), handle)
import           Control.Lens           (makeLenses, makePrisms, noneOf)
import           Control.Lens           (to, traverse, use, (%~), _head)
import           Control.Lens           ((&), (.=), (.~), (<>=))
import           Control.Lens           ((^.), (^?), (^?!), _Just, _last)
import           Control.Lens           ((%=), (<<>=))
import           Control.Lens           ((<&>))
import           Control.Lens           (isn't)
import           Control.Lens.Extras    (is)
import           Control.Monad          (forM, liftM, (<=<))
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Zipper         ((:>>), Top, focus, fromWithin)
import           Control.Zipper         (leftward, rightward, zipper)
import           Data.Char              (isControl, toUpper)
import           Data.Char              (toLower)
import           Data.IORef             (IORef, modifyIORef', newIORef)
import           Data.IORef             (readIORef, writeIORef)
import           Data.List              (find)
import qualified Data.Map               as M
import           Data.Monoid            ((<>))
import           Data.Reflection        (Given (..), give)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
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
              , arguments = ["string" :>>: ''String, "beg" :>>: ''Int, "len" :>>: ''Int]
              , definition = [cexp| [(id)sender setMarkedText: string
                                      selectionRange: NSMakeRange(beg, len)
                                      replacementRange: NSMakeRange(NSNotFound, NSNotFound)]|]
              }

defineSelector
  newSelector { selector = "selectInputMode"
              , reciever = (''NSObject, "sender")
              , arguments = ["mode" :>>: ''String]
              , definition = [cexp| [(id)sender selectInputMode: mode]|]
              }

type ClientMachine = Effect (State ClientState :+
                             Coroutine Waiting (Maybe T.Text) :+
                             Lift IO :+ Nil)

data Environment = Env { _currentSession :: Session
                       , _currentClient  :: NSObject
                       } deriving (Typeable)

data Waiting = Registration T.Text (Maybe T.Text)
             | Selection T.Text (Maybe (Char, T.Text)) [T.Text]
             | Inquiry T.Text
               deriving (Read, Show, Eq, Ord)

type Continuation = Maybe T.Text -> ClientMachine Bool

data MarkedText = Marked T.Text | Normal T.Text
                deriving (Read, Show, Eq, Ord, Typeable)


type Client = NSObject

data Session = Session { _clientMap :: M.Map Client ClientState
                       , _self      :: HSKKController
                       , _convMode  :: ConvMode
                       , _skkDic    :: IORef Dictionary
                       } deriving (Typeable)

instance Show Session where
  showsPrec d (Session m s mode _) =
    showParen (d > 10) $
    showString "Session " . showsPrec 11 m . showChar ' ' .
    showsPrec 11 s . showChar ' ' . showsPrec 11 mode

data ClientState = Composing { _pushCmd    :: SKKCommand -> IO [SKKResult]
                             , _clientMode :: ConvMode
                             , _idling     :: IORef Bool
                             }
                 | Selecting { _clientMode :: ConvMode
                             , _selection  :: Top :>> [[T.Text]] :>> [T.Text]
                             , _selGokan   :: T.Text
                             , _selOkuri   :: Maybe (Char, T.Text)
                             , _continue   :: Continuation
                             , _display    :: [MarkedText] -> ClientMachine Bool
                             }
                 | Registering { _pushCmd       :: SKKCommand -> IO [SKKResult]
                               , _clientMode    :: ConvMode
                               , _idling        :: IORef Bool
                               , _registerBuf   :: T.Text
                               , _registerGokan :: T.Text
                               , _registerOkuri :: Maybe T.Text
                               , _continue      :: Continuation
                               }
                 | Asking { _askMessage :: T.Text
                          , _askBuf     :: T.Text
                          , _continue   :: Continuation
                          }
                 deriving (Typeable)

makeLenses ''Environment
makeLenses ''Session
makePrisms ''MarkedText

sender, client :: Given Environment => Client
sender = given ^. currentClient
client = given ^. currentClient

session :: Given Environment => Session
session = given ^. currentSession

formatMarkedText :: (Given Environment, MonadIO m)
                 => MarkedText -> m ()
formatMarkedText (Marked txt)
  = sender # setMarkedText (T.unpack txt) 0 (T.length txt)
formatMarkedText (Normal txt)
  = sender # insertText (T.unpack txt)

instance Show ClientState where
  showsPrec d (Composing _ mode _) =
    showParen (d > 10) $ showString "Composing " . showsPrec 11 mode
  showsPrec d (Asking msg buf _) =
    showParen (d > 10) $ showString "Asking " . showsPrec 11 msg .
    showChar ' ' . showsPrec 11 buf
  showsPrec d (Registering _ mode _ buf gok oku _) =
    showParen (d > 10) $
    showString "Registering " . showsPrec 11 mode . showChar ' ' .
    showsPrec 11 buf . showChar ' ' . showsPrec 11 gok .
    showChar ' ' . showsPrec 11 oku
  showsPrec d (Selecting mode cur gok oku _ _) =
    showParen (d > 10) $
    showString "Selecting " . showsPrec 11 mode . showChar ' ' .
    showsPrec 11 gok . showChar ' ' . showsPrec 11 oku . showChar ' ' .
    showsPrec 11 (cur ^. focus) . showChar ' '

makeLenses ''ClientState
makePrisms ''ClientState

objc_interface [cunit|
@interface HSKKController : IMKInputController
@property (assign) typename HsStablePtr session;
@end
|]

objc_typecheck

updateSession :: (MonadIO m, Given Environment) => (Session -> Session) -> m ()
updateSession upd = liftIO $ do
  let myself = session ^. self
  sess0 <- $(objc ['myself :> ''HSKKController] $
             ''Session <: [cexp| myself.session |])
  let sess = upd sess0
  $(objc ['sess :> ''Session, 'myself :> ''HSKKController] $
       void [cexp| myself.session = sess |])

changeMode :: Session -> String -> IO ()
changeMode sess str = maybe (return ()) (changeMode' sess) $ lookupMode str

changeMode' :: Session -> ConvMode -> IO ()
changeMode' sess kana = do
  give (Env sess undefined) $ updateSession $ convMode .~ kana
  return ()

newClientState :: (Given Environment,
                   EffectLift IO l)
               => Effect l ClientState
newClientState = do
  let dic = session ^. skkDic
      mode = session ^. convMode
  (push, ref) <- liftIO $ newPusher defKanaTable mode dic
  return $ Composing push mode ref

resetClient :: (EffectLift IO l,
                EffectState ClientState l, Given Environment)
            => Effect l ()
resetClient = do
  sender # setMarkedText "" 0 0
  put =<< newClientState

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

withSession' :: Session
             -> Client
             -> (Given Environment => ClientMachine Bool)
             -> IO Bool
withSession' sess0 sndr act = runLift $ give (Env sess0 sndr) $ do
  cst <- getClientState
  iter <- runCoroutine $ runState cst act
  evalState cst $ handleIter iter

handleIter :: (EffectLift IO l,
               EffectState ClientState l,
               Given Environment)
           => Iterator Waiting (Maybe T.Text) (Lift IO :+ Nil) (Bool, ClientState)
           -> Effect l Bool
handleIter iter = do
  case iter of
    Done (a, cst') -> do
      liftIO $ updateSession $ clientMap %~ M.insert sender cst'
      put cst'
      return a
    Next cont (Selection body mok cands) -> do
      cst <- get
      let mode = session ^. convMode
      let z = (zipper (defPager cands) & fromWithin traverse)
          cst'  = Selecting mode z body mok (handleIter <=< extend . extend . cont)
                            (relayWith cst)
      put cst'
      liftIO $ updateSession $ clientMap %~ M.insert sender cst'
      return True
    Next cont (Registration body mok) -> do
      let mode = session ^. convMode
          dic  = session ^. skkDic
      (push, ref) <- liftIO $ newPusher defKanaTable mode dic
      let cst' = Registering push mode ref "" body mok
                             (handleIter <=< extend . extend .cont)
      put cst'
      liftIO $ updateSession $ clientMap %~ M.insert sender cst'
      return True
    Next cont (Inquiry msg) -> do
      let cst' = Asking msg "" (handleIter <=< extend . extend .cont)
      put cst'
      liftIO $ updateSession $ clientMap %~ M.insert sender cst'
      return True


inputText :: Session -> NSObject -> String -> CLong -> CULong -> IO Bool
inputText sess0 cl input keyCode flags =
  handle errorLogger $ withSession' sess0 cl $ do
    let modifs = decodeModifiers flags
        key = decodeKeyboard keyCode
        shifted = all (compatible Shift) modifs && not (null modifs)
    cst <- get
    case cst of
      Asking {} -> inquiry (head input) key modifs
      Selecting {} -> doSelection (head input) key modifs
      _ -> do
        idle <- liftIO $ readIORef $ cst ^?! idling
        let mode = session ^. convMode
            dic  = session ^. skkDic
        if | null modifs && idle && input == "q" ->
              case mode of
               HankakuKatakana -> do
                 client # selectInputMode (modeToString Hiragana)
                 updateSession $ convMode .~ Hiragana
                 return True
               Hiragana -> do
                 client # selectInputMode (modeToString Katakana)
                 updateSession $ convMode .~ Katakana
                 return True
               Katakana -> do
                 client # selectInputMode (modeToString Hiragana)
                 updateSession $ convMode .~ Hiragana
                 return True
               Ascii    -> pushKey $ Incoming 'q'
           | null modifs && input == "q" -> pushKey ToggleKana
           | null modifs && idle && input == "l" ->
               case mode of
                 HankakuKatakana -> do
                   client # selectInputMode (modeToString Ascii)
                   updateSession $ convMode .~ Ascii
                   return True
                 Hiragana -> do
                   client # selectInputMode (modeToString Ascii)
                   updateSession $ convMode .~ Ascii
                   return True
                 Katakana -> do
                   client # selectInputMode (modeToString Ascii)
                   updateSession $ convMode .~ Ascii
                   return True
                 Ascii    -> pushKey $ Incoming 'l'
           | not (isControl $ head input) && all isAlphabeticModifier modifs &&
                       (key & is (_Just . _Char)) -> do
               pushKey $ Incoming $ head input
           | modifs == [Control] && key == Just (Char 'g') ->
               case cst ^? continue of
                 Nothing -> do
                   (push, ref) <- liftIO $ newPusher defKanaTable mode dic
                   put $ Composing push mode ref
                   relay []
                 Just cont -> do
                   cont Nothing
           | otherwise -> case key of
               Just Delete
                 | Registering _ _ _ _ _ _ _ <- cst, idle -> do
                   registerBuf %= initT
                   displayCurrentState
                 | otherwise -> pushKey Backspace
               Just (isNewline -> True) ->
                 if (cst & is _Registering) && idle
                 then do
                   let cont = cst ^?! continue
                   buf <- use registerBuf
                   if T.null buf
                   then cont Nothing
                   else cont $ Just buf
                 else pushKey Finish
               Just Tab    -> pushKey Complete
               Just Space | idle -> if cst & is _Registering
                                    then relay [Normal " "]
                                    else pushKey $ Incoming ' '
                          | otherwise -> pushKey Convert
               Just (Char 'q') | all (compatible Control) modifs && not (null modifs) ->
                 if idle && mode /= Ascii
                 then client # selectInputMode (modeToString HankakuKatakana) >> return True
                 else pushKey ToggleHankaku
               Just (JIS 'q') | all (compatible Control) modifs && not (null modifs) ->
                 if idle && mode /= Ascii
                 then client # selectInputMode (modeToString HankakuKatakana) >> return True
                 else pushKey ToggleHankaku
               Just JisEisuu -> return True
               Just JisKana  -> return True
               Just (Char c)
                 | all (`elem` [Alternate, Shift]) modifs ->
                   let c' = if shifted then toUpper c else c
                   in  pushKey $ Incoming c'
               Just (JIS c)
                 | all (`elem` [Alternate, Shift]) modifs ->
                   let c' = if shifted then toUpper c else c
                   in pushKey $ Incoming c'
               _ | all isAlphabeticModifier modifs && (cst & is _Registering) -> do
                     nsLog $ "relaying input directly to the registerer " ++ input
                     pushKey $ Incoming $ head input
                 | otherwise -> resetClient >> return False

errorLogger :: SomeException -> IO Bool
errorLogger (SomeException exc) = do
  nsLog $ "***Exception: " ++ show exc
  return False

initT :: T.Text -> T.Text
initT "" = ""
initT t  = T.init t

inquiry :: Given Environment
            => Char -> Maybe Keyboard -> [Modifier]
            -> ClientMachine Bool
inquiry inp mkey mods
  | mods == [Control] && mkey == Just (Char 'g') = continueWith Nothing
  | maybe False isNewline mkey && null mods = continueWith . Just =<< use askBuf
  | otherwise = do
      buf <- askBuf <<>= T.singleton inp
      msg <- use askMessage
      displayMarkedText $ msg <> buf
      return True

doSelection :: Given Environment
            => Char -> Maybe Keyboard -> [Modifier]
            -> ClientMachine Bool
doSelection ch key modifs = do
  Selecting _ z body mok push _ <- get
  let cur = z ^. focus
  if | maybe False isNewline key && null modifs -> do
         finishSelection (Just $ head cur)
     | Just Space == key && null modifs ->
        case z & rightward of
          Just pg -> do
            selection .= pg
            displayCurrentState
          Nothing -> startRegistration body mok
     | Just Delete == key || null modifs && ch == 'x' -> do
        case z & leftward of
          Just pg -> do
            selection .= pg
            displayCurrentState
          Nothing -> do
            push Nothing
     | all isAlphabeticModifier modifs && ch == 'X' -> do
         let targ = z ^. focus . _head
             msg = T.concat [body, maybe "" (T.singleton . fst) mok
                            , " /", targ, "/ "
                            ,"を削除しますか？(yes/no) "]
         displayMarkedText msg
         answer <- suspend $ Inquiry msg
         if answer == Just "yes"
           then do
             let dicR = session ^. skkDic
             liftIO $ modifyIORef' dicR $
               unregister (Input body (fst <$> mok)) targ
             finishSelection $ Just ""
           else displayCurrentState >> return True
     | maybe False (`elem` [Return, Enter]) key , null modifs -> do
         finishSelection (Just $ head $ z ^. focus)
     | modifs == [Control] && key == Just (Char 'g') ->
         finishSelection Nothing
     | null modifs, Just t <- defCSelector ch cur -> do
         finishSelection $ Just t
     | [a] <- cur, all isAlphabeticModifier modifs -> do
         _ <- finishSelection $ Just a
         pushKey $ Incoming ch
     | otherwise -> return True

displayMarkedText :: (MonadIO m, Given Environment) => T.Text -> m ()
displayMarkedText txt = sender # setMarkedText (T.unpack txt) 0 (T.length txt)

finishSelection :: Given Environment => Maybe T.Text -> ClientMachine Bool
finishSelection mtxt = get >>= \case
  Selecting _ _ _ mok cont _ -> do
    cont $ mtxt <&> \case
      ""  -> ""
      txt -> txt <> maybe "" snd mok
  _ -> error "Not in selection state!"

continueWith :: Given Environment => Maybe T.Text -> ClientMachine Bool
continueWith minp = (^? continue) <$> get >>= \case
  Just f -> f minp
  Nothing -> do
    resetClient
    return True

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

pushKey :: Given Environment => SKKCommand -> ClientMachine Bool
pushKey input = do
  nogo <- gets $ is _Selecting
  if nogo
    then return False
    else do
    push <- (^?! pushCmd) <$> get
    idled <- maybe (return True) (liftIO . readIORef) =<< gets (^? idling)
    ans <- liftIO $ push input
    let accepted = (input & is _Incoming)
                   || all (noneOf (_Idle.traverse) (is _NoHit)) ans
                   || Backspace == input && not idled
    ((), buf) <- runWriter $ mapM_ (extractTxt $ input ^? _Incoming) ans
    nsLog $ "key result: " ++ show (input, ans, buf)
    if | Just (ConvNotFound mid mok) <- find (is _ConvNotFound) ans -> do
          startRegistration mid mok
       | Just (ConvFound body mokuri cands) <- find (is _ConvFound) ans -> do
          let css  = defPager cands
              msg  = showPage mokuri $ head css
          _ <- relay [Marked msg]
          mans <- suspend $ Selection body (first toLower <$> mokuri) cands
          case mans of
            Just st -> sendCmd Refresh >> relay [Normal st]
            Nothing -> do
              cstttt <- get
              nsLog $ "Selection canceled with state: " ++ show cstttt
              displayCurrentState >> return True
       | otherwise ->  relay buf >> return accepted

displayCurrentState :: Given Environment => ClientMachine Bool
displayCurrentState = get >>= \case
  Asking msg buf _ -> relay [Marked $ msg <> buf]
  Selecting _ pg _ mok _ _ -> do
    let msg = showPage mok (pg ^. focus)
    relay [Marked msg]
  Registering _ _ _ buf mid mok _ -> do
    let msg = "[単語登録："  <> prettyOkuri mid mok <> "]" <> buf
    displayMarkedText msg
    return True
  Composing  cmd _ _ -> do
    anss <- lift $ cmd CurrentState
    ((), buf) <- runWriter $ mapM_ (extractTxt Nothing) anss
    relay buf

sendCmd :: (EffectLift IO l, EffectState ClientState l)
        => SKKCommand -> Effect l Bool
sendCmd cmd = get >>= \case
  Selecting {} -> return True
  cst          -> all (not . isIdling) <$> liftIO (cmd & cst ^?! pushCmd)

startRegistration :: Given Environment => T.Text -> Maybe (Char, T.Text)
                   -> ClientMachine Bool
startRegistration mid mok = do
  let msg = "[単語登録："  <> prettyOkuri mid (snd <$> mok) <> "]"
  displayMarkedText msg
  mans <- suspend $ Registration mid (snd <$> mok)
  st <- get
  case mans of
    Just str | not (T.null str) -> do
      let dic = session ^. skkDic
      liftIO $ modifyIORef' dic $ insert (Input mid (toLower . fst <$> mok)) $ Candidate str ""
      let conv'd = str <> maybe "" snd mok
      _ <- sendCmd Refresh
      case st of
        Selecting _ _ _ _ cont _ -> cont (Just conv'd)
        _ -> relayWith st [Normal conv'd]
    _ -> displayCurrentState >> return True

relay :: Given Environment
      => [MarkedText]
      -> ClientMachine Bool
relay xs = do
  cst <- get
  relayWith cst xs

relayWith :: Given Environment
          =>  ClientState -> [MarkedText] -> ClientMachine Bool
relayWith cst [] = do
  case cst ^? continue of
     Nothing -> do
       resetClient
       return True
     Just cont -> do
       cont Nothing
relayWith cst0 mts = do
  case cst0 of
    Asking msg buf0 _ -> do
      askBuf <>= T.concat [t | Normal t <- mts]
      let txt = msg <> buf0 <> T.concat (map unmark mts)
      displayMarkedText txt
      return True
    Composing {} -> liftM or $ forM (catMText mts) $ \case
      Marked txt -> do
        displayMarkedText txt
        return True
      Normal txt -> do
        sender # insertText (T.unpack txt)
        return True
    Registering _ _ _ buf0 body mok _cont -> do
      registerBuf <>= T.concat [txt | Normal txt <- mts]
      let buf = buf0 <> T.concat [txt | Normal txt <- mts]
          msg = T.concat ["[単語登録：", prettyOkuri body mok, "]", buf]
                <> T.concat [txt | Marked txt <- mts]
      displayMarkedText msg
      return True
    Selecting _ _ _ _ _cont disp -> disp mts

unmark :: MarkedText -> T.Text
unmark (Marked t) = t
unmark (Normal t) = t

catMText :: [MarkedText] -> [MarkedText]
catMText (Marked txt : Marked txt' : ts) = catMText (Marked (txt <> txt') : ts)
catMText (t : ts) = t : catMText ts
catMText [] = []

prettyOkuri :: T.Text -> Maybe T.Text -> T.Text
prettyOkuri t Nothing = t
prettyOkuri t (Just c) = t <> "*" <> c

extractTxt :: (EffectWriter [MarkedText] l) => Maybe Char -> SKKResult -> Effect l ()
extractTxt mc (Idle eds) = mapM_ (plainExtractTxt mc) eds
extractTxt _ (Finished t) = tell [Normal t]
extractTxt _ (Okuri body o) = tell [Marked $ "▽" <> body <> "*" <> o]
extractTxt _ (Converting str) = tell [Marked $ "▽" <> str]
extractTxt _ _ = return ()

plainExtractTxt :: (EffectWriter [MarkedText] l)
                => Maybe Char
                -> KanaResult
                -> Effect l ()
plainExtractTxt _ (Converted text) = tell [Normal text]
plainExtractTxt _ (InProgress buf) = tell [Marked $ T.decodeUtf8 buf]
plainExtractTxt (Just c) NoHit = tell [Normal $ T.singleton c]
plainExtractTxt Nothing NoHit = return ()


commit :: Session -> NSObject -> IO ()
commit sess sndr = do
  _ <- withSession' sess sndr $ do
    sender # setMarkedText "" 0 0
    a <- pushKey Finish
    resetClient
    return a
  return ()

getCurrentMode :: Session -> String
getCurrentMode sess = sess ^. convMode . to modeToString

getClientState :: (Given Environment, EffectLift IO l)
               => Effect l ClientState
getClientState = do
  let mst  = session ^. clientMap . to (M.lookup sender)
      kana = session ^. convMode
  case mst of
    Just st | maybe True (kana ==) (st ^? clientMode) -> return st
    _ -> newClientState

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

