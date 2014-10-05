{-# LANGUAGE DeriveDataTypeable, LambdaCase, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, RecursiveDo           #-}
module Network.Skkserv where
import           Control.Applicative    ((<$), (<$>), (<*>), (<|>))
import           Control.Concurrent     (forkIO, killThread)
import           Control.Concurrent     (threadDelay)
import           Control.Lens           (to, (^.))
import           Control.Monad          (forever, unless, when)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Acquire
import           Data.Attoparsec.Text   (IResult (..), Parser, char, manyTill)
import           Data.Attoparsec.Text   (parse, satisfy, string)
import           Data.Data              (Data, Typeable)
import           Data.Function          (fix)
import qualified Data.HashMap.Strict    as HM
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import           Data.Text.IO           (hGetChunk)
import qualified Data.Text.IO           as T
import           FRP.Ordrea
import           Network                (PortID (..), accept, listenOn)
import           Network                (withSocketsDo)
import           Prelude                hiding (lookup)
import           System.IO              (BufferMode (..), Handle, hClose)
import           System.IO              (hFlush, hIsClosed, hSetBuffering)
import           System.IO              (hSetEncoding, mkTextEncoding)
import           Text.InputMethod.SKK

data Request = GetCandidate T.Text
             | GetVersion
             | GetServerInfo
             | LookPrefix T.Text
             | Disconnect
               deriving (Read, Show, Eq, Ord,
                         Data, Typeable)

data Response = Candidates [T.Text]
              | Version   T.Text
              | ServerInfo T.Text [T.Text]
              | Prefixes  [T.Text]
              | NotFound T.Text
                deriving (Read, Show, Eq, Ord,
                          Data, Typeable)

type Skkserv = Event Request -> SignalGen (Event (Maybe Response))

skkserv :: Behavior Dictionary -> Skkserv
skkserv bDic req = return $ applyBE (recv <$> bDic) req
  where
    recv _ Disconnect  = Nothing
    recv dic (GetCandidate t)
      = Just $ maybe (NotFound t) (Candidates . map formatCandidate) $
        lookup (toInput t) dic
    recv dic (LookPrefix t)
      = Just $ Prefixes $ [ d `T.snoc` r
                          | (d, r) <- dic ^. okuriAriDic . to HM.keys
                          , t `T.isPrefixOf` d
                          ] ++
                          [ d
                          | d <- dic ^. okuriNasiDic . to HM.keys
                          , t `T.isPrefixOf` d
                          ]
    recv _ GetVersion = Just $ Version "hskkserv-0.0.0"
    recv _ GetServerInfo = Just $ ServerInfo "hskkserv" ["127.0.0.1:1178"]

formatResponse :: Response -> T.Text
formatResponse msg = case msg of
  NotFound  t  -> "4" <> t
  Candidates ts -> "1/" <> T.intercalate "/" ts <> "/\n"
  Version v    -> v <> " "
  ServerInfo h ads -> h <> ":" <> T.intercalate ":" ads
  Prefixes ts  -> "1/" <> T.intercalate "/" ts <> "/\n"

pipeParser :: Parser a -> Event T.Text -> SignalGen (Event a)
pipeParser p src = flattenE . justE <$> mapAccumEM (parse p) (upd <$> src)
  where
    upd = toFixedPoint id
    toFixedPoint ts str runP =
      case runP str of
        Fail {}    -> fail "parse failed"
        Done "" r  -> return (parse p, Just $ ts [r])
        Done t r   -> toFixedPoint (ts . (r:)) t (parse p)
        Partial f  -> return (f, Just $ ts [])

handleE0 :: Handle -> IO (ExternalEvent T.Text, IO ())
handleE0 h = do
  eev <- newExternalEvent
  tid <- forkIO $ fix $ \self -> do
    cl  <- hIsClosed h
    unless cl $ do
      chunk <- hGetChunk h
      triggerExternalEvent eev chunk
      self
  return (eev, void $ hClose h >> forkIO (killThread tid))

handleE :: Handle -> Acquire (ExternalEvent T.Text)
handleE h = fst <$> mkAcquire (handleE0 h) snd

request :: Parser Request
request = Disconnect            <$  string "0"
      <|> GetCandidate . T.pack <$  char '1'
                                <*> manyTill (satisfy (/= ' ')) (char ' ')
      <|> GetVersion            <$  string "2"
      <|> GetServerInfo         <$  string "3"
      <|> LookPrefix . T.pack   <$  char '4'
                                <*> manyTill (satisfy (/= ' ')) (char ' ')

data SkkservSettings
  = SkkservSettings { port    :: Int
                    , dicPath :: FilePath
                    }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

runSkkserv :: SkkservSettings -> Skkserv -> IO ()
runSkkserv SkkservSettings{..} srv = withSocketsDo $
  listenOn (PortNumber $ toEnum port) >>= \s -> forever $ do
    (h, _, _) <- accept s
    hSetBuffering h NoBuffering
    eucjp <- mkTextEncoding "EUC-JP"
    hSetEncoding  h eucjp
    forkIO $ with (handleE h) $ \ev -> do
      step <- start $ do
        reqE <- pipeParser request =<< externalE ev
        rsp <- srv reqE
        fmap and . eventToBehavior <$> generatorE (interp h <$> rsp)
      whiler step

whiler :: IO Bool -> IO ()
whiler act = do
  next <- act
  when next $ threadDelay (10^(4 :: Int)) >> whiler act

interp :: Handle -> Maybe Response -> SignalGen Bool
interp h Nothing = liftIO (hFlush h) >> return False
interp h (Just msg) = liftIO $ do
  T.hPutStr h $ formatResponse msg
  hFlush h
  return True

