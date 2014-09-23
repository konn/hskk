module Main where
import Network.Skkserv
import Text.InputMethod.SKK

import           Control.Applicative    ((<$>))
import           Control.Concurrent.STM (atomically, newTVarIO, readTVarIO)
import           Control.Concurrent.STM (writeTVar)
import           Data.String            (fromString)
import qualified Data.Text.IO           as T
import           FRP.Ordrea
import           System.Environment     (getArgs)
import           System.FilePath
import           System.FSNotify

main :: IO ()
main = withManager $ \man -> do
  [dic] <- getArgs
  dic0 <- parseDic dic
  tvar <- newTVarIO dic0
  _ <- watchDir man (fromString $ takeDirectory dic)
    ((== fromString dic) . eventPath) $ const $ do
    atomically . writeTVar tvar =<< parseDic dic
  runSkkserv SkkservSettings { port = 1728, dicPath = dic } $ \a -> do
    dicB <- externalB $ readTVarIO tvar
    skkserv dicB a

parseDic :: FilePath -> IO Dictionary
parseDic fp = do
  Right dic <- parseDictionary <$> T.readFile fp
  return dic
