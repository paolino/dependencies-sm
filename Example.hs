import Dependency.Notifier
import Dependency.Manager
import System.FilePath
import System.Hiernotify.Polling
import Control.Concurrent
import Data.List
import Control.Monad
import System.Process
import System.IO

-- | example
mkItem :: Int -> Maybe String -> FilePath -> Item IO FilePath
mkItem n mt x = let 
        build = do
              t <- maybe (readFile x) return mt
              putStrLn $ "build: " ++ x ++ " --> " ++ show (take 20 t)
              return $ if "meta" `isPrefixOf` t then [mkItem (n + 1) (Just $ drop 4 t) $ x </> show n] else []
        destroy = putStrLn $ "destroy: " ++ x
        depmask y = y `isPrefixOf` init x
        in Item x build destroy depmask

c = Configuration "." 4 $ (not . isPrefixOf ".")

main = do
  p <- mkPollNotifier 2 c
  r <- mkController p (mkItem 0 Nothing)
  forever $  getLine >>= system >> return ()
  
