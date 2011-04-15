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

mkItem :: Maybe String -> FilePath -> Item IO FilePath
mkItem mt x = let 
        build = do
              ts <- lines `fmap` maybe (readFile x) return mt
              putStrLn $ "build: " ++ x ++ " --> " ++ show ( ts)
              return $ if not (null ts) then [mkItem  (Just $ unlines $ tail ts) $ ( head ts) ] else []
        destroy = putStrLn $ "destroy: " ++ x
        depmask y = y `isPrefixOf` init x
        in Item x build destroy depmask

c = Configuration "." 4 $ (liftM2 (&&) (not . isPrefixOf ".") (/= "Example"))

main = do
  p <- mkPollNotifier 2 c
  r <- mkController p (mkItem Nothing)
  forever $  getLine >>= system >> return ()
  
