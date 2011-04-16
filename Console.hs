import Dependency.Manager
import System.FilePath
import System.Hiernotify.Polling
import Control.Concurrent
import Control.Concurrent.Killable
import Data.List
import Control.Monad

mkController :: Notifier -> (FilePath -> Item IO FilePath) ->  IO (IO ())
mkController (Notifier diff s) mkItem = do 
  let folder f t n = f t n >>= either (\e -> print e >> return t) return
      op :: Manager IO FilePath -> IO ()
      op t = do
              d@(DifferenceP ns ds ts, _) <- diff 
              t' <- foldM (folder insertItem) t $ map mkItem ns
              t'' <- foldM (folder deleteItem) t' $ ds
              t''' <- foldM (folder touchItem) t'' $ ts
              op t'''
  z <-  forkIO $ op mkManager
  return (s >> kill z)

mkItem :: Maybe String -> FilePath -> Item IO FilePath
mkItem mt x = let 
        build = do
              ts <- lines `fmap` maybe (readFile x) return mt
              putStrLn $ "build: " ++ x ++ " --> " ++ show ( ts)
              return $ if not (null ts) then [mkItem  (Just $ unlines $ tail ts) $ ( head ts) ] else []
        unlink = putStrLn $ "unlink: " ++ x
        depmask y = y `isPrefixOf` init x
        in Item x build unlink depmask

main = do
  p <- mkPollNotifier 2 $ Configuration "." 4 $ (liftM2 (&&) (not . isPrefixOf ".") (not . isPrefixOf "console-notifier-dependencies-sm"))
  r <- mkController p (mkItem Nothing)
  getLine
  
