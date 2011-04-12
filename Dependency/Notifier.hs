{-# LANGUAGE ViewPatterns, StandaloneDeriving , DeriveDataTypeable #-}
module Dependency.Notifier where

import Dependency.Manager
import Data.Typeable
import System.Hiernotify.Polling
import Control.Concurrent
import Control.Concurrent.Killable
import Control.Exception

deriving instance Typeable IndexError
instance Exception IndexError

-- | super simple glue to let a notifier control the rebuilding. It raises 'IndexError' as Exceptions. It returns the shutdown acion
mkController :: Ord b => Notifier -> (FilePath -> b) -> (b -> Item IO b) ->  IO (IO ())
mkController (Notifier diff s) conv mkItem = do 
  let op t = do
      (fmap conv -> DifferenceP ns ds ts, _) <- diff -- wait next difference and convert to index
      case insertItems (deleteItems (touchItems t ts) ds) (map mkItem ns) of  -- insert news , deleteds, touches
        Left e -> throw e -- index error
        Right t -> update t >>= either throw op -- update action
  z <-  forkIO $ op mkManager
  return (s >> kill z)




