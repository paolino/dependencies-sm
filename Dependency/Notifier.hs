{-# LANGUAGE ViewPatterns, StandaloneDeriving , DeriveDataTypeable, Rank2Types #-}
module Dependency.Notifier where

import Dependency.Manager
import Data.Typeable
import System.Hiernotify.Polling
import Control.Concurrent
import Control.Concurrent.Killable
import Control.Exception

deriving instance Typeable IndexError
deriving instance Show IndexError
instance Exception IndexError


-- | super simple glue to let a notifier control the rebuilding. It raises 'IndexError' as Exceptions. It returns the shutdown acion
mkController :: Notifier -> (FilePath -> Item IO FilePath) ->  IO (IO ())
mkController (Notifier diff s) mkItem = do 
  let op t = do
      d@(DifferenceP ns ds ts, _) <- diff -- wait next difference and convert to index
      print d 
      case deleteItems t ds >>= \t -> touchItems t ts >>= \t -> insertItems t (map mkItem ns) of  -- insert news , deleteds, touches
        Left e -> throw e -- index error
        Right t -> update t >>= either throw op -- update action
  z <-  forkIO $ op mkManager
  return (s >> kill z)




