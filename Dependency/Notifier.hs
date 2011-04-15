{-# LANGUAGE ViewPatterns, StandaloneDeriving , DeriveDataTypeable, Rank2Types #-}
module Dependency.Notifier where

import Dependency.Manager
import Control.Monad
import Data.Typeable
import System.Hiernotify.Polling
import Control.Concurrent
import Control.Concurrent.Killable
import Control.Exception



-- | super simple glue to let a notifier control the rebuilding. It raises 'IndexError' as Exceptions. It returns the shutdown acion
mkController :: Notifier -> (FilePath -> Item IO FilePath) ->  IO (IO ())
mkController (Notifier diff s) mkItem = do 
  let op :: Manager IO FilePath -> IO ()
      op t = do
              d@(DifferenceP ns ds ts, _) <- diff -- wait next difference and convert to index
              t' <- foldM (\t n ->  insertItem t n >>= either (\e -> print e >> return t) return) t $ map mkItem ns
              t'' <- foldM (\t n ->  deleteItem t n >>= either (\e -> print e >> return t) return) t' $ ds
              t''' <- foldM (\t n -> touchItem t n >>= either (\e -> print e >> return t) return) t'' $ ts
              op t'''
  z <-  forkIO $ op mkManager
  return (s >> kill z)




