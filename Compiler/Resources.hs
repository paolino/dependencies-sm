{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
-- | Abstract 'Resources' definition and some implementations. 'Resources' value are monadic resource managers, with classic methods, 'update'
-- 'select' and 'delete'. Resurces are indexed by unique indices.
module Compiler.Resources (
    -- * Operations
    Resources (..),
    -- * Builders
    inFiles,
    inMemory,
    )
    where

import Control.Concurrent.STM (newTVarIO, readTVar, writeTVar, atomically)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Hashable (Hashable, hash)
import System.FilePath ((</>))
import System.Directory (removeFile)
import Data.Bijection (Bijection (..))

-- | Resource management. Type \'b\' is the index type, type \'k'\ is the serialization type.
data Resources m k b = Resources
  { update :: forall a . Bijection a k => b -> a -> m ()           -- ^ set a value for the index
  , select :: forall a . Bijection a k => (b -> Bool) -> m [(b,a)] -- ^ get a list of index and value where indices match the condition
  , delete :: b -> m ()                       -- ^ forget the given index 
  }

-- | In memory resource management
inMemory :: forall k b . Eq b => IO (Resources IO k b)
inMemory = do
  t <- newTVarIO [] 
  let
    update' x y m =  (x,to y :: k) : filter ((/=) x . fst) m
    select' f m = map (second from) . filter (f . fst) $ m
    delete' x m = filter ((/=) x . fst) m
  return $ Resources 
      (\x y -> atomically $ update' x y <$> readTVar t >>= writeTVar t) 
      (\f ->  atomically $ select' f <$> readTVar t ) 
      (\x -> atomically $ delete' x  <$> readTVar t >>=  writeTVar t)
   
-- | Filesystem persistent synchronous resource management
inFiles :: forall b k . (Eq b , Hashable b) 
  => (FilePath -> IO k)       -- ^ read file action for the serialization type
  -> (FilePath -> k -> IO ()) 
  -> FilePath 
  -> IO (Resources IO k b)
inFiles readFile writeFile p  = do
  hashes <- inMemory :: IO (Resources IO FilePath b)
  let
    update' x y = do
      let h = show (hash x)
      update hashes x h 
      writeFile (p </> h) $ to y
    select' f = do
      (xs,hs) <- unzip <$> select hashes f
      rs <- mapM (fmap from . readFile . (p </>)) hs
      return $ zip xs rs
    delete' x = do
      [(_,h)] <- select hashes ((==) x)
      delete hashes x
      removeFile $ p </> h
  return $ Resources update' select' delete'


