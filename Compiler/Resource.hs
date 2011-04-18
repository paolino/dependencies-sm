{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
module Compiler.Resource (
    Resources (..),
    inFiles,
    inMemory
    )
    where

import Prelude hiding (readFile, writeFile)
import Data.Binary (Binary,encode,decode)
import Control.Concurrent.STM (newTVarIO, readTVar, writeTVar, atomically)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.ByteString.Lazy (ByteString, writeFile, readFile)
import Data.Hashable (Hashable, hash)
import System.FilePath ((</>))
import System.Directory (removeFile)


class FromTo k a where
  from :: k -> a
  to :: a -> k

instance Binary a => FromTo ByteString a where
  from = decode
  to = encode

instance FromTo a a where
  from = id
  to = id

-- | Resource management. Resource resources are stored and retrived from here.
data Resources k b = Resources
  { update :: forall a . FromTo k a => b -> a -> IO ()           -- ^ set a value for the index
  , select :: forall a . FromTo k a => (b -> Bool) -> IO [(b,a)] -- ^ get a list of index and value where indices match the condition
  , delete :: b -> IO ()                       -- ^ forget the given index 
  }

inMemory :: forall k b . Eq b => IO (Resources k b)
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
   

inFiles :: forall b . (Eq b , Hashable b) => FilePath -> IO (Resources ByteString b)
inFiles p  = do
  hashes <- inMemory :: IO (Resources String b)
  let
    update' :: forall a . FromTo ByteString a => b -> a -> IO ()
    update' x y = do
      let h = show (hash x)
      update hashes x h 
      writeFile (p </> h) $ to y
    select' :: forall a . FromTo ByteString a => (b -> Bool) -> IO [(b,a)]
    select' f = do
      (xs,hs) <- unzip <$> select hashes f
      rs <- mapM (fmap from . readFile . (p </>)) hs
      return $ zip xs rs
    delete' x = do
      [(_,h)] <- select hashes ((==) x)
      delete hashes x
      removeFile $ p </> h
  return $ Resources update' select' delete'



