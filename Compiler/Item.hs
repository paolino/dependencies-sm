{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Compiler.Item where

import Prelude hiding (lookup)
import Dependency.Manager (Item (..))
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Data.ByteString.Lazy (ByteString)

import Compiler.Core
import Compiler.Resource


-- | Maps compilers and resource provider to an 'Item' builder
mkItem :: Ord b 
  => (b -> Compiler b)  -- ^ base compilers for new resources , from dsl
  -> Resources ByteString b      -- ^ resource manager
  -> b                  -- ^ a new index
  -> Item IO b           -- ^ an item for the 'Dependency.Manager.insertItem'
mkItem cs s x = mkItem' (cs x) where
  mkItem' (Compiler x co ds) = Item 
    { index = x
    , build = let 
          build' co = case co of
            Completed (y,cs) -> y >>= update s x >> return (map mkItem' cs)
            Compile m f  -> select s m >>= f . catMaybes . map (\(x,y) -> (x,) <$> y) >>= build'
          in build' co
    , unlink = delete s x
    , depmask = ds
    }
          
