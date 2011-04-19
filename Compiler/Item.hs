{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes,ScopedTypeVariables#-}
module Compiler.Item where

import Prelude hiding (lookup)
import Dependency.Manager (Item (..))
import Control.Applicative ((<$>))

import Compiler.Core
import Compiler.Resources


-- | Maps compilers and resource provider to an 'Item' builder
mkItem :: forall b k. Ord b 
  => (b -> Compiler IO k b)  -- ^ base compilers for new resources , from dsl
  -> Resources IO k b      -- ^ resource manager
  -> b                  -- ^ a new index
  -> Item IO b           -- ^ an item for the 'Dependency.Manager.insertItem'
mkItem cs s x = mkItem' (cs x) where
  mkItem' (Compiler x co ds) = Item 
    { index = x
    , build = let 
          build' :: Steps Compiler IO k b -> IO [Item IO b]  -- mono local binds ....
          build' co = case co of
            Completed (y,cs) -> y >>= update s x >> return (map mkItem' cs)
            Compile m f  -> select s m >>= f  >>= build'
          in build' co
    , unlink = delete s x
    , depmask = ds
    }


