{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Compiler.Item where

import Prelude hiding (lookup)
import Data.Typeable
import Data.Binary
import Dependency.Manager
import Data.Map (Map, fromList, lookup)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)


import Compiler.Pure
import Compiler.Resource


-- | Maps compilers and resource provider to an 'Item' builder
mkItem :: (Functor m , Monad m, Ord b ) 
  => (b -> Compiler b)  -- ^ base compiler selector, from dsl
  -> Resources m b      -- ^ resource manager
  -> b                  -- ^ an index
  -> Item m b           -- ^ an item for the 'Dependency.Manager.insertItem'
mkItem cs s x = mkItem' (cs x) where
  mkItem' (Compiler x co ds) = Item 
    { index = x
    , build = let 
          build' co = case co of
            Completed (y,cs) -> update s x (Resource y) >> return (map mkItem' cs)
            Compile m f  -> select s m >>= build' . f . catMaybes . map (\(x,y) -> (x,) <$> resource y)
          in build' co
    , unlink = delete s x
    , depmask = ds
    }
          

