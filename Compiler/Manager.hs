{-# LANGUAGE ExistentialQuantification, RankNTypes, MonoLocalBinds #-}
-- | A specialised 'Manager' for 'Compiler's
module Compiler.Manager (
  mkCompilerManager 
  ) where

import Prelude hiding (lookup)
import Dependency.Manager (Item (..))
import Control.Applicative ((<$>))

import Compiler.Interface 
import Compiler.Resources
import Dependency.Manager (mkManager, Manager)


--  Maps compilers with a resource provider to an 'Item' builder
mkItem :: (Monad m , Ord b)
  => (b -> Compiler m k b)  -- ^ base compilers for new resources , from dsl
  -> Resources m k b      -- ^ resource manager
  -> b                  -- ^ a new index
  -> Item m b           -- ^ an item for the 'Dependency.Manager.insertItem'
mkItem cs s x = mkItem' (cs x) where
  mkItem' (Compiler x co ds) = Item 
    { index = x
    , build = let 
          build' co = case co of
            Completed (y,cs) -> y >>= update s x >> return (map mkItem' cs)
            Compile m f  -> select s m >>= f  >>= build'
          in build' co
    , unlink = delete s x
    , depmask = ds
    }

-- | Build a 'Manager' where each 'Item' is projected from a 'Compiler'
mkCompilerManager ::  (Functor m, Monad m , Ord b)
  => (b -> Compiler m k b)  -- ^ base compiler selector for new resources 
  -> Resources m k b      -- ^ resource manager
  -> Manager m b          -- ^ a fresh manager
mkCompilerManager cs = mkManager . mkItem cs
