{-# LANGUAGE  MonoLocalBinds #-}
-- | A specialised 'Manager' for 'Compiler's
module Compiler.Manager (
  mkCompilerManager,
  ) where

import Prelude hiding (lookup)
import Dependency.Manager (Item (..))
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Data.Dependant (Depmask (..), fmapDepmask)
import Compiler.Interface 
import Compiler.Resources
import Dependency.Manager (mkManager, Manager)


--  Maps compilers with a resource provider to an 'Item' builder
mkItem :: (Monad m , Ord b)
  => (b -> Depmask b (Compiler m k))  -- ^ base compilers for new resources , from dsl
  -> Resources m k b      -- ^ resource manager
  -> b                  -- ^ a new index
  -> Depmask b (Item m)  -- ^ an item for the 'Dependency.Manager.insertItem'
mkItem cs s x = mkItem' (x,cs x) where
    mkItem' (z,y) = f z `fmapDepmask` y
    f z co = Item (build' z co) (delete s z)
    build' z co = case co of
      Completed (y,cs) -> y >>= update s z >> return (map (fst &&& mkItem') cs)
      Compile (Depmask ds (Fromdeps f))  -> select s ds >>= f >>= build' z

-- | Build a 'Manager' where each 'Item' is projected from a 'DCompiler'
mkCompilerManager ::  (Functor m, Monad m , Ord b)
  => (b -> Depmask b (Compiler m k))  -- ^ base compiler selector for new resources 
  -> Resources m k b      -- ^ resource manager
  -> Manager m b          -- ^ a fresh manager
mkCompilerManager cs = mkManager . mkItem cs
