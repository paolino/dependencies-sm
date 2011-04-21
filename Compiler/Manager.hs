{-# LANGUAGE  MonoLocalBinds, ExistentialQuantification #-}
-- | A specialised 'Manager' for 'Compiler's
module Compiler.Manager (
  -- * Interface to the module
  Compiler (..),
  Depmask (..),
  Bijection (..),
  -- * Building a manager
  Manager,
  mkCompilerManager,
  -- * Operations
  step,
  IndexError (..),
  Difference (..)
  ) where

import Prelude hiding (lookup)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Compiler.Resources
import Dependency.Manager (mkManager, Manager, Item (..),Depmask (..), step, Difference (..), IndexError (..))

import Data.Bijection

-- | Abstract description of the steps leading to a compiled value. 
-- The compiled value /a/ type must be projectable to a common type /k/ to permit serialization. 
--
-- Each 'Compile' step require its type /a/ of dependencies, while the last 'Completed' step produces the compiled value /a/
-- 
-- Monad type /m/ for the computations and index type /b/ are free for the implementations
--
data Monad m => Compiler m k b 
  -- | Last compilation step, containing the new resource, and a set of new compilers.
  = forall a. Bijection a k => Completed (m a,[(b,Depmask b (Compiler m k b ))])
  -- | Compile step. Dependency resources are expected with their index to produce next step.
  | forall a. Bijection a k => Compile (Depmask b ([(b,a)] -> m (Compiler m k b)))


--  Maps compilers with a resource provider to an 'Item' builder
mkItem :: (Monad m , Ord b)
  => (b -> Depmask b (Compiler m k b))  -- ^ base compilers for new resources , from dsl
  -> Resources m k b      -- ^ resource manager
  -> b                  -- ^ a new index
  -> Depmask b (Item m b)  -- ^ an item for the 'Dependency.Manager.mkManager'
mkItem cs s x = mkItem' (x,cs x) where
    mkItem' (z,y) = f z <$> y
    f z co = Item (build' z co) (delete s z)
    build' z co = case co of
      Completed (y,cs) -> y >>= update s z >> return (map (fst &&& mkItem') cs)
      Compile (Depmask ds f)  -> select s ds >>= f >>= build' z

-- | Build a 'Manager' where each new 'Compiler' is projected in a new 'Item' for the 'Manager' to react on new indices.
mkCompilerManager ::  (Functor m, Monad m , Ord b)
  => (b -> Depmask b (Compiler m k b))  -- ^ base compiler selector for new resources 
  -> Resources m k b                    -- ^ resource manager
  -> Manager m b                        -- ^ a fresh manager
mkCompilerManager cs = mkManager . mkItem cs
