{-# LANGUAGE ExistentialQuantification #-}

-- | Compiler abstract data definition.
module Compiler.Interface where

import Data.Bijection
import Data.Dependant (Depmask (..))

-- | Abstract description of the steps leading to a compiled value. 
-- The compiled value /a/ type must be projectable to a common type /k/ to permit serialization. 
--
-- Each 'Compile' step require its type /a/ of dependencies, while the last 'Completed' step produces the compiled value /a/
-- 
-- Container type /m/ for the computations and index type /b/ are free for the implementations
--
data Compiler m k b 
  -- | Last compilation step, containing the new resource, and a set of new compilers.
  = forall a. Bijection a k => Completed (m a,[(b,Depmask b (Compiler m k))])
  -- | Compile step. Dependency resources are expected with their index to produce next step.
  | forall a. Bijection a k => Compile (Depmask b (Fromdeps m k a))
 

-- | A wrapper around a monadic function from indexed dependencies to a new compiler stage
newtype Fromdeps m k a b = Fromdeps ([(b,a)] -> m (Compiler m k b))
