{-# LANGUAGE ExistentialQuantification #-}

-- | Compiler abstract data definition.
module Compiler.Interface where

import Data.Bijection
import Data.Binary (Binary)

-- | Abstract description of the steps leading to a compiled value. 
-- The compiled value /a/ type must be projectable to a common type /k/ to permit serialization. 
--
-- Each 'Compile' step require its type /a/ of dependencies, while the last 'Completed' step produces the compiled value /a/
-- 
-- Container type /m/ for the computations and index type /b/ are free for the implementations
--
data Steps m k b 
  -- | Last compilation step, containing the new resource, and a set of new compilers.
  = forall a. Bijection a k => Completed 
      { completed :: (m a,[Compiler m k b])
      }
  -- | Compile step. Dependency resources are expected with their index to produce next step.
  | forall a. Bijection a k => Compile 
      { mask :: b -> Bool
      , compile :: [(b,a)] -> m (Steps m k b)
      }

-- | A compiler. It contains its target index, the recursive steps requesting resources and producing the final resource for the index, and a
-- selector for all dependencies, that will be used. All resources indexed by the selected indices, are intended to be uptodate before the compilation.
--
--
data Compiler m k b = Compiler
  { target :: b                   -- ^ index of the compiler
  , steps :: Steps m k b              -- ^ steps to make the compilation
  , dependencies :: b -> Bool     -- ^ all dependencies that will be asked during compilation
  }

          

