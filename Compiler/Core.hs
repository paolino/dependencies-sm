{-# LANGUAGE ExistentialQuantification #-}
module Compiler.Core where

import Data.Bijection
import Data.Binary (Binary)

-- | Description of a pure compiler. Pure compilers hides types of dependency resources and their output resource type in existential boxes.
-- compiled value is instance of 'Binary' to allow writing to disk.
data Steps c m k b 
  -- | Last compilation step, containing the new resource, and a set of new compilers.
  = forall a. Bijection a k => Completed 
      { completed :: (m a,[c m k b])
      }
  -- | Compile step. Dependency resources are expected with their index to produce next step.
  | forall a. Bijection a k => Compile 
      { mask :: b -> Bool
      , compile :: [(b,a)] -> m (Steps c m k b)
      }

-- | A compiler. It contains its target index, the recursive steps requesting resources and producing the final resource for the index, and a
-- selector for all dependencies, that will be used. All resources indexed by the selected indices, are intended to be uptodate before the compilation.
-- This is the meaning of the selector.
data Compiler m k b = Compiler
  { target :: b                   -- ^ index of the compiler
  , steps :: Steps Compiler m k b              -- ^ steps to make the compilation
  , dependencies :: b -> Bool     -- ^ all dependencies that will be asked during compilation
  }

          

