{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Compiler.Pure where

import Prelude hiding (lookup)
import Data.Typeable
import Data.Binary
import Dependency.Manager
import Data.Map (Map, fromList, lookup)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

-- | Description of a pure compiler. Pure compilers hides types of dependency resources and their output resource type in existential boxes.
-- dependencies are casted so they are instance of 'Typeable', while compiled value is instance of 'Binary' to allow writing to disk.
data Steps b 
  -- | Last compilation step, containing the new resource, and a set of new compilers.
  = forall a. (Binary a, Typeable a) => Completed 
      { completed :: (a,[Compiler b])
      }
  -- | Compile step. Dependency resources are expected with their index to produce next step.
  | forall a. Typeable a => Compile 
      { mask :: b -> Bool
      , compile :: [(b,a)] -> Steps b 
      }

-- | A compiler. It contains its target index, the recursive steps requesting resources and producing the final resource for the index, and a
-- selector for all dependencies, that will be used. All resources indexed by the selected indices, are intended to be uptodate before the compilation.
-- This is the meaning of the selector.
data Compiler b = Compiler
  { target :: b
  , steps :: Steps b
  , dependencies :: b -> Bool
  }

          

