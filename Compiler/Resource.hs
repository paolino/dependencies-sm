{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Compiler.Resource where

import Prelude hiding (lookup)
import Data.Typeable
import Data.Binary
import Dependency.Manager
import Data.Map (Map, fromList, lookup, insert)
import Control.Arrow (second)
import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import Control.Monad.Trans
import Control.Concurrent.STM


-- | Resource management. Resource resources are stored and retrived from here.
data Resources m b = Resources
  { update :: b -> Resource -> m ()           -- ^ set a value for the index
  , select :: (b -> Bool) -> m [(b,Resource)] -- ^ get a list of index and value where indices match the condition
  , delete :: b -> m ()                       -- ^ forget the given index 
  }

-- | boxing a compiled value. For the store to operate 'Binary' is needed for serialization, 'Typeable' to be casted as dependency.
data Resource = forall a. (Binary a, Typeable a) => Resource a

-- unboxing a compiled value, using 'Typeable' instance
resource :: Typeable a => Resource -> Maybe a
resource (Resource x) = cast x


updateFile :: (Functor m, MonadIO m, Typeable a, Binary a) => (FilePath -> b) -> Resources m b -> FilePath -> (String -> a) -> m ()
updateFile f r p read = (fmap read . liftIO . readFile) p >>= update r (f p) . Resource


mkResources :: (b -> String) -> FilePath -> Resources IO b
mkResources f p = Resources {
        update = \y (Resource x) -> do 
              atomically $ readTVar index >>= writeTVar index . insert y (f y)
              writeFile (p </> f y) (encode x)
        select = \f -> do
              ks <- to

