-- | A yielding driver for a build action. Client must supply a 'Build' type action from a target item to some new targets and dependencies.
-- 
-- New targets can be marked as todo, expressing a condition coming from the client. In example it can be a filesystem resource modified.
--
-- The action is free in the monad, the client does its businnes in it.
--
-- The driver is actually yielding a higher order action. This interface permits the client to control each build action. 
-- This can be useful for backtracking or recovering errors.
-- 
-- When the driver yield a 'Done' the resulting state of the driver is exposed, which is serializable and it is the seed for the next building.
--
-- The contract rules for the 'Driver' are
--
--  1. all requested to build items must be called for build
-- 
--  2. all inserted item must be called for build when they changed their dependency set from previuos build  
-- 
--  3. each item, dependent on any of the above, must be called for build
-- 
-- paolino (0.1)
--
module Driver (Driver (..), bootDriver, New (..) , Build, Graph) where

import Data.Set (Set)
import Data.List (find)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow ((&&&),(***))
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, mappend, mempty)
import Debug.Trace

import Graph


-- the running state of the driver
data Analyze a = Analyze
    { graph   :: Graph a  -- actual dependency graph
    , todo    :: Set a    -- items to be built
    , done    :: Set a    -- items built
    } 

-- | A new item for the driver
data New a = New {
  item :: a,      -- ^ the new item
  rebuild :: Bool,  -- ^ force rebuilding
  deps :: [a]       -- ^ its dependencies
  }

-- compute a new state for the driver. aside widening the graph new items are inserted in the todo
insertNews :: (Show a, Ord a, Eq a)   => Graph a    -- previous graph
                              -> Analyze a  -- current state
                              -> [New a]     -- news from the build action
                              -> Analyze a  -- new state
insertNews prev  (Analyze g rs ds) news =
  let   ng = newGraph . map (item &&& S.fromList . deps) $ news
        nrs = S.fromList . map item . filter rebuild $ news
        g' = g `mappend` ng -- new current graph
        igns = nodes ng `S.difference` nrs -- items mentioned only in the graph (not marked as todo from outside)
        cdeps = S.fromList $ filter (\x -> x `neighbours` prev /= x `neighbours` g') $ S.toList $ igns -- touched father items
        rdeps =  reachables (nrs `mappend` mempty) (reverseGraph g') -- dependants of asked items and touched father items
        in Analyze g' ((rs `mappend` rdeps) `S.difference` ds) ds

-- | Build action interface, from a item to some new items. Client can use monad m for his businnes
type Build m a = a -> m [New a]

-- | State driver to drive the building of items.
data Driver m a    = Done (Graph a)  -- ^ Work is over, the graph given is useful for next run
                    | Cycle           -- ^ A cycle was detected, abort
                    | Step (Build m a -> m (Driver m a)) -- ^ driver regular stepping 


-- | Boot a driver from the previous graph. Use mempty when it's missing.
bootDriver :: (Show a,Ord a, Functor m, Monad m) 
  => Graph a    -- ^ previous graph
  -> [New a]    -- ^ boot request
  -> Driver m a -- ^ resulting driver
bootDriver prev = step prev . insertNews prev (Analyze mempty mempty mempty) 

-- check the Done state or fire a research for a item to build
step :: (Show a,Functor m, Ord a, Monad m) => Graph a -> Analyze a -> Driver m a
step prev r@(Analyze g todos dones) = case S.null $ todo r of
    -- No remaining items, spits out the new graph
    True -> Done $ g
    -- An item remains, let's find a ready item
    False -> case find (\t -> S.null (neighbours t g `S.intersection` todos)) . S.toList $ todos of
        Nothing -> Cycle
        Just x ->   let insertion = insertNews prev $ Analyze g (x `S.delete` todos) (x `S.insert` dones)
                    in Step $ \f -> step prev <$> insertion <$> f x



