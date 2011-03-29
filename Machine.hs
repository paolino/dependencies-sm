module Machine (Machine (..), bootMachine, New (..) , Build, Graph) where

import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow ((&&&),(***))
import Control.Applicative ((<$>))
import Data.Monoid (Monoid, mappend, mempty)

import Graph


-- the running state of the machine
data Analyze a = Analyze
    { graph   :: Graph a  -- actual dependency graph
    , todo    :: Set a    -- targets to be built
    , done    :: Set a    -- targets built
    }

-- | A new target for the machine
data New a = New {
  target :: a,      -- the new target
  rebuild :: Bool,  -- force rebuilding
  deps :: [a]       -- its dependencies
  }

-- compute a new state for the machine. aside widening the graph new targets are inserted in the todo
insertNews :: (Ord a, Eq a)   => Graph a    -- previous graph
                              -> Analyze a  -- current state
                              -> [New a]     -- news from the build action
                              -> Analyze a  -- new state
insertNews prev  (Analyze g rs ds) news =
  let   ng = fromList . map (target &&& S.fromList . deps) $ news
        nrs = S.fromList . map fst . filter snd . map (target &&& rebuild) $ news
        g' = g `mappend` ng -- new current graph
        igns = nodes ng `S.difference` nrs -- targets mentioned only in the graph (not marked as todo from outside)
        cdeps = S.fromList $ filter (\x -> x `neighbours` prev /= x `neighbours` g') $ S.toList $ igns -- touched father targets
        rdeps = reachableNodes (nrs `mappend` cdeps) g' -- dependants of asked targets and touched father targets
  in Analyze g' (rs `mappend` (rdeps `S.difference` ds)) ds

-- | Build action interface, from a target to some new targets. Client can use monad m for his businnes
type Build m a = a -> m [New a]

-- | State machine to drive the building of targets.
data Machine m a    = Done (Graph a)  -- | Work is over, the graph given is useful for next run
                    | Cycle           -- | A cycle was detected, abort
                    | Step (Build m a -> m (Machine m a)) -- | machine regular stepping 

-- | Boot an machine from the previous graph. Use mempty when it's missing.
bootMachine prev = step prev $ Analyze mempty mempty mempty

-- check the Done state or fire a research for a target to build
step :: (Functor m, Ord a, Monad m) => Graph a -> Analyze a -> Machine m a
step prev r = case S.null $ todo r of
    -- No remaining items, spits out the new graph
    True -> Done $ graph r
    -- An item remains, let's find a ready item
    False -> findReady prev r $ S.findMin $ todo r

--  Find an item ready to be compiled. Goes straight down to the *left* until an item with all dependencies done is found.
findReady :: (Functor m, Monad m, Ord a) => Graph a -> Analyze a -> a -> Machine m a
findReady prev r x = findReady' r x S.empty where
    findReady' r@(Analyze g todos dones) x vs
        -- We already visited this item, cycle detected!
        | x `S.member` vs = Cycle
        -- The x has all neighbours in done set
        | neighbours x g `S.isSubsetOf` dones = let 
                insertion = insertNews prev $ Analyze g (x `S.delete` todos) (x `S.insert` dones)
                in Step $ \f -> step prev <$> insertion <$> f x
        -- Continue our search: find a neighbour we haven't visited yet
        | otherwise = let   ds = neighbours x g `S.difference` vs 
                            x = S.findMin ds
                      in if S.null ds then Cycle else findReady' r x (S.insert x vs)




