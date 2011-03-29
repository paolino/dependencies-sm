-- | Directed Graph library.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graph
    ( Graph
    , newGraph
    , nodes
    , neighbours
    , reverseGraph
    , reachables
    ) where

import Data.Maybe (fromMaybe)
import Prelude hiding (reverse, filter)
import Control.Applicative ((<$>), (<*>))
import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Binary (Binary, put, get)
import Data.Typeable (Typeable)
import Debug.Trace

-- A node in the directed graph
newtype Node a = Node {links :: Set a} deriving (Binary, Monoid)

-- | A directed graph
newtype Graph a = Graph {unGraph :: Map a (Node a)} deriving (Binary, Monoid)


-- | Construction of directed graphs
--
newGraph :: Ord a
         => [(a, Set a)]     -- ^ List of (node, reachable neighbours)
         -> Graph a  -- ^ Resulting directed graph
newGraph = Graph . M.fromList . map (\(t, d) -> (t, Node d))


-- | Get all nodes in the graph
--
nodes :: Ord a
      => Graph a  -- ^ Graph to get the nodes from
      -> Set a            -- ^ All nodes in the graph
nodes = M.keysSet . unGraph

-- | Get a set of reachable neighbours from a directed graph
--
neighbours :: Ord a
           => a                -- ^ Node to get the neighbours of
           -> Graph a  -- ^ Graph to search in
           -> Set a            -- ^ Set containing the neighbours
neighbours x = fromMaybe S.empty . fmap links . M.lookup x . unGraph

-- | Reverse a directed graph (i.e. flip all edges)
--
reverseGraph :: Ord a
        => Graph a
        -> Graph a
reverseGraph = mconcat . map reverse' . M.toList . unGraph
  where reverse' (id', Node neighbours') = newGraph $ zip (S.toList neighbours') $ repeat $ S.singleton id'

-- | Find all reachable nodes from a given set of nodes in the directed graph
--
reachables :: Ord a => Set a -> Graph a -> Set a
reachables set graph = reachable (setNeighbours set) set
  where
    reachable next visited
        | S.null next = visited
        | otherwise = reachable (sanitize' neighbours') (next `S.union` visited)
      where
        sanitize' = S.filter (`S.notMember` visited)
        neighbours' = setNeighbours (sanitize' next)

    setNeighbours = S.unions . map (`neighbours` graph) . S.toList
    

