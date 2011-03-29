-- | Internal structure of the Graph type. Not exported outside of the
-- library.
--
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Graph
    ( Graph
    , fromList
    , member
    , nodes
    , neighbours
    , reverse
    , reachableNodes
    , sanitize
    , graphContents
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

-- | A node in the directed graph
--
data Node a = Node
    { nodeTag        :: a      -- ^ Tag identifying the node
    , nodeNeighbours :: Set a  -- ^ Edges starting at this node
    } deriving (Show, Typeable)

instance (Binary a, Ord a) => Binary (Node a) where
    put (Node t n) = put t >> put n
    get = Node <$> get <*> get

-- | Append two nodes. Useful for joining graphs.
--
appendNodes :: Ord a => Node a -> Node a -> Node a
appendNodes (Node t1 n1) (Node t2 n2)
    | t1 /= t2 = error'
    | otherwise = Node t1 (n1 `S.union` n2)
  where
    error' = error $ "Graph: Appending differently tagged nodes"

-- | Type used to represent a directed graph
--
newtype Graph a = Graph {unGraph :: Map a (Node a)}
                        deriving (Show, Binary, Typeable)

-- | Allow users to concatenate different graphs
--
instance Ord a => Monoid (Graph a) where
    mempty = Graph M.empty
    mappend (Graph m1) (Graph m2) = Graph $
        M.unionWith appendNodes m1 m2

nodeContents :: Ord a => Node a -> S.Set a
nodeContents (Node x ys) = x `S.insert` ys

graphContents :: Ord a => Graph a -> S.Set a
graphContents = S.unions . map nodeContents . M.elems . unGraph


-- | Construction of directed graphs
--
fromList :: Ord a
         => [(a, Set a)]     -- ^ List of (node, reachable neighbours)
         -> Graph a  -- ^ Resulting directed graph
fromList = Graph . M.fromList . map (\(t, d) -> (t, Node t d))



-- | Check if a node lies in the given graph
--
member :: Ord a
       => a                -- ^ Node to check for
       -> Graph a  -- ^ Directed graph to check in
       -> Bool             -- ^ If the node lies in the graph
member n = M.member n . unGraph

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
neighbours x = fromMaybe S.empty . fmap nodeNeighbours
             . M.lookup x . unGraph

-- | Reverse a directed graph (i.e. flip all edges)
--
reverse :: Ord a
        => Graph a
        -> Graph a
reverse = mconcat . map reverse' . M.toList . unGraph
  where
    reverse' (id', Node _ neighbours') = fromList $
        zip (S.toList neighbours') $ repeat $ S.singleton id'

-- | Find all reachable nodes from a given set of nodes in the directed graph
--
reachableNodes :: Ord a => Set a -> Graph a -> Set a
reachableNodes set graph = reachable (setNeighbours set) set
  where
    reachable next visited
        | S.null next = visited
        | otherwise = reachable (sanitize' neighbours') (next `S.union` visited)
      where
        sanitize' = S.filter (`S.notMember` visited)
        neighbours' = setNeighbours (sanitize' next)

    setNeighbours = S.unions . map (`neighbours` graph) . S.toList
    
-- | Remove all dangling pointers, i.e. references to nodes that do 
-- not actually exist in the graph.
--
sanitize :: Ord a => Graph a -> Graph a
sanitize (Graph graph) = Graph $ M.map sanitize' graph
  where
    sanitize' (Node t n) = Node t $ S.filter (`M.member` graph) n
