{-# LANGUAGE ViewPatterns, DeriveDataTypeable #-}
-- | Low level module. The structure exported from this module is a 2-step circular machine: 'Graph' a b. 
--
-- Its type arguments are:
--
-- * Resources expressed as type \'a\'
--
-- * Indices expressed as type \'b\'.
--
-- A 'Graph' is built around 2 kinds of dependencies, logic and existential. 
-- A logic dependency is expressed by an index mask, while an existential dependency by an index. Existential dependencies for a resource 
-- are unique or not present.
--
-- The semantic of a logic dependency is that the uptodate state of the dependent resource can't hold if its dependencies are not all uptodate. 
-- Operationally, any time a dependency is to be built, all dependent resources recursively must be built.
--
-- The semantic of an existence dependency is that a dependent resource can exist only if its dependency (if present) is uptodate. 
-- Every time an existential dependency is to be built or it's deleted, its dependant must be deleted.
--
-- A 'Graph' value can be in 'Accept' or 'Present' state. 
--
-- When it is in 'Accept' state three operations are available to make it evolve. All these operations produces a graph in the 'Present' 
-- state or signal an 'IndexError'.
--
-- * 'create' is used to insert a new resource. Its index, value, logic dependency mask, and a possible existence dependency must be given.
-- Cycles formed following any kind of dependants are forbidden. Duplicate index or an non existent existence index are errors too.
--
-- * 'erase' is used to delete one by index. Index must be present.
--
-- * 'touch' is used to signal a resource was modified. Index must be present.
--
-- When the 'Graph' is in 'Present' state the next change for the client is presented, together with the new graph state. The new state can be in
-- 'Present' state if anything is left to be presented to reach an uptodate state, or in 'Accept' state, if all changes have been presented.
--
-- The order of requests presented will be
--
-- * all resources marked 'Unlink' in unspecified order first, followed by 
--
-- * all resources marked 'Build' sorted so that resources presented have all their dependencies uptodate
--
module Dependency.Graph 
  ( Graph (..)
  , mkGraph
  , Operation (..)
  , Create
  , Erase
  , Touch
  , Request (Unlink, Build)
  , IndexError (..)
  )
  where
import Data.Map (Map, (!), empty, adjust, member, insert, delete, findWithDefault, lookup, null, filter, findMin, toList)
import qualified Data.Set as S (fromList)
import Control.Monad (when, foldM, (<=<),(>=>))
import Prelude hiding (lookup, null, filter)
import Data.List ((\\), break, nub)
import Control.Arrow (second, (***)) 
import Data.Typeable (Typeable) 
import Dependency.Lib (cycleDetect)

-- a node surround a core value with links to logic dependants and existential dependants
data Node a b = Node 
  { value :: a
  , logic :: [b]
  , existence :: [b]
  }

-- modify logic dependants
fmapLogic :: ([b] -> [b]) -> Node a b -> Node a b
f `fmapLogic` Node x ls es = Node x (f ls) es

-- modify existential dependants
fmapExistence :: ([b] -> [b]) -> Node a b -> Node a b
f `fmapExistence` Node x ls es = Node x ls $ f es

-- | Possible requests/states for a resource
data Request a 
  = Uptodate {status :: a} -- ^ no request
  | Build {status :: a} -- ^ the resource must be built
  | Unlink {status :: a}  -- ^ the resource must be deleted
  deriving (Show,Eq)

-- check for unlink request
isUnlink (Unlink _) = True
isUnlink _ = False

-- check for uptodate status
isUptodate (Uptodate x) = True
isUptodate _ = False


instance Functor Request where
  f `fmap` s = s{status = f $ status s}

-- indexed nodes, tagged with their status/request 
type Graph' a b = Map b (Request (Node a b))

-- marker type
data Modi = MBuild | MUnlink

-- crashing lookup. it's used for internal operation and should never happen
lookupE x g = maybe (error "Dependency.Graph: index not found") id $ x `lookup` g 

-- launches marking from a node to its dependants, folding on the graph
flood :: Ord b => b -> Graph' a b -> Graph' a b
flood x g = let g' = ff (modi MUnlink) ( (existence . status) $ x `lookupE` g) g
  in ff (modi MBuild) ( (logic . status) $ x `lookupE` g') g'
  where ff = flip . foldr 
        
-- change the request/status of a node or stop if already marked and flood the dependants
modi :: Ord b => Modi -> b -> Graph' a b -> Graph' a b

modi MUnlink x g = case x `lookupE` g of
  (Unlink _) -> g 
  z -> flood x $ adjust (Unlink . status) x g 

modi MBuild x g = case x `lookupE` g of
  (Unlink _) -> g
  (Build _) -> g
  z -> flood x $  adjust (Build . status) x g

-- specific value for the nodes, it contains client specified values
data Value a b = Value
  { core :: a
  , index :: b
  , depends :: b -> Bool
  , belongs :: Maybe b
  }

-- more specific graph
type VGraph' a b = Graph' (Value a b) b

-- | Errors that can happen when using indices to refer to resources
data IndexError   = Cycle           -- ^ trying to 'create' a resource causes a cycle following depandants
                  | Duplicate       -- ^ trying to 'create' a resource with an already present index
                  | Unbelonging     -- ^ trying to 'create' a resource with an existence link to a non existing index
                  | Missing         -- ^ trying to 'touch' or 'erase' a resource giving a non existing index
                  deriving (Show,Eq,Typeable)

-- cons a new node, detect cycle or touch it
append :: Ord b => b -> a -> (b -> Bool) -> Maybe b -> VGraph' a b -> Either IndexError (VGraph' a b)
append x y fx mx  g =  do
  when (x `member` g) $ Left  Duplicate
  maybe (return ()) (\x -> when (not $ x `member` g) $ Left Unbelonging) mx
  let   ds = map (index . value . status . snd) . toList . filter (($x) . depends . value . status) $ g
        g' = insert x (Uptodate $ Node (Value y x fx mx) ds []) g
        g'' = fmap (\n -> if fx (index . value $ n) then (nub . (x:)) `fmapLogic` n else n) `fmap` g'
        g''' = case mx of
          Nothing -> g''
          Just z -> fmap (\n -> if (index . value $ n) == z then (nub . (x:)) `fmapExistence` n else n) `fmap` g''
        t = cycleDetect $ map (id *** (\n -> S.fromList $ (logic . status) n ++ (existence . status) n)) $ toList g'''
  if t then Left Cycle else Right $ modi MBuild x g'''

-- remove a node marked as 'Unlink'. It crashes if the node was not requested to
remove :: Ord b => b -> VGraph' a b -> VGraph' a b
remove x g =  let g' = fmap (fmapExistence (\\[x]) . fmapLogic (\\[x])) `fmap` g
              in case x `lookupE` g' of
                (Unlink _) -> x `delete` g'
                _ -> error "Dependency.Graph: removing an unmarked node"

-- | Create method. Index and value associated is given along dependency logic and existential dependency, if present.
type Create  a b 
            = b          -- ^ index for the item
            -> a          -- ^ new item
            -> (b -> Bool)-- ^ mask for dependencies
            -> Maybe b    -- ^ existence dependence 
            -> Either IndexError (Graph a b) -- ^ the updated graph or an error among ['Cycle','Unbelonging','Duplicate']

-- | The item indexed as the argument and its existential dependants will be marked as 'Unlink', logic dependencies are recursively marked for 
-- 'Build' an so on.
type Erase a b
            = b  -- ^ index to be deleted
            -> Either IndexError (Graph a b) -- ^ the updated graph or a 'Missing' error

-- | The item indexed as the argument and its logical dependants will be marked as 'Build'. All items existentially depending on the touched items will be marked as 'Unlink' and so on.
type Touch a b
            = b  -- ^ index to be touched
            -> Either IndexError (Graph a b) -- ^ the updated graph or a 'Missing' error


  
-- | Valid operations when a 'Graph' is in Accept state.
data Operation a b = Operation 
  { create  :: Create a b -- ^ insert a new resource
  , erase   :: Erase a b -- ^ erase a resource
  , touch   :: Touch a b -- ^ touch a resource
  }

-- | A 'Graph' is a 2 state machine. 
-- When in Accept state any of the 3 operations can be performed. 
-- When in Present state a resource is presented in a request, along with the consequent Graph
--
data Graph a b 
  = Accept (Operation a b)    -- ^ graph ready to change
  | Present (Request a,Graph a b) -- ^ graph presenting request to change

-- closes a set of operations on the actual graph
operations :: Ord b => VGraph' a b -> Operation a b
operations g = Operation 
  (\x y fx mx -> step `fmap` append ( x) y fx mx g)
  (\x -> if x `member` g then Right . step $ modi MUnlink ( x) g else Left Missing) 
  (\x -> if x `member` g then Right . step $ modi MBuild ( x) g else Left Missing) 

-- create next Present state given the actual graph. It falls in accept state if everything is uptodate
step :: Ord b => VGraph' a b -> Graph a b
step g = case filter isUnlink g of
  g' -> if null g'  then 
          case filter k g of
            g' -> if null g' then Accept $ operations g
                  else 
                    let (_,x) = findMin g' 
                    in Present ((core . value) `fmap` x, step $ adjust (Uptodate . status) (index . value . status $ x) g)
        else 
          let (_,x) = findMin g' 
          in Present ((core . value) `fmap` x , step $ remove (index . value . status $ x) g)
  where
  k n@(Build x) = all (isUptodate . snd) . toList . filter ((depends . value  $ x) . index . value . status) $ g 
  k _ = False

-- | Create a fresh 'Graph' in 'Accept' state. The graph has no resources indexed.
mkGraph :: Ord b => Graph a b
mkGraph = Accept $ operations empty


