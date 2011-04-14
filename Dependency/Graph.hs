{-# LANGUAGE ViewPatterns, DeriveDataTypeable #-}

module Dependency.Graph ( Status (Unlink, Build)
  , Create
  , create
  , Erase
  , erase
  , Touch
  , touch
  , Step
  , step 
  , Graph
  , mkGraph
  , IndexError (..)
  , Done (..)
  )where
import Data.Map (Map, (!), empty, adjust, member, insert, delete, findWithDefault, lookup, null, filter, findMin, toList)
import Control.Monad (when, foldM, (<=<))
import Prelude hiding (lookup, null, filter)
import Data.List ((\\), break)
import Control.Arrow (second) 
import Debug.Trace
import Data.Typeable

data Node a b = Node 
  { value :: a
  , logic :: [b]
  , existence :: [b]
  }

fmapLogic :: ([b] -> [b]) -> Node a b -> Node a b
f `fmapLogic` Node x ls es = Node x (f ls) es

fmapExistence :: ([b] -> [b]) -> Node a b -> Node a b
f `fmapExistence` Node x ls es = Node x ls $ f es

data Status a 
  = Uptodate {status :: a}
  | Build {status :: a}
  | Unlink {status :: a} deriving (Show,Eq)

isUnlink (Unlink _) = True
isUnlink _ = False

isUptodate (Uptodate x) = True
isUptodate _ = False


instance Functor Status where
  f `fmap` s = s{status = f $ status s}

type Graph' a b = Map b (Status (Node a b))

--
-- marking routine


-- marker type
data Modi = MBuild | MUnlink

flood :: Ord b => Status (Node a b) -> Graph' a b -> Either IndexError (Graph' a b)
flood z = ff (modi MUnlink) (existence . status $ z) <=< ff (modi MBuild) (logic . status $ z)
  where ff f xs t = foldM (flip f) t xs

data IndexError = Duplicate | Unbelonging | InternalMissingKey | InternalRemovingAGoodIndex deriving (Show,Eq,Typeable)

modi :: Ord b => Modi -> b -> Graph' a b -> Either IndexError (Graph' a b)

modi MUnlink x g = case  x `lookup` g of
  Nothing -> Left InternalMissingKey
  Just (Unlink _) -> Right g -- stop this branch
  Just z -> flood z $ adjust (Unlink . status) x g -- mark as unlink and go further

modi MBuild x g = case  x `lookup` g of
  Nothing -> Left InternalMissingKey
  Just (Unlink _) -> Right g -- oh, stop this branch
  Just (Build _) -> Right g -- stop this branch
  Just z -> flood z $ adjust (Build . status) x g

--

data Value a b = Value
  { core :: a
  , index :: b
  , depends :: b -> Bool
  , belongs :: Maybe b
  }

type VGraph' a b = Graph' (Value a b) b

-- cons a new node and touch it
append :: Ord b => b -> a -> (b -> Bool) -> Maybe b -> VGraph' a b -> Either IndexError (VGraph' a b)
append x y fx mx  g= do
  when (x `member` g) $ Left  Duplicate
  maybe (return ()) (\x -> when (not $ x `member` g) $ Left Unbelonging) mx
  let   ds = map (index . value . status . snd) . toList . filter (($x) . depends . value . status) $ g
        g' = insert x (Uptodate $ Node (Value y x fx mx) ds []) g
        g'' = fmap (\n -> if fx (index . value $ n) then (x:) `fmapLogic` n else n) `fmap` g'
        g''' = case mx of
          Nothing -> g''
          Just z -> fmap (\n -> if (index . value $ n) == z then (x:) `fmapExistence` n else n) `fmap` g''
  modi MBuild x g'''

-- remove a node marked as Unlink
remove :: Ord b => b -> VGraph' a b -> Either IndexError (VGraph' a b)
remove x g =  let g' = fmap (fmapExistence (\\[x]) . fmapLogic (\\[x])) `fmap` g
              in case x `lookup` g' of
                Nothing -> Left InternalMissingKey
                Just (Unlink _) -> Right $ x `delete` g'
                Just _ -> Left InternalRemovingAGoodIndex


type ChangeGraph a b =  Either IndexError (Graph a b)


-- | Create method. Index and value associated is given along dependency logic and existential dependency, if present.
type Create  a b 
            = b          -- ^ index for the item
            -> a          -- ^ new item
            -> (b -> Bool)-- ^ mask for dependencies
            -> Maybe b    -- ^ existence dependence 
            -> ChangeGraph a b
-- | All items indexed as the argument and all existential dependants will be marked to yield an Unlink 
type Erase a b
            = b  -- ^ index to be deleted
            -> ChangeGraph a b 
-- | All items indexed as the argument and their logical dependants will be marked to yield a Build. All items existentially depending on the touched items will be marked as Unlink
type Touch a b
            = b  -- ^ index to be touched
            -> ChangeGraph a b 

-- | next programmed operation along the updated manager considering the yielded value
type Step a b = Either Done (Status a, ChangeGraph a b)
  
-- | Abstract Graph object. A bunch of closures over the internal structure.
data Graph a b = Graph 
  { create  :: Create a b -- ^ insert a new node 
  , erase  :: Erase a b -- ^ delete a node
  , touch   :: Touch a b -- ^ touch a node
  , step    :: Step a b -- ^ step the 'Graph'
  }

-- | Things are uptodate
data Done = Done

step' :: Ord b => VGraph' a b -> Either Done (Status a, Either IndexError (VGraph' a b))
step' g = case filter isUnlink g of
  g' -> if null g'  then 
          case filter k g of
            g' -> if null g' then Left Done 
                  else 
                    let (_,x) = findMin g' 
                    in Right ((core . value) `fmap` x, Right $ adjust (Uptodate . status) (index . value . status $ x) g)
        else 
          let (_,x) = findMin g' 
          in Right ((core . value) `fmap` x ,remove (index . value . status $ x) g)
  where
  k n@(Build x) = all (isUptodate . snd) . toList . filter ((depends . value  $ x) . index . value . status) $ g 
  k _ = False



-- | Builds an empty concrete Graph object
mkGraph ::  Ord b => Graph a b
mkGraph = mk empty where
  mk g = Graph  (\x y f mr -> mk `fmap` append x y f mr g) 
                (\x -> mk `fmap` modi MUnlink x g) 
                (\x -> mk `fmap` modi MBuild x g) 
                (second (fmap mk) `fmap` step' g) 
                

