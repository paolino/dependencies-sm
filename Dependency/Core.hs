{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
-- | Core dependency manager.
module Dependency.Core
  (Yield (..)
  , Create
  , create
  , Delete
  , delete
  , Touch
  , touch
  , Step
  , step 
  , Core
  , mkCore
  )where

import Control.Arrow (second)
import Data.Set (Set, fromList, toList, union, unions,  findMin,  null)
import qualified Data.Set as S
import Data.List (mapAccumL, find)
import Data.Monoid (mempty)
import Data.Maybe (catMaybes)
import Prelude hiding (null)

data Logic b a = Logic {
  logic :: a,
  dependencies :: b -> Bool,
  dependants :: Set b
  }

instance Functor (Logic b) where
  fmap f (Logic x y z) = Logic (f x) y z


data Existence b a = Existence {
  existence :: a ,
  belongs :: Maybe b,
  holds :: Set b
  }

instance Functor (Existence b) where
  fmap f (Existence x y z) = Existence (f x) y z


-------------------------------------------

data Node b a = Node {
  index :: b,
  load :: a
  }
instance Functor (Node b) where
  fmap f (Node x l) = Node x (f l)

type NodesL b a = [NodeL b a]

type NodeL b a = Node b (Existence b (Logic b a))

insertNode :: Ord b => NodesL b a  -> b -> a -> (b -> Bool) -> Maybe b -> NodesL b a
insertNode ns x y f mr = Node x (Existence (Logic  y f qs) mr mempty):ns where
  qs  = fromList . map index $ filter (($x) . dependencies . existence . load) $ ns


flood :: Ord b =>  (NodeL b a -> Set b) -> Maybe (a -> a) -> Set b -> NodesL b a  -> NodesL b a
flood s t xs ns 
            | null xs = ns 
            | True = let 
              x = findMin xs
              (xs',ns') = mapAccumL f xs ns 
              f ts n | index n == x = let
                in (ts `union` s n, (\f -> fmap (fmap (fmap f)) n) `fmap` t)
              in flood s t  (S.delete x xs') (catMaybes ns')

deleteNodes :: Ord b => Set b -> NodesL b a  -> NodesL b a
deleteNodes  = flood (holds . load) Nothing

modifyHolds :: Ord b => (a -> a) -> Set b -> NodesL b a  -> NodesL b a
modifyHolds f = flood (holds . load) (Just f)

modifyDependants :: Ord b =>  (a -> a) -> Set b -> NodesL b a  -> NodesL b a
modifyDependants  f = flood (dependants . existence . load) (Just f)

-- | Yielded values from a manager. The constructor tagging mark the item for the client to act accordingly
data Yield a 
  = Uptodate a -- ^ this item is uptodate
  | Unlink a -- ^ this item must be eliminated
  | Build a -- ^ this item must be built
  | Empty -- ^ the manager has no item to manage

-- | Create method. Index and value associated is given along dependency logic and existential dependency, if present.
type Create b a 
            = b          -- ^ index for the item
            -> a          -- ^ new item
            -> (b -> Bool)-- ^ mask for dependencies
            -> Maybe b    -- ^ existence dependence 
            -> Core b a
-- | All items indexed as the argument and all existential dependants will be marked to yield an Unlink 
type Delete b a 
            = b  -- ^ index to be deleted
            -> Core b a
-- | All items indexed as the argument and their logical dependants will be marked to yield a Build. All items existentially depending on the touched items will be marked as Unlink
type Touch b a 
            = b  -- ^ index to be touched
            -> Core b a

-- | next programmed operation along the updated manager considering the yielded value
type Step b a = (Yield a, Core b a)
  
-- | Abstract Core object. A bunch of closures over the internal structure.
data Core b a = Core 
  { create  :: Create b a 
  , delete  :: Delete b a
  , touch   :: Touch b a
  , step    :: Step b a
  }


-----------------------------------------------------------------
type NodeT b a = NodeL b (Yield a)
type NodesT b a = NodesL b (Yield a)


setBuild (Uptodate x) = Build x
setBuild y = y

setUnlink (Uptodate x) = Unlink x
setUnlink (Build x) = Unlink x
setUnlink x = x

setUptodate (Build x) = Uptodate x
setUptodate (Unlink x) = Unlink x
setUptodate x = x

isUptodate (Uptodate a) = True
isUptodate _ = False

getTainted = logic . existence . load


create' :: Ord b =>  NodesT b a -> b -> a -> (b -> Bool) -> Maybe b ->  NodesT b a
create' ns x y f mr = insertNode ns x (Build y) f mr 

delete' :: Ord b =>  NodesT b a -> b -> NodesT b a
delete' ns x = modifyHolds setUnlink (fromList [x]) $ ns

touch' :: Ord b =>  NodesT b a -> b -> NodesT b a
touch' ns x = let 
  ts = filter ((== x) . index) ns
  ds = unions $ map (holds . load) ts
  in modifyDependants setBuild (fromList [x]) . modifyHolds setUnlink ds $ ns

step' :: NodesT b a -> (Yield a, NodesT b a)
step' [] = (Empty,[])
step' ns@(n:ns') = case break g ns of
  (_,[]) -> (getTainted n,ns' ++ [n])
  (fs,n:ss) -> case getTainted n of
    d@(Unlink _) -> (d,fs ++ ss)
    b@(Build _) -> (b,fs ++ ss ++ [fmap (fmap (fmap setUptodate)) n])
  where
  g (getTainted -> Unlink _) = True
  g n@(getTainted -> Build _) = let
    deps = dependencies . existence . load $ n
    rs = filter (deps . index) ns
    in all (isUptodate . getTainted) rs
  g _ = False

mkCore = mk [] where
  mk ns = Core (\x y f mr -> mk $ create' ns x y f mr) (mk . delete' ns) (mk . touch' ns) (second mk $ step' ns)

