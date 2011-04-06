{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
-- | Core dependency manager.
module Dependency.Core
  ( Yield (..)
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


-- Logic dependencies layer
data Logic b = Logic {
  dependencies :: b -> Bool,
  dependants :: Set b
  }

-- Existence dependency layer
data Existence b = Existence {
  belongs :: Maybe b,
  holds :: Set b
  }

-------------------------------------------

-- A dependency node.
data Node b a = Node {
  index :: b,
  value :: a,
  logic :: Logic b,
  existence :: Existence b
  }

instance Functor (Node b) where
  fmap f n = n{value = f $ value n}

type Nodes b a = [Node b a]

-- O(n). cons a new node after collecting its dependants. 
insertNode :: Ord b => Nodes b a  -> b -> a -> (b -> Bool) -> Maybe b -> Nodes b a
insertNode ns x y f mr 
  | x `elem` map index ns = error "Dependency.Core: multiple values for an index are not supported."
  | otherwise             = Node x y (Logic f qs) (Existence mr mempty):ns where
      qs = fromList . map index $ filter (($x) . dependencies . logic) $ ns


flood :: Ord b =>  (Node b a -> Set b) -> Maybe (a -> a) -> Set b -> Nodes b a  -> Nodes b a
flood s t xs ns 
            | null xs = ns 
            | True = let 
              x = findMin xs
              (xs',ns') = mapAccumL f xs ns 
              f ts n 
		            | index n == x = (ts `union` s n, flip fmap n `fmap` t)
		            | otherwise = (ts, Just n)
              in flood s t (S.delete x xs') (catMaybes ns')

deleteNodes :: Ord b => Set b -> Nodes b a  -> Nodes b a
deleteNodes  = flood (holds . existence) Nothing

modifyHolds :: Ord b => (a -> a) -> Set b -> Nodes b a  -> Nodes b a
modifyHolds f = flood (holds . existence) (Just f)

modifyDependants :: Ord b =>  (a -> a) -> Set b -> Nodes b a  -> Nodes b a
modifyDependants  f = flood (dependants . logic) (Just f)

-----------------------------------------------------------------
type NodeT b a = Node b (Yield a)
type NodesT b a = Nodes b (Yield a)

--- mark rules ---------------------------
setBuild ::  Yield a -> Yield a
setBuild (Uptodate x) = Build x
setBuild y = y

setUnlink ::  Yield a -> Yield a
setUnlink (Uptodate x) = Unlink x
setUnlink (Build x) = Unlink x
setUnlink x = x

setUptodate ::  Yield a -> Yield a
setUptodate (Build x) = Uptodate x
setUptodate (Unlink x) = Unlink x
setUptodate x = x

isUptodate ::  Yield a -> Bool
isUptodate (Uptodate x) = True
isUptodate _ = False

-----------------------------------------------
--
-- Helper methods for Core object
--
----------------------------------------------


create' :: Ord b =>  NodesT b a -> b -> a -> (b -> Bool) -> Maybe b ->  NodesT b a
create' ns x y f mr = modifyDependants setBuild (fromList [x]) $ insertNode ns x (Build y) f mr 

delete' :: Ord b =>  NodesT b a -> b -> NodesT b a
delete' ns x = modifyHolds setUnlink (fromList [x]) $ ns

touch' :: Ord b =>  NodesT b a -> b -> NodesT b a
touch' ns x = modifyDependants setBuild (fromList [x]) . modifyHolds setUnlink ds $ ns where
  ds = unions . map (holds . existence) . filter ((== x) . index) $ ns

step' :: NodesT b a -> (Yield a, NodesT b a)
step' [] = (Empty,[])
step' ns@(n:ns') = case break g ns of
  (_,[]) -> (value n,ns' ++ [n])
  (fs,n:ss) -> case value n of
    d@(Unlink _) -> (d,fs ++ ss)
    b@(Build _) -> (b,fs ++ ss ++ [fmap setUptodate n])
  where
  g (value -> Unlink _) = True
  g n@(value -> Build _) = all (isUptodate . value) . filter ((dependencies . logic $ n) . index ) $ ns
  g _ = False

------------------------------------------------------------------------------------------------------
-- Interface.
------------------------------------------------------------------------------------------------------


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

-- | Build an empty concrete Core object
mkCore = mk [] where
  mk ns = Core (\x y f mr -> mk $ create' ns x y f mr) (mk . delete' ns) (mk . touch' ns) (second mk $ step' ns)

