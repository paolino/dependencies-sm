{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns #-}
-- | Core dependency manager.
module Dependency.Core
  ( Yield (Unlink, Build)
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
  , IndexError (..)
  , Done (..)
  , dump
  )where

import Control.Arrow (second, (&&&), (***), (>>>))
import Control.Monad (when, forM)
import Data.Set (Set, fromList, toList, union, unions,  findMin,  null, member)
import qualified Data.Set as S
import Data.List (mapAccumL, find)
import Data.Monoid (mempty)
import Data.Maybe (catMaybes)
import Prelude hiding (null)
import Debug.Trace

import Dependency.Lib

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

addDeps n x = n{logic = let q = logic n in q{dependants = x `S.insert` dependants q}}
addHolds n x = n{existence = let q = existence n in q{holds = x `S.insert` holds q}}

data IndexError = Cycle | Duplicate | Unbelonging deriving Show

-- 
insertNode :: Ord b => Nodes b a -> b -> a -> (b -> Bool) -> Maybe b -> Either IndexError (Nodes b a)
insertNode ns x y f mr 
  | x `elem` map index ns = Left Duplicate
  | f x = Left Cycle
  | otherwise  = do
                ns'' <- case mr of
                    Nothing -> return ns'
                    Just r -> let (t,ns'') = z r in
                        if t then return ns'' else Left Unbelonging
                let   g = Node x y (Logic f $ fromList qs) (Existence mr mempty):ns'' 
                      gd = map (index &&& dependants . logic) g
                      gq = map (index &&& holds . existence) g
                when (cycleDetect gd || cycleDetect gq) $ Left Cycle
                return g
      where
      (qs,ns') = mapAccumL g [] ns
      onC t f = if t then f else id
      g is n =  onC (($x) . dependencies . logic $ n) ((index n :) *** id) . onC (f $ index n) (id *** flip addDeps x) $ (is,n)
      z e = mapAccumL h False ns'
        where h c n = if index n == e then (True,addHolds n x) else (c,n)


flood :: Ord b =>  (Node b a -> Set b) -> Maybe (a -> a) -> Set b -> Nodes b a  -> Nodes b a
flood s t xs ns 
            | null xs = ns 
            | True =   let 
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

-- | Yielded values from a manager. The constructor tagging mark the item for the client to act accordingly
data Yield a 
  = Uptodate a -- ^ nothing to do
  | Unlink a -- ^ this item must be eliminated
  | Build a -- ^ this item must be built
  deriving Show

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

-- | Things are uptodate
--
data Done = Done

create' :: Ord b =>  NodesT b a -> b -> a -> (b -> Bool) -> Maybe b -> Either IndexError (NodesT b a)
create' ns x y f mr = modifyDependants setBuild (fromList [x]) `fmap` insertNode ns x (Build y) f mr

delete' :: Ord b =>  NodesT b a -> b -> NodesT b a
delete' ns x = modifyHolds setUnlink (fromList [x]) $ ns

touch' :: Ord b =>  NodesT b a -> b -> NodesT b a
touch' ns x = modifyDependants setBuild (fromList [x]) . modifyHolds setUnlink ds $ ns where
  ds = unions . map (holds . existence) . filter ((== x) . index) $ ns

step' :: NodesT b a -> Either Done (Yield a, NodesT b a)
step' [] = Left Done
step' ns@(n:ns') = case break g ns of
  (_,[]) -> Left Done 
  (fs,n:ss) -> case value n of
    d@(Unlink _) -> Right (d,fs ++ ss)
    b@(Build _) -> Right (b,fs ++ ss ++ [fmap setUptodate n])
  where
  g (value -> Unlink _) = True
  g n@(value -> Build _) = all (isUptodate . value) . filter ((dependencies . logic $ n) . index ) $ ns
  g _ = False

------------------------------------------------------------------------------------------------------
-- Interface.
------------------------------------------------------------------------------------------------------


-- | Create method. Index and value associated is given along dependency logic and existential dependency, if present.
type Create b a 
            = b          -- ^ index for the item
            -> a          -- ^ new item
            -> (b -> Bool)-- ^ mask for dependencies
            -> Maybe b    -- ^ existence dependence 
            -> Either IndexError (Core b a)
-- | All items indexed as the argument and all existential dependants will be marked to yield an Unlink 
type Delete b a 
            = b  -- ^ index to be deleted
            -> Core b a
-- | All items indexed as the argument and their logical dependants will be marked to yield a Build. All items existentially depending on the touched items will be marked as Unlink
type Touch b a 
            = b  -- ^ index to be touched
            -> Core b a

-- | next programmed operation along the updated manager considering the yielded value
type Step b a = Either Done (Yield a, Core b a)
  
-- | Abstract Core object. A bunch of closures over the internal structure.
data Core b a = Core 
  { create  :: Create b a 
  , delete  :: Delete b a
  , touch   :: Touch b a
  , step    :: Step b a
  , dump    :: [(Yield a,Set b)]
  }

-- | Build an empty concrete Core object
mkCore = mk [] where
  mk ns = Core  (\x y f mr -> mk `fmap` create' ns x y f mr) 
                (mk . delete' ns) 
                (mk . touch' ns) 
                (second mk `fmap` step' ns) 
                (map (value &&& dependants . logic) ns)

