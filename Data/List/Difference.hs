-- | A datatype to track changes for a list of indices
module Data.List.Difference where

import Data.Monoid (Monoid (..), mempty, mappend)
import Data.List ((\\), nub, intersect)

-- | Difference datatype containing a difference as three sets of indices.
data Difference a = Difference {
  created :: [a], -- ^ elements appeared
  deleted :: [a], -- ^ elements disappeared
  modified :: [a] -- ^ elements touched
  } deriving (Show, Eq)


instance Functor Difference where
  fmap f (Difference xs ys zs) = Difference (map f xs) (map f ys) (map f zs)


-- half correct instance. It forces files which have been deleted and created to be marked as modifications. It's not correct as a delete after a create is not a modification. 
instance Eq a => Monoid (Difference a) where
  Difference n d m `mappend` Difference n' d' m' = let
    mm = nub $ m ++ m'
    nn = nub $ n ++ n'
    dd = nub $ d ++ d' 
    in Difference ((nn \\ dd) \\ mm) ((dd \\ nn) \\ mm) (nub $ mm ++ intersect nn dd)
  mempty = Difference [] [] []

  



