-- | Useful generic stuff

module Dependency.Lib (cycleDetect) where

import Data.Set (Set,member,union,fromList)
import Control.Arrow (second)


-- | detect a cycle in a graph
cycleDetect :: Ord a
              => [(a,Set a)] -- ^ the graph
              -> Bool -- ^ True if a cycle was detected
cycleDetect [] = False
cycleDetect (xt@(x,xs) :ys) = x `member` xs || cycleDetect (map f ys) where
  f = second $ \xs' -> if x `member` xs' then xs' `union` xs else xs'