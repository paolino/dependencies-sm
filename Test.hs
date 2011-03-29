{-# LANGUAGE StandaloneDeriving #-}

import Prelude hiding (reverse)
import Control.Monad
import Control.Arrow 
import qualified Data.Set as S
import Data.Set (Set)
import Data.Monoid
import Control.Monad.State
import Control.Monad.Identity
import Data.List hiding (reverse)
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Driver
import Graph
import Debug.Trace


deriving instance Show a  => Show (New a)

complexity = 15

graph :: Gen [New Int]
graph = do
  r <- choose (1,100) -- number of items 
  let f qs r = do
      k <- choose (1,1000) `suchThat` (not . flip elem (map item qs))
      f <- arbitrary
      if r == 0 then return (New k f []:qs) else do 
        ds <- nub <$> replicateM complexity (elements $ map item qs)
        return (New r f ds:qs)
  foldM f [] [0 .. r - 1]

        
newtype Collect a = Collect (Set a,Set a)
instance Ord a => Monoid (Collect a) where
  Collect (x,y) `mappend` Collect (x',y') = Collect (x `mappend` x', y `mappend` y')
  mempty = (Collect (mempty,mempty))

mkGraph :: Ord a => [New a] -> Graph a 
mkGraph = fromList . map (item &&& S.fromList . deps)

testNoPrev = do 
  g <- graph
  let g' = mkGraph g
  let pg = do
        ns <- replicateM complexity (elements g)
        let rs = map item . filter rebuild $ ns
        return (rs,ns)
  monadic (flip evalState mempty) $ do
    let consume Cycle = return False
        consume (Done g'') = run $ gets (\(Collect (x,y)) -> x == reachableNodes y (reverse g''))
        consume (Step f) = let
            q x = do
              (rs,ns) <- pick pg
              run $ modify (`mappend` Collect (S.fromList [x],S.fromList rs))
              return ns 
            in f q >>= consume  
    (rs,ns) <- pick pg
    run $ modify (`mappend` Collect (mempty,S.fromList rs))
    consume $ bootDriver g' ns

main = quickCheck testNoPrev
