{-# LANGUAGE StandaloneDeriving #-}
import Dependency.Graph
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List hiding (delete)
import Test.HUnit


crashOnLeft = either (error . show) id
drawn = unfoldr (either (const Nothing) (Just . second crashOnLeft) . step)
drawnG g =  case unfoldr (either (const Nothing) (Just . (id &&& id) . crashOnLeft . snd) . step) g of
  [] -> g
  xs -> last xs

create' x fx mx t = create t x x fx mx 
erase' = flip erase
touch' = flip touch

matchError i x = case x of
  Left i -> True
  _ -> False

testDrawn s xs f = assertEqual s xs (drawn . crashOnLeft $ f mkGraph)
testError s i f  = assertBool s . matchError i $ f mkGraph
t0 = testDrawn "null" [Build ()] $ create' () (const False) Nothing
t1 = testDrawn "null, cycle logic" [] $ create' () (const True) Nothing
t2 = testError "null, unbeloning" Unbelonging $ create' () (const False) (Just ())
t3 = testError "null, cycle logic, unbeloning" Unbelonging $ create' () (const True) (Just ())

t4 = testDrawn "multi" [Build 1, Build 2] $ create' 1 (<1) Nothing >=> create' 2 (<2) Nothing
t5 = testDrawn "multi, reorder" [Build 1, Build 2] $ create' 1 (<1) Nothing <=< create' 2 (<2) Nothing
t6 = testError "multi, duplicate" Duplicate $ create' 1 (<1) Nothing <=< create' 1 (<1) Nothing

t7 = testDrawn "null, touch" [Build ()] $ fmap drawnG . create' () (const False) Nothing >=> touch' ()

t8 = testDrawn "multi, touch" [Build 1, Build 2] $ fmap drawnG . (create' 1 (<1) Nothing >=> create' 2 (<2) Nothing) >=> touch' 1
t9 = testDrawn "multi, touch part" [Build 2] $ fmap drawnG . (create' 1 (<1) Nothing >=> create' 2 (<2) Nothing) >=> touch' 2

t10 = testDrawn "multi, existential" [Build 1, Build 2] $ create' 1 (<1) Nothing >=> create' 2 (const False) (Just 1)
t11 = testDrawn "multi, existential, touch" [Unlink 2, Build 1] $ fmap drawnG . (create' 1 (<1) Nothing >=> create' 2 (const False) (Just 1))
  >=> touch' 1
t12 =  testDrawn "multi, mixed , touch" [Unlink 2, Build 1, Build 3] $ 
  fmap drawnG . (create' 1 (<1) Nothing >=> create' 2 (const False) (Just 1) >=> create' 3 ((==) 1) Nothing) >=> touch' 1
t13 =  testDrawn "multi, mixed , touch" [Unlink 2, Build 1, Build 3] $ 
  fmap drawnG . (create' 1 (<1) Nothing >=> create' 2 (const False) (Just 1) >=> create' 3 (<3) Nothing) >=> touch' 1
