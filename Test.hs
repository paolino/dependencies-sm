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

noone = const False
anyone = const True
eq = (==)
testDrawn s xs f = assertEqual s xs (drawn . crashOnLeft $ f mkGraph)
testError s i f  = assertBool s . matchError i $ f mkGraph

before = (fmap drawnG .)


t0 = testDrawn "null" [Build ()] $ create' () noone Nothing
t1 = testError "null, cycle logic" Cycle $ create' () anyone Nothing
t2 = testError "null, unbeloning" Unbelonging $ create' () noone (Just ())
t3 = testError "null, cycle logic, unbeloning" Unbelonging $ create' () anyone (Just ())

t4 = testDrawn "multi" [Build 1, Build 2] $ create' 1 noone Nothing >=> create' 2 (<2) Nothing
t5 = testDrawn "multi, reorder" [Build 1, Build 2] $ create' 1 noone Nothing <=< before (create' 2 (<2) Nothing)
t6 = testError "multi, duplicate" Duplicate $ create' 1 noone Nothing <=< create' 1 noone Nothing
t7 = testDrawn "null, touch" [Build ()] $ (before $ create' () noone Nothing) >=> touch' ()

t8 = testDrawn "multi, touch" [Build 1, Build 2] $ (before $ create' 1 noone Nothing >=> create' 2 (<2) Nothing) >=> touch' 1
t9 = testDrawn "multi, touch part" [Build 2] $ before (create' 1 noone Nothing >=> create' 2 (<2) Nothing) >=> touch' 2

t10 = testDrawn "multi, existential" [Build 2] $ before (create' 1 noone Nothing) >=> create' 2 noone (Just 1)
t11 = testDrawn "multi, existential, touch" [Unlink 2, Build 1] $ before (create' 1 noone Nothing) >=> before (create' 2 noone (Just 1))
  >=> touch' 1

t12 =  testDrawn "multi, mixed , touch" [Unlink 2, Build 1, Build 3] $ 
  before (create' 1 noone Nothing) >=> before (create' 2 noone (Just 1) >=> create' 3 (eq 1) Nothing) >=> touch' 1
t13 =  testDrawn "multi, mixed , touch" [Unlink 2, Build 1, Build 3] $ 
  before (create' 1 noone Nothing) >=> before (create' 2 noone (Just 1) >=> create' 3 (eq 2) Nothing) >=> touch' 1
t14 =  testDrawn "null, delete" [Unlink ()] $ 
  before (create' () noone Nothing) >=> erase' ()

t15 =  testDrawn "multi, logic" [Unlink 1, Build 2] $ 
  before (create' 1 noone Nothing >=> create' 2 (eq 1) Nothing) >=> erase' 1
t16 =  testDrawn "multi, existential " [Unlink 1, Unlink 2] $ 
  before (create' 1 noone Nothing) >=> before (create' 2 noone (Just 1)) >=> erase' 1
t17 =  testError "multi, existential, belonging to a not ready node" BelongingToNotUptodate $ 
  create' 1 noone Nothing >=> create' 2 noone (Just 1)


ts = TestList $ map TestCase [t0,t1,t2,t3,t4,t5,t6,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17]

main = runTestTT ts
