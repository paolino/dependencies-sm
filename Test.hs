{-# LANGUAGE StandaloneDeriving, NoMonomorphismRestriction, ViewPatterns #-}
import Dependency.Graph (Graph (..), Request (..), Operation , IndexError (..))
import qualified Dependency.Graph as G
import Control.Monad
import Control.Arrow
import Control.Applicative
import Data.List hiding (delete)
import Test.HUnit



type ChangeGraph a b = Either IndexError (Graph a b)

crashOnLeft :: Show a => Either a b -> b
crashOnLeft = either (error . show) id

drawn :: Graph a b -> ([Request a],Graph a b)
drawn g@(Accept _) = ([],g)
drawn (Run (x,g)) = first (x:) $ drawn g

create :: a -> (a -> Bool) -> Maybe a -> Graph a a -> ChangeGraph a a
create x fx mx (Accept t) = G.create t x x fx mx 

erase ::  b -> Graph a b -> ChangeGraph a b
erase x (Accept t) =  G.erase t x

touch ::  b -> Graph a b -> ChangeGraph a b
touch x (Accept t) = G.touch t x

matchError ::  t -> Either t1 t2 -> Bool
matchError i x = case x of
  Left i -> True
  _ -> False

noone ::  b -> Bool
noone = const False

anyone ::  b -> Bool
anyone = const True

eq ::  Eq a => a -> a -> Bool
eq = (==)

multiDrawn :: Ord b => [Graph a b -> ChangeGraph a b] -> Either IndexError ([[Request a]], Graph a b)
multiDrawn = foldM (\(ys,g) f -> first (\y -> ys ++[y]) . drawn <$> f g ) ([], G.mkGraph)

testDrawn :: (Show a , Eq a, Ord b) => String -> [(Graph a b -> ChangeGraph a b, [Request a])] ->  Assertion
testDrawn s (unzip -> (fs,xs)) = assertEqual s xs . fst $ crashOnLeft $ multiDrawn fs

testError :: Ord b => String -> t -> ChangeGraph a b ->  Assertion
testError s i = assertBool s . matchError i 



t1 = testError "null, cycle logic" Cycle (create () anyone Nothing  G.mkGraph)
t1a = testError "null, cycle mixed" Cycle (snd <$> multiDrawn [create 1 (== 3) Nothing,create 3 (== 2) Nothing, create 2 noone (Just 1)])

t2 = testError "null, unbeloning" Unbelonging (create () noone (Just ()) G.mkGraph)
t3 = testError "null, cycle logic, unbeloning" Unbelonging (create () anyone (Just ()) G.mkGraph)

t4 = testDrawn "multi" 
  [ (create 1 noone Nothing, [Build 1])
  , (create 2 (eq 1) Nothing, [Build 2])
  ]

t18 = testDrawn "complex the first" 
  [ (create 2 (/= 2) Nothing, [Build 2])
  , (create 1 noone Nothing, [Build 1,Build 2])
  , (create 3 noone Nothing, [Build 3, Build 2] )
  , (create 4 noone (Just 1), [Build 4, Build 2])
  , (touch 1, [Unlink 4, Build 1,Build 2])
  , (touch 3, [Build 3,Build 2])
  , (erase 1, [Unlink 1,Build 2])
  , (erase 3, [Unlink 3,Build 2])
  , (erase 2, [Unlink 2])
  ]
{-
t5 = testDrawn "multi, reorder" [Build 1, Build 2] $ create 1 noone Nothing <=< before (create 2 (<2) Nothing)
t6 = testError "multi, duplicate" Duplicate $ create 1 noone Nothing <=< create 1 noone Nothing
t7 = testDrawn "null, touch" [Build ()] $ (before $ create () noone Nothing) >=> touch' ()

t8 = testDrawn "multi, touch" [Build 1, Build 2] $ (before $ create 1 noone Nothing >=> create 2 (<2) Nothing) >=> touch' 1
t9 = testDrawn "multi, touch part" [Build 2] $ before (create 1 noone Nothing >=> create 2 (<2) Nothing) >=> touch' 2

t10 = testDrawn "multi, existential" [Build 2] $ before (create 1 noone Nothing) >=> create 2 noone (Just 1)
t11 = testDrawn "multi, existential, touch" [Unlink 2, Build 1] $ before (create 1 noone Nothing) >=> before (create 2 noone (Just 1))
  >=> touch' 1

t12 =  testDrawn "multi, mixed , touch" [Unlink 2, Build 1, Build 3] $ 
  before (create 1 noone Nothing) >=> before (create 2 noone (Just 1) >=> create 3 (eq 1) Nothing) >=> touch' 1
t13 =  testDrawn "multi, mixed , touch" [Unlink 2, Build 1, Build 3] $ 
  before (create 1 noone Nothing) >=> before (create 2 noone (Just 1) >=> create 3 (eq 2) Nothing) >=> touch' 1
t14 =  testDrawn "null, delete" [Unlink ()] $ 
  before (create () noone Nothing) >=> erase' ()

t15 =  testDrawn "multi, logic" [Unlink 1, Build 2] $ 
  before (create 1 noone Nothing >=> create 2 (eq 1) Nothing) >=> erase' 1
t16 =  testDrawn "multi, existential " [Unlink 1, Unlink 2] $ 
  before (create 1 noone Nothing) >=> before (create 2 noone (Just 1)) >=> erase' 1
t17 =  testError "multi, existential, belonging to a not ready node" BelongingToNotUptodate $ 
  create 1 noone Nothing >=> create 2 noone (Just 1)

-}
ts = TestList $ map TestCase [t1,t1a,t2,t3,t4,t18]

main = runTestTT ts
