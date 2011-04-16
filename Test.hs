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
drawn (Present (x,g)) = first (x:) $ drawn g

create :: a -> (a -> Bool) -> Maybe a -> Graph a a -> ChangeGraph a a
create x fx mx (Accept t) = G.create t x x fx mx 

erase ::  b -> Graph a b -> ChangeGraph a b
erase x (Accept t) =  G.erase t x

touch ::  b -> Graph a b -> ChangeGraph a b
touch x (Accept t) = G.touch t x

matchError :: Eq t => t -> Either t a -> Bool
matchError i x = case x of
  Left j -> j == i
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

testError :: Ord b => String -> IndexError -> [Graph a b -> ChangeGraph a b]  ->  Assertion
testError s i = assertBool s . matchError i . multiDrawn 



t1 = testError "null, cycle logic" Cycle 
  [ create () anyone Nothing
  ]
t2 = testError "null, cycle mixed" Cycle 
  [ create 1 (== 3) Nothing
  , create 3 (== 2) Nothing
  , create 2 noone (Just 1)
  ]

t3 = testError "null, unbeloning" Unbelonging 
  [ create () noone (Just ())
  ]
t4 = testError "null, cycle logic, unbeloning" Unbelonging 
  [ create () anyone (Just ())
  ]
t5 = testError "multi, duplicate" Duplicate 
  [ create 1 noone Nothing
  , create 1 noone Nothing
  ]

t6 = testDrawn "multi" 
  [ (create 1 noone Nothing, [Build 1])
  , (create 2 (eq 1) Nothing, [Build 2])
  ]

t7 = testDrawn "multi, reorder" 
  [ (create 2 (eq 1) Nothing, [Build 2])
  , (create 1 noone Nothing, [Build 1, Build 2])
  ] 


t8 = testDrawn "multi, touch"  
  [ (create 1 noone Nothing, [Build 1])
  , (touch 1,[Build 1])
  , (create 2 (<2) Nothing, [ Build 2])
  , (touch 1, [Build 1, Build 2])
  , (touch 2, [Build 2])
  ]

t9 = testDrawn "multi, existential" 
  [ (create 1 noone Nothing, [Build 1])
  , (create 2 (== 3) (Just 1), [Build 2])
  , (touch 2, [Build 2])
  , (create 3 noone Nothing, [Build 3, Build 2])
  , (touch 1, [Unlink 2, Build 1])
  ]

  
t10 = testDrawn "multi, erasing" 
  [ (create 1 noone Nothing, [Build 1])
  , (erase 1, [Unlink 1])
  , (create 1 noone Nothing, [Build 1])
  , (create 2 (== 1) Nothing, [Build 2])
  , (erase 2, [Unlink 2])
  , (create 2 (== 1) Nothing, [Build 2])
  , (erase 1, [Unlink 1, Build 2])
  ]
  
t11 = testDrawn "multi, erasing" 
  [ (create 1 noone Nothing, [Build 1])
  , (create 2 noone (Just 1), [Build 2])
  , (erase 1, [Unlink 1, Unlink 2])
  ]

t12 = testDrawn "complex the first" 
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

ts = TestList $ map TestCase [t1,t2,t3,t4,t5,t6,t7,t8,t9,t10, t11, t12]

main = runTestTT ts
