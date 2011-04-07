{-#LANGUAGE NoMonomorphismRestriction #-}
import Dependency.Core
import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.List
import Debug.Trace
import Data.Char

insertIndipendent (l,s) t  = either (error . show) id $ create t l s (`isPrefixOf` init l) Nothing

item :: [String] -> Gen String
item is =  do
    t <- elements ['a' .. 'z']
    elements ([t] : map (++ [t]) is) `suchThat` ( not . flip elem is)

overlappingStrings :: Gen [String]
overlappingStrings = do
  n <- elements [0 .. 100::Int]
  foldM (\is _ -> (: is) `fmap` item is) [] [1..n]

agraph = (map snd &&& foldr insertIndipendent mkCore) `fmap` nub `fmap`  map (id &&& id) `fmap` reverse `fmap` overlappingStrings

consumeBuilds t = case step t of
	(Empty,t') -> ([],t')
	(Uptodate _,t') -> ([],t')
	(Unlink _,t') -> consumeBuilds t'
	(Build x,t') ->  first ( x:) $ consumeBuilds t'

buildsAll = do
	(xs,t) <- agraph
	let (ys,_) =consumeBuilds t
	return (sort xs == sort ys)			

buildsRight = do
    (xs,t) <- agraph
    let (ys,_) = consumeBuilds t
    let zs = zip ys (tails ys) 
    return (not $ any (\(x,ys) -> any (`isPrefixOf` init x) ys) zs)

oneTouch =  do
      (xs,t) <-  agraph
      let (ys,t') = consumeBuilds t
      fmap (all id) . forM xs $ \x -> do 
        let t'' = touch t' x
        let (ys',_) = consumeBuilds t''
        return $ all (\(x,ys) ->  any (`isPrefixOf` init x) ys) $ tail $ zip ys' (inits ys')

main = mapM_ quickCheck [buildsAll, buildsRight, oneTouch]

