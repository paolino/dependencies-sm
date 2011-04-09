{-#LANGUAGE NoMonomorphismRestriction #-}
import Dependency.Core
import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.List
import Debug.Trace
import Data.Char

type TCore = Core String (String , Maybe String)

insertIndipendent :: String -> TCore -> TCore
insertIndipendent l t  = either (error . show) id $ create t l (l,Nothing) (`isPrefixOf` init l) Nothing

insertBelongs:: String -> String -> TCore -> TCore
insertBelongs l s t = either (error . show) id $ create t l (l,Just s) (`isPrefixOf` init l) (Just s)

item :: [String] -> Gen String
item is =  do
    t <- elements ['a' .. 'z']
    elements ([t] : map (++ [t]) is) `suchThat` ( not . flip elem is)

overlappingStrings :: Gen [String]
overlappingStrings = do
  n <- elements [0 .. 100::Int]
  foldM (\is _ -> (: is) `fmap` item is) [] [1..n]

agraph ::  Gen ([String], TCore)
agraph = (id &&& foldr insertIndipendent mkCore) `fmap` nub `fmap`  overlappingStrings



consumeBuilds ::  Core b a -> ([a], Core b a)
consumeBuilds t = case step t of
	Left Done -> ([],t)
	Right (Unlink _,t') -> consumeBuilds t'
	Right (Build x,t') ->  first ( x:) $ consumeBuilds t'

{-
consumeBuildsBl :: [String] -> Core b a -> Gen ([a], Core b a)
consumeBuildsBl xs t = case step t of
	Left Done -> return ([],t)
	Right (Unlink _,t') -> error "unexpected unlink"
	Right (Build x,t') -> do
    y <- elements xs
    x' <- item xs
    let t'' = insertBelongs x' y t'
    first ( x:) `fmap` consumeBuildsBl xs t'
-}

buildsAll = do
	(xs,t) <- agraph
	let (ys,_) =consumeBuilds t
	return (sort xs == sort (map fst ys))			

buildsRight = do
    (xs,t) <- agraph
    let (ys',_) = consumeBuilds t
    let ys = map fst ys'
    let zs = zip ys (tails ys) 
    return (not $ any (\(x,ys) -> any (`isPrefixOf` init x) ys) zs)

oneTouch =  do
      (xs,t) <-  agraph
      let (ys',t') = consumeBuilds t
      fmap (all id) . forM xs $ \x -> do 
        let t'' = touch t' x
        let (ys',_) = consumeBuilds t''
        let ys = map fst ys'
        return $ all (\(x,ys) ->  any (`isPrefixOf` init x) ys) $ tail $ zip ys (inits ys)



main = mapM_ quickCheck [buildsAll, buildsRight, oneTouch]
