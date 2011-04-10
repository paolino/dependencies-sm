{-#LANGUAGE NoMonomorphismRestriction #-}
import Dependency.Core
import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.List
import Debug.Trace
import Data.Char

type TCore = Core String (String , Maybe String)

------------------------------------------------------------------
-- logic links
-- -----------------------------------------------------------------
--
--
testLogic =  
    let 
      insertIndipendent :: String -> TCore -> TCore
      insertIndipendent l t  = either (error . show) id $ create t l (l,Nothing) (`isPrefixOf` init l) Nothing



      item :: [String] -> Gen String
      item is =  do
          t <- elements ['a' .. 'z']
          elements ([t] : map (++ [t]) is) `suchThat` ( not . flip elem is)

      overlappingStrings :: Gen [String]
      overlappingStrings = do
        n <- elements [0 .. 60::Int]
        foldM (\is _ -> (: is) `fmap` item is) [] [1..n]

      agraph ::  Gen ([String], TCore)
      agraph = (id &&& foldr insertIndipendent mkCore) `fmap` nub `fmap`  overlappingStrings



      consumeBuilds ::  Core b a -> ([a], Core b a)
      consumeBuilds t = case step t of
        Left Done -> ([],t)
        Right (Unlink _,t') -> consumeBuilds t'
        Right (Build x,t') ->  first ( x:) $ consumeBuilds t'


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
              let zs = x: filter (\y -> x `isPrefixOf` init y) xs
              return $  all (\(x,ys) ->  any (`isPrefixOf` init x) ys) (tail $ zip ys (inits ys)) && sort ys == sort zs
    in buildsAll .&. buildsRight .&. oneTouch

-----------------------------------------------------------------------------------------------------------------------
--- existential links
------------------------------------------


insertBelongs :: String -> Maybe String -> TCore -> TCore
insertBelongs l s t = either (error . show) id $ create t l (l,s) (const False) s


item :: [String] -> Gen (String,Maybe String)
item is =  do
  t <- elements ['a' .. 'z']
  n <- elements ([t] : map (++ [t]) is) `suchThat` ( not . flip elem is)
  k <- case null is of
    False -> Just `fmap` elements is -- existential dependence
    True -> return Nothing
  return (n,k)

agraph = do
  n <- elements [0 .. 60::Int] -- number of items
  ns <- foldM (\xs _ -> (:xs) `fmap` item (map fst xs) ) [] [1 .. n] -- different items
  return $ foldr (\(x,y) t -> insertBelongs x y t) mkCore ns



{-
consumeBuildsBl :: [String] -> Core b a -> Gen ([(a, String, String)], Core b a)
consumeBuildsBl xs t = case step t of
	Left Done -> return ([],t)
	Right (Unlink _,t') -> error "unexpected unlink"
	Right (Build x,t') -> do
        y <- elements xs
        x' <- item xs
        first ((x,y,x'):) `fmap` consumeBuildsBl xs t'


aBlGraph = do 
  (xs,t) <- agraph
  (ys,t') <- consumeBuildsBl xs t' buildsAll, buildsRight, oneTouch
  return $ foldr (\(x,y,x') t -> insertBelongs x' y t)

        let (ys',_) = consumeBuilds t''
        let ys = map fst ys'
-}



main = mapM_ quickCheck [testLogic]
