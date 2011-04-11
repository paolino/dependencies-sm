{-#LANGUAGE NoMonomorphismRestriction #-}
import Dependency.Core
import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.List hiding (delete)
import Debug.Trace
import Data.Char


-- dumbly consume everything yielded by a Core, It collect Build and Unlink in different list
consume ::  Core b a -> (([a],[a]), Core b a)
consume t = case step t of
  Left Done -> (([],[]),t)
  Right (Unlink x,t') -> first (second (x:)) $ consume t'
  Right (Build x,t') ->  first (first (x:)) $ consume t'


-- choosen Core instance. The value contains the index (duplicated) and possibly the existential dependency index
type TCore = Core String (String , Maybe String)

------------------------------------------------------------------
-- logic dependencies tests. 
-- -----------------------------------------------------------------

-- logic dependency rule. An item logically depends on another if the second has an index which is strictly a beginning part of the first
logicRule :: String -> String -> Bool
logicRule l = (`isPrefixOf` init l)

testLogic =  
    let 
      
      insertLogic :: String -> TCore -> TCore
      insertLogic l t  = either (error . show) id $ create t l (l,Nothing) (logicRule l) Nothing

      item :: [String] -> Gen String
      item is =  do
          t <- elements ['a' .. 'z']
          elements ([t] : map (++ [t]) is) `suchThat` ( not . flip elem is)

      overlappingStrings :: Gen [String]
      overlappingStrings = do
        n <- elements [0 .. 60::Int]
        foldM (\is _ -> (: is) `fmap` item is) [] [1..n]

      agraph ::  Gen ([String], TCore)
      agraph = (id &&& foldr insertLogic mkCore) `fmap` nub `fmap`  overlappingStrings

      buildsAll = do
        (xs,t) <- agraph
        let ((ys,ds),_) = consume t
        return $ sort xs == sort (map fst ys) && null ds

      buildsRight = do
          (xs,t) <- agraph
          let ((ys',ds),_) = consume t
          let ys = map fst ys'
          let zs = zip ys (tails ys) 
          return $ (not $ any (\(x,ys) -> any (logicRule x) ys) zs) && null ds

      oneTouch =  do
            (xs,t) <-  agraph
            let ((ys',_),t') = consume t
            fmap (all id) . forM xs $ \x -> do 
              let t'' = touch t' x
              let ((ys',_),_) = consume t''
              let ys = map fst ys'
              let zs = x: filter (\y -> logicRule y x) xs
              return $  all (\(x,ys) ->  any (logicRule x) ys) (tail $ zip ys (inits ys)) && sort ys == sort zs
      oneDelete = do
            (xs,t) <-  agraph
            let (_,t') = consume t
            fmap (all id) . forM xs $ \x -> do 
              let t'' = delete t' x
              let ((ys',ds),_) = consume t''
              let ys = map fst ys'
              return $ map fst ds == [x] && all (\(x,ys) ->  any (logicRule x) $ x:ys) (tail $ zip (x:ys) (inits (x:ys)))
    in [buildsAll,buildsRight,oneTouch,oneDelete]

-----------------------------------------------------------------------------------------------------------------------
--- existential links
------------------------------------------

testExistential = let 
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
    return $ (ns,foldr (\(x,y) t -> insertBelongs x y t) mkCore ns)

  ancestors :: [(String,Maybe String)] -> String -> [String]
  ancestors xs x = case lookup x xs of
    Nothing -> error "ancestors:incoherent graph"
    Just Nothing -> [x]
    Just (Just y) -> x:ancestors xs y


  buildsAll = do
    (xs,t) <- agraph
    let ((ys,_),_) = consume t
    return (sort (map fst xs) == sort (map fst ys))			

  oneTouch = do
      (xs,t) <-  agraph
      let (_,t') = consume t
      fmap (all id) . forM xs $ \x -> do 
        let t'' = touch t' (fst x)
        let ((ys,zs),_) = consume t''
        return 
            $ ys == [x] 
            && flip all zs (\y -> fst x `elem` ancestors xs (fst y)) 
            && all (`elem` zs) (filter (\y -> fst x `elem` ancestors xs (fst y)) xs \\ [x]) 
  oneDelete = do
      (xs,t) <-  agraph
      let (_,t') = consume t
      fmap (all id) . forM xs $ \x -> do 
        let t'' = delete t' (fst x)
        let ((ys',zs),_) = consume t''
        return $ null ys' 
            && flip all zs (\y -> fst x `elem` ancestors xs (fst y)) 
            && all (`elem` zs) (filter (\y -> fst x `elem` ancestors xs (fst y)) xs) 

  in [buildsAll,oneTouch,oneDelete]
-----------------------------------------------------------------------------------------------------------------------
--- mixed links
------------------------------------------

testMixed = let 
  insertMixeds ::  String -> Maybe String -> TCore -> TCore
  insertMixeds l s t = either (error . show) id $ create t l (l,s) (logicRule l) s


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
    return $ (ns,foldr (\(x,y) t -> insertMixeds x y t) mkCore ns)

  ancestors :: [(String,Maybe String)] -> String -> [String]
  ancestors xs x = case lookup x xs of
    Nothing -> error "ancestors:incoherent graph"
    Just Nothing -> [x]
    Just (Just y) -> x:ancestors xs y


  buildsAll = do
    (xs,t) <- agraph
    let ((ys,_),_) = consume t
    return (sort (map fst xs) == sort (map fst ys))			


  buildsRight = do
    (xs,t) <- agraph
    let ((ys',ds),_) = consume t
    let ys = map fst ys'
    let zs = zip ys (tails ys) 
    return $ (not $ any (\(x,ys) -> any (logicRule x) ys) zs) && null ds

  oneTouch = do
      (xs,t) <-  agraph
      let (_,t') = consume t
      fmap (all id) . forM xs $ \x -> do 
        let t'' = touch t' (fst x)
        let ((ys,ds),_) = consume t''
        let yys = map fst ys
        let zs = (x: filter (\(y,_) -> logicRule y (fst x)) xs) \\ ds
        return $  all (\(x,ys) ->  any (logicRule x) ys) (tail $ zip yys (inits yys)) && sort yys == sort (map fst zs)
            && flip all ds (\y -> any (\x -> fst x `elem` ancestors xs (fst y)) ys) 
            && all (`elem` ds) (filter (\y -> any (\x -> fst x `elem` ancestors xs (fst y)) ys) xs \\ ys) 


  in [buildsRight,buildsAll,oneTouch]

main = mapM_ quickCheck $  testLogic ++ testExistential ++ testMixed 
