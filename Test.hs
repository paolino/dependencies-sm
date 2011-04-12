
import Dependency.Core
import Test.QuickCheck
import Control.Monad
import Control.Arrow
import Data.List hiding (delete)


-- dumbly consume everything yielded by a Graph, It collect Build and Unlink in different list
consume ::  Graph b a -> (([a],[a]), Graph b a)
consume t = case step t of
  Left Done -> (([],[]),t)
  Right (Unlink x,t') -> first (second (x:)) $ consume t'
  Right (Build x,t') ->  first (first (x:)) $ consume t'

insertNode ::  String -> Maybe String -> (String -> Bool) -> TGraph -> TGraph
insertNode l s f t = either (error . show) id $ create t l (l,s) f s
-- choosen Graph instance. The value contains the index (duplicated) and possibly the existential dependency index
type TGraph = Graph String (String , Maybe String)

-- logic dependency rule. An item logically depends on another if the second has an index which is strictly a beginning part of the first
logicRule :: String -> String -> Bool
logicRule l = (`isPrefixOf` init l)

-- recursively collect existential deps
ancestors :: [(String,Maybe String)] -> String -> [String]
ancestors xs x = case lookup x xs of
  Nothing -> error "ancestors:incoherent graph"
  Just Nothing -> [x]
  Just (Just y) -> x:ancestors xs y

paranoia :: Int
paranoia = 30

-- sum up items
items ::  ([a] -> Gen a) -> Gen [a]
items item = do
        n <- choose (0,paranoia)
        foldM (\is -> const $ (: is) `fmap` item is) [] [1..n]

-- a new item with an existential dependency or Nothing if it's the first
word ::  [String] -> Gen (String, Maybe String)
word is = do 
    t <- choose ('a','z')
    i <- elements ([t] : map (++ [t]) is) `suchThat` ( not . flip elem is)
    k <- case null is of
      False -> Just `fmap` elements is 
      True -> return Nothing
    return (i,k)


--------------------------------
-- logic dependencies tests. 
-- -----------------------------

testLogic =  conjoin [buildsAll,buildsRight,oneTouch,oneDelete]

      where

    agraph ::  Gen ([String], TGraph)
    agraph = (id &&& foldr insert mkGraph) `fmap` items (fmap fst . word)
        where
      insert :: String -> TGraph -> TGraph
      insert l = insertNode l Nothing (logicRule l)

    buildsAll, buildsRight, oneDelete, oneTouch :: Gen Bool

      
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

--------------------------------------
--- existential dependencies tests
--------------------------------------

testExistential = conjoin [buildsAll,oneTouch,oneDelete] 
  
    where
  
  agraph ::  Gen ([(String,Maybe String)], TGraph)
  agraph = (id &&& foldr (\(x,y) t -> insert x y t) mkGraph) `fmap` items (word . map fst)
      where
    insert :: String -> Maybe String -> TGraph -> TGraph
    insert l s = insertNode l s (const False) 

  buildsAll, oneDelete, oneTouch :: Gen Bool
  
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

------------------------------
--- mixed dependencies tests
------------------------------

testMixed = conjoin [buildsRight,buildsAll,oneTouch, oneDelete,catchDuplicate,catchUnbelonging]
  
    where

  agraph ::  Gen ([(String,Maybe String)], TGraph)
  agraph = (id &&& foldr (\(x,y) t -> insert x y t) mkGraph) `fmap` items (word . map fst)
      where
    insert ::  String -> Maybe String -> TGraph -> TGraph
    insert l s = insertNode l s (logicRule l)

  catchUnbelonging, catchDuplicate, buildsAll, buildsRight, oneDelete, oneTouch :: Gen Bool

  catchDuplicate = do
    (xs,t) <- agraph
    if null xs then return True
      else do
        (x,_) <- elements xs 
        return $ case create t x (x,Nothing) (const False) Nothing of
          Left Duplicate -> True
          _ -> False

  catchUnbelonging = do
    (xs,t) <- agraph
    if null xs then return True
      else do
        (y,_) <- word $ map fst xs
        (z,_) <- word $ y:map fst xs
        return $ case create t y (y,Nothing) (const False) (Just z) of
          Left Unbelonging -> True
          _ -> False
        
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

  oneDelete = do
      (xs,t) <-  agraph
      let (_,t') = consume t
      fmap (all id) . forM xs $ \x -> do 
        let t'' = delete t' (fst x)
        let ((ys',zs),_) = consume t''
        return $ null ys' 
            && flip all zs (\y -> fst x `elem` ancestors xs (fst y)) 
            && all (`elem` zs) (filter (\y -> fst x `elem` ancestors xs (fst y)) xs) 

main = quickCheck $  conjoin [testLogic,testExistential,testMixed] 
