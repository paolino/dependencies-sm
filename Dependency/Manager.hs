-- | Dependency manager. High level interface to the library.  
module Dependency.Manager -- (Item (..), Manager, mkManager, insertItems, deleteItems, touchItems, update,  IndexError (..))  where
  where
import Control.Applicative ((<$>))
import Control.Monad.Writer (tell, WriterT, runWriterT, listen, lift, foldM)
import Dependency.Graph
import Control.Monad.Cont

-- | Item values must be supplied by the clients. They iindex resources and provide build and destroy actions for them. 
-- Their dependencies are expressed with a matching function on indices. Clients can choose monad and iindex type.
data Ord b => Item m b = Item 
  { iindex :: b            -- ^ an iindex for the item. It must be unique, or 'Duplicate' will be detected
  , build :: m [Item m b] -- ^ building action which return new 'Item''s, called when this item is to be built
  , destroy :: m ()       -- ^ destroying action, called when this item is to be deleted
  , depmask :: b -> Bool  -- ^ all indices matching will be logical dependencies for this item. Inaccurate selectors can lead to 'Cycle' error
  }


type Result m b = m (Either IndexError (Manager m b))
-- | Manager object. A manager is hiding the internal dependency structure. 4 methods can make it evolve.
data Manager m b = Manager 
  { insertItem :: Item m b -> Result m b -- ^ insert new items in the manager
  , deleteItem :: b -> Result m b -- ^ schedule deletion of items by iindex
  , touchItem :: b -> Result m b -- ^ schedule touch of items of iindex
  }

  
-- Compile all. Shortcuts build and create, making existential dependencies disappear from 'Manager' interface.
compile :: (Ord b, Functor m, Monad m) => Graph (Item m b) b -> WriterT [(b,Item m b)] m (Graph (Item m b) b)
compile (Run (y,ng)) = case y of
  Unlink x -> lift (destroy x) >> compile ng
  Build x -> do
    is <- lift $ build x
    tell $ zip (repeat $ iindex x) is
    compile ng
compile g = return g

type EGraph m b = Either IndexError  (Graph (Item m b) b)

compileRecursive :: (Ord b, Functor m, Monad m) 
    => (EGraph m b -> ContT (EGraph m b) (WriterT [(b, Item m b)] m) (EGraph m b))
    -> Graph (Item m b) b 
    -> ContT (EGraph m b) (WriterT [(b, Item m b)] m) (EGraph m b)
compileRecursive k g = do 
  (g,is) <- lift (listen (compile g))
  foldM (\(Right (Accept g)) (j,i) -> case create g (iindex i) i (depmask i) (Just j) of 
                          Left e -> k $ Left e
                          Right g' -> compileRecursive k g'
                          ) (Right g) is

runCompilation :: (Ord b, Functor m, Monad m) => Graph (Item m b) b -> m (EGraph m b)
runCompilation g = fmap fst . runWriterT . flip runContT return $ callCC $ \k -> compileRecursive k g
                          
-- | Create a fresh manager, with no items controlled
mkManager ::  (Ord b , Functor m, Monad m) => Manager m b
mkManager = let
  execute = fmap (fmap mkManager) . runCompilation
  insertItem' (Accept g) x = either (return . Left) execute (create g (iindex x) x (depmask x) Nothing)
  deleteItem' (Accept g) x = execute $ erase g x
  touchItem' (Accept g) x = execute $ touch g x
  mkManager g = Manager (insertItem' g) (deleteItem' g) (touchItem' g) 
  in mkManager mkGraph

