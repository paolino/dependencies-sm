-- | High level interface to the 'Dependency.Graph'. Clients should be able to rewrite each possible compiling action in an 'Item'.
module Dependency.Manager 
  ( Item (..)
  , IndexError (..)
  , Result
  , Manager
  , insertItem
  , deleteItem
  , touchItem
  , mkManager
  )  where
import Control.Applicative ((<$>))
import Control.Monad.Writer (tell, WriterT, runWriterT, listen, lift, foldM)
import qualified Dependency.Graph as DG (Graph)
import Dependency.Graph  hiding (Graph)

import Control.Monad.Cont

-- | Item values must be supplied by the clients. They index resources and provide build and destroy actions for them. 
-- Their dependencies are expressed with a matching function on indices. Clients can choose monad and index type.
data Ord b => Item m b = Item 
  { index :: b            -- ^ an index for the item. It must be unique, or 'Duplicate' will be detected
  , build :: m [Item m b] -- ^ building action which return new 'Item''s, evaluated when this item is to be built
  , destroy :: m ()       -- ^ destroying action, evaluated when this item is to be deleted
  , depmask :: b -> Bool  -- ^ all indices matching will be logical dependencies for this item. Inaccurate selectors can lead to 'Cycle' error
  }

-- | Each manager action can lead to a problem with indices found in IndexError 
type Result m b = m (Either IndexError (Manager m b))

-- | Manager object. A manager is hiding the internal dependency structure. 4 methods can make it evolve.
data Manager m b = Manager 
  { insertItem :: Item m b -> Result m b -- ^ insert new items in the manager
  , deleteItem :: b -> Result m b -- ^ schedule deletion of items by index
  , touchItem :: b -> Result m b -- ^ schedule touch of items of index
  }

-- the real graph where values are items
type Graph m b = DG.Graph (Item m b) b

-- helped by Writer monad to collect new items from build actions
type Collecting m b = WriterT [(b,Item m b)] m 

-- compile until graph goes in accept mode. New items with their keys are collected by telling
compile :: (Ord b, Functor m, Monad m) => Graph m b -> Collecting m b (Graph m b)
compile (Run (y,ng)) = case y of
  Unlink x -> lift (destroy x) >> compile ng
  Build x -> do
    is <- lift $ build x
    tell $ zip (repeat $ index x) is
    compile ng
compile g = return g

-- errors from graph operation are reported
type ErrorGraph m b = Either IndexError (Graph m b)

-- core of the folding of recursive compiling the new itemes collected on builds. 
-- It crashes on Run state of the graph, which must be handled by compile
--
amend :: Ord b => Graph m b -> (b, Item m b) -> ErrorGraph m b
amend (Accept g) (j,i) = create g (index i) i (depmask i) (Just j) 
amend _ _ = error "Dependency.Manager: compiling was not completed"

-- add a ContT monad layer to come out straight of recursion. The result of the computation is already forced to ErrorGraph. 
-- There is nothing we can do on what has happened in the underlaying monad. The client should care about this.
type Blowup m b = ContT (ErrorGraph m b) (Collecting m b)


-- recursively create and build everything produced by builds. It finishes promptly when an error comes from a 'DG.create'
compileRecursive :: (Functor m, Monad m, Ord b)
      => (IndexError -> Blowup m b (Graph m b))
      -> Graph m b
      -> Blowup m b (Graph m b)
compileRecursive k g = lift (listen (compile g)) >>= uncurry (foldM (\g -> either k (compileRecursive k) . amend g))

-- resolve all pending builds and erases and recursively all new requests from builds
runCompilation :: (Ord b, Functor m, Monad m) => Graph m b -> m (ErrorGraph m b)
runCompilation g = fmap fst . runWriterT . flip runContT return $ callCC $ \k -> Right <$> compileRecursive (k . Left) g
                          

-- | Create a fresh manager, with no items controlled
mkManager ::  (Ord b , Functor m, Monad m) => Manager m b
mkManager = let
  execute = fmap (fmap mkManager) . runCompilation
  insertItem' (Accept g) x = either (return . Left) execute (create g (index x) x (depmask x) Nothing)
  deleteItem' (Accept g) x = either (return . Left) execute $ erase g x
  touchItem' (Accept g) x = either (return . Left) execute $ touch g x
  mkManager g = Manager (insertItem' g) (deleteItem' g) (touchItem' g) 
  in mkManager mkGraph

