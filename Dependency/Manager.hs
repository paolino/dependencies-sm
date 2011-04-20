-- | A possible high level interface to the "Dependency.Graph", 
-- freely taken from /Hakyll/ dependency framework (<http://hackage.haskell.org/package/hakyll>).
-- 
-- Here we force the client to a model: the 'Item'. This model has everything is necessary to use the "Dependency.Graph" framework. 
--
-- An 'Item' is a resource for the graph and it contains also its 'index', that must be unique. 
--
-- It contains also the reactions to 'Build' and 'Unlink', with a peculiarity: the reaction to 'Build' can produce more items. 
-- Incidentally this items are existentially depending on the item that created them by that reaction. 
-- And more, there is no other way to produce this type of dependency link.
--
-- This peculiarity is known in *Hakyll* as meta compiling. A compiling action produce new compilers. A runtime feature. 
--
-- Last field of an 'Item' is a dependency mask ('depmask') which is exactly the way to express locic dependencies in a 'Graph' (see 'create')
--


module Dependency.Manager 
  ( 
  -- * Interfacing to the module
    Item (..)
  -- * Operational elements
  , Manager 
  , update
  , mkManager
  -- * From "Dependency.Graph"
  , IndexError (..)
  -- * From "Data.List.Difference"
  , Difference (..)
  )  where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Monad.Writer (tell, WriterT, runWriterT, listen, lift, foldM)
import Data.List.Difference (Difference (..))
import qualified Dependency.Graph as DG (Graph)
import Dependency.Graph  hiding (Graph)


import Control.Monad.Cont

-- | Item values must be supplied by the clients. They provide build and destroy actions for them. 
-- Their dependencies are expressed with a matching function on indices. Clients can choose monad and index type.
data Ord b => Item m b = Item 
  { build :: m [(b,Item m b)] -- ^ building action which return new 'Item''s, evaluated when this item is to be built
  , unlink :: m ()        -- ^ destroying action, evaluated when this item is to be deleted
  , depmask :: b -> Bool  -- ^ all indices matching will be logical dependencies for this item. Inaccurate selectors can lead to 'Cycle' error
  }

-- Each manager action can lead to a problem with indices signalled in IndexError 
type Result m b = m (Either IndexError (Core m b))

-- A manager is what's to be held to track dependencies
data Core m b = Core 
  { insertItem :: b -> Item m b -> Result m b --  insert new items in the manager
  , deleteItem :: b -> Result m b --  schedule deletion of items by index
  , touchItem :: b -> Result m b --  schedule touch of items of index
  }

-- the real graph where values are items
type Graph m b = DG.Graph (Item m b) b

-- helped by Writer monad to collect new items from build actions
type Collecting m b = WriterT [(b,(b,Item m b))] m 

-- compile until graph goes in accept mode. New items with their keys are collected by telling
compile :: (Ord b, Functor m, Monad m) => Graph m b -> Collecting m b (Graph m b)
compile (Present (y,ng)) = case y of
  Unlink (_,x) -> lift (unlink x) >> compile ng
  Build (i,x) -> do
    is <- lift $ build x
    tell $ zip (repeat i) is
    compile ng
compile g = return g

-- errors from graph operation are reported
type ErrorGraph m b = Either IndexError (Graph m b)

-- core of the folding of recursive compiling the new itemes collected on builds. 
-- It crashes on Present state of the graph, which must be handled by compile
--
amend :: Ord b => Graph m b -> (b, (b,Item m b)) -> ErrorGraph m b
amend (Accept g) (j,(y,i)) = create g y i (depmask i) (Just j) 
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
                          

--  Create a fresh manager, with no items controlled.
mkCore ::  (Ord b , Functor m, Monad m) => Core m b
mkCore = let
  execute = fmap (fmap mkCore) . runCompilation
  insertItem' (Accept g) y x = eitherExecute $ create g y x (depmask x) Nothing
  deleteItem' (Accept g) x = eitherExecute $ erase g x
  touchItem' (Accept g) x = eitherExecute $ touch g x
  mkCore g = Core (insertItem' g) (deleteItem' g) (touchItem' g) 
  eitherExecute = either (return . Left) execute
  in mkCore mkGraph


-- | A stepping manager for 'Item's.
newtype Manager m b =  Manager {
  update ::  Difference b -> m (Either IndexError (Manager m b)) -- ^ updates the manager from a 'Difference' on indices
  }

-- | Build a 'Manager' given an 'Item' creator.
mkManager ::  (Ord b, Functor m, Monad m) => (b -> Item m b) -> Manager m b
mkManager mkItem = let  
        update t (Difference ns ds ts) = flip runContT return  $ callCC $ \k -> Right <$> do
              let folder f t n = lift (f t n) >>= either (k . Left) return
              t' <- foldM (folder $ uncurry . insertItem) t $ map (id &&& mkItem) ns
              t'' <- foldM (folder deleteItem) t' ds
              Manager . update <$> foldM (folder touchItem) t'' ts
        in Manager (update mkCore)


