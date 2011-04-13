-- | Dependency manager. High level interface to the library.  
module Dependency.Manager (Item (..), Manager, mkManager, insertItems, deleteItems, touchItems, update,  IndexError (..))  where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Dependency.Graph

-- | Item values must be supplied by the clients. They index resources and provide build and destroy actions for them. 
-- Their dependencies are expressed with a matching function on indices. Clients can choose monad and index type.
data Ord b => Item m b = Item 
  { index :: b            -- ^ an index for the item. It must be unique, or 'Duplicate' will be detected
  , build :: m [Item m b] -- ^ building action which return new 'Item''s, called when this item is to be built
  , destroy :: m ()       -- ^ destroying action, called when this item is to be deleted
  , depmask :: b -> Bool  -- ^ all indices matching will be logical dependencies for this item. Inaccurate selectors can lead to 'Cycle' error
  }


type ChangeManager m b =  Either IndexError (Manager m b)
-- | Manager object. A manager is hiding the internal dependency structure. 4 methods can make it evolve.
data Manager m b = Manager 
  { insertItems :: [Item m b] -> ChangeManager m b -- ^ insert new items in the manager
  , deleteItems :: [b] -> ChangeManager m b -- ^ schedule deletion of items by index
  , touchItems :: [b] -> ChangeManager m b -- ^ schedule touch of items of index
  , update :: m (ChangeManager m b) -- ^ execute everything needed to update the manager. This will involve calling 'Item' monadic methods
  }
  
-- Compile all. Shortcuts build and create, making existential dependencies disappear from 'Manager' interface.
compile :: (Ord b, Functor m, Monad m) => Graph (Item m b) b -> m (Either IndexError (Graph (Item m b) b))
compile d = case step d of
  Left Done -> return (Right d)
  Right (_,Left e) -> return $ Left e
  Right (Unlink x, Right d') -> destroy x >> compile d'
  Right (Build x, Right d') -> (foldM f d' <$> build x) >>= either (return . Left) compile where
    f d (c@(Item k _ _ dm)) = create d k c dm (Just $ index x)


-- | Create a fresh manager, with no items controlled
mkManager ::  (Ord b , Functor m, Monad m) => Manager m b
mkManager = let
  insertItems' x = fmap mkManager . news x
  news d xs = foldM f d xs where   f d (c@(Item k _ _ dm)) = create d k c dm Nothing
  deleteItems' x = fmap mkManager . foldM erase x
  touchItems' x = fmap mkManager . foldM touch x
  update' x = fmap mkManager <$> compile x
  mkManager x = Manager (insertItems' x) (deleteItems' x) (touchItems' x) (update' x)
  in mkManager mkGraph
