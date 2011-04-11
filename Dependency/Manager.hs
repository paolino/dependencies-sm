-- | Dependency manager. High level interface to the library.  
module Dependency.Manager (Item (..), Manager, newManager, insertItems, deleteItems, touchItems, update,  IndexError (..))  where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Dependency.Core

-- | Abstract client core monadic data. Values must be supplied by the clients. Clients can choose monad and index type.
data Ord b => Item m b = Item 
  { index :: b            -- ^ an index for the item. It must be unique, or 'Duplicate' will be detected
  , build :: m [Item m b] -- ^ building action which return new 'Item''s, called when this item is to be built
  , destroy :: m ()       -- ^ destroying action, called when this item is to be deleted
  , depmask :: b -> Bool  -- ^ dependencies selector. All indices matching will be logical dependencies for this item
  }

-- | Manager object. 
data Manager m b = Manager 
  { insertItems :: [Item m b] -> Either IndexError (Manager m b) -- ^ insert new items in the manager
  , deleteItems :: [b] -> Manager m b -- ^ schedule deletion of indices
  , touchItems :: [b] -> Manager m b -- ^ schedule touch of indices
  , update :: m (Either IndexError (Manager m b)) -- ^ execute everything needed to update the manager
  }
  
-- Compile all (hiding some results)
compile :: (Ord b, Functor m, Monad m) => Core b (Item m b) -> m (Either IndexError (Core b (Item m b)))
compile d = case step d of
  Left Done -> return (Right d)
  Right (Unlink x, d') -> destroy x >> return (Right d')
  Right (Build x,d') -> foldM f d' <$> build x where
    f d (c@(Item k _ _ dm)) = create d k c dm (Just $ index x)

-- | Create a fresh manager, with no items controlled
newManager ::  (Ord b , Functor m, Monad m) => Manager m b
newManager = let
  insertItems' x = fmap mkManager . news x
  news d xs = foldM f d xs where   f d (c@(Item k _ _ dm)) = create d k c dm Nothing
  deleteItems' x = mkManager . foldr (flip delete) x
  touchItems' x = mkManager . foldr (flip touch) x
  update' x = fmap mkManager <$> compile x
  mkManager x = Manager (insertItems' x) (deleteItems' x) (touchItems' x) (update' x)
  in mkManager mkCore
