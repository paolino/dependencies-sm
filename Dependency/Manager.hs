{-# LANGUAGE NoMonomorphismRestriction #-}
-- | Dependency manager. A thin layer on Dependency.Core for monadic builders.
module Dependency.Manager (Item (Item), Manager, insertItems, deleteItems, touchItems, update, newManager)  where

import Control.Applicative ((<$>))
import Control.Monad (foldM)
import Dependency.Core

-- | Abstract client core monadic data, must be supplied. Clients can choose monad and index type.
data Ord b => Item m b = Item 
  { index :: b            -- ^ an index for the item
  , build :: m [Item m b] -- ^ building action which return new Items, called when this item is built
  , destroy :: m ()       -- ^ destroying action, called when this item is deleted
  , depmask :: b -> Bool  -- ^ dependencies selector
  }

-- | Manager object. First 3 methods are pure and prepare the monadic update, which is the last method.
data Manager m b = Manager {
  insertItems :: [Item m b] -> Either IndexError (Manager m b),
  deleteItems :: [b] -> Manager m b,
  touchItems :: [b] -> Manager m b,
  update :: m (Either IndexError (Manager m b))
  }
  
-- Compile all (hiding some results)
compile :: (Ord b, Functor m, Monad m) => Core b (Item m b) -> m (Either IndexError (Core b (Item m b)))
compile d = case step d of
  Left Done -> return (Right d)
  Right (Unlink x, d') -> destroy x >> return (Right d')
  Right (Build x,d') -> foldM f d' <$> build x where
    f d (c@(Item k _ _ dm)) = create d k c dm (Just $ index x)

-- | Create a fresh management, with no items controlled
newManager ::  (Ord b , Functor m, Monad m) => Manager m b
newManager = let
  insertItems' x = fmap mkManager . news x
  news d xs = foldM f d xs where   f d (c@(Item k _ _ dm)) = create d k c dm Nothing
  deleteItems' x = mkManager . foldr (flip delete) x
  touchItems' x = mkManager . foldr (flip touch) x
  update' x = fmap mkManager <$> compile x
  mkManager x = Manager (insertItems' x) (deleteItems' x) (touchItems' x) (update' x)
  in mkManager mkCore
