-- | A typeclass and instances to project a type back and forth to another one

{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Bijection (Bijection (..)) where

-- | The Bijection class
class Bijection a b where
  from :: b -> a  -- ^ back from b to a
  to :: a -> b  -- ^ forth from a to b

instance Bijection a a where
  from = id
  to = id
