{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances, OverlappingInstances #-}
module Data.Bijection where

import Data.Binary (Binary, encode, decode)
import Data.ByteString.Lazy (ByteString)

class Bijection a b where
  from :: b -> a
  to :: a -> b

instance Bijection a a where
  from = id
  to = id


instance Binary a => Bijection a ByteString where  
  from = decode
  to = encode
