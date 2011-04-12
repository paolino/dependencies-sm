{-# LANGUAGE ViewPatterns #-}


import Data.Map (Map, (!), adjust)


data Node a b = Node 
  { value :: a
  , logic :: [b]
  , existence :: [b]
  }

data Status a 
  = Uptodate {status :: a}
  | Build {status :: a}
  | Unlink {status :: a}


type Graph a b = Map b (Status (Node a b))

data Modi = MBuild | MUnlink

flood :: Ord b => Status (Node a b) -> Graph a b -> Graph a b
flood z = ff (modi MUnlink) (existence . status $ z) . ff (modi MBuild) (logic . status $ z)
  where ff = flip . foldr

modi :: Ord b => Modi -> b -> Graph a b -> Graph a b 
modi MUnlink x g = case z of
  Unlink _ -> g
  z -> flood z $ adjust (Unlink . status) x g
  where z = g ! x

modi MBuild x g = case z of
  Unlink _ -> g
  Build _ -> g
  z -> flood z $ adjust (Build . status) x g
  where z = g ! x
