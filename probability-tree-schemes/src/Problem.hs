{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE TypeApplications #-}

module Problem where
-- import           Types
import qualified Data.List as L
import           Data.Tree
import           Data.Functor.Foldable
import           Data.Functor.Base
import           Control.Lens
import           Data.Tree.Lens
-- import           Control.Monad.Reader


-- Note these work, these are equivalent to computeProbBad
factorials :: Int -> [Int]
factorials size = foldr (\n l -> n : fmap (*n) l ) [] [1..size] 

-- >>> factorials' 6
-- [1,2,6,24,120,720]
factorials' :: Int -> [Int]
factorials' size = cata fn [1..size] 
  where
    fn Nil = []
    fn (Cons n l) = n : fmap (*n) l

-- >>> oneBranch [1..5]
-- Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 3, subForest = [Node {rootLabel = 4, subForest = [Node {rootLabel = 5, subForest = []}]}]}]}]}
oneBranch :: [a] -> Tree a
oneBranch [] = error "Invalid empty tree"
oneBranch [x] = Node x []
oneBranch (x:xs) = Node x [oneBranch xs]

-- >>> factorialsRose 5
-- Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 6, subForest = [Node {rootLabel = 24, subForest = [Node {rootLabel = 120, subForest = []}]}]}]}]}
factorialsRose :: Int -> Tree Int
factorialsRose size = cata fn $ oneBranch [1..size]
 where
   fn :: TreeF Int (Tree Int) -> Tree Int
   fn (NodeF n l) = Node n (L.map (fmap (*n)) l) 
   -- fn (NodeF n l) = Node n (L.map (over payloads (*n)) l) 

-- >>> factorialsRose' 5
-- Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = [Node {rootLabel = 6, subForest = [Node {rootLabel = 12, subForest = [Node {rootLabel = 20, subForest = []}]}]}]}]}
factorialsRose' :: Int -> Tree Int
factorialsRose' size = cata fn $ oneBranch [1..size]
 where
   fn :: TreeF Int (Tree Int) -> Tree Int
   -- fn (NodeF n l) = Node n (L.map (fmap (*n)) l) 
   -- fn (NodeF n l) = Node n (L.map (over payloads (*n)) l) 
   fn (NodeF n l) = Node n (L.map (over root (*n)) l) 

payloads :: Lens' (Tree a) a
payloads = lens rootLabel (\(Node _ xs) a -> Node a xs )
