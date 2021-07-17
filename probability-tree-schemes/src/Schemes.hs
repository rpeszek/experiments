
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE TypeApplications #-}

module Schemes where
import           Types  
import qualified Data.List as L
import           Data.Functor.Foldable
import           Control.Lens



printLeaves :: forall p a. (Show p, Show a) => ProbTree p a -> String
printLeaves = cata fn
  where
      fn :: ProbTreeF p a String -> String
      fn (LeafF n lb a) = show (n, lb, a)
      fn (BranchesF _ _ xs) = L.intercalate "," xs

computeProb :: ProbTree NodeProb a -> ProbTree CumProb a
computeProb = ana fn
  where
    fn :: Tree a -> TreeF a (Tree a)
    fn (Branches n a xs) = BranchesF n a (fmap (over probability (* n)) xs)
    fn x = project x

-- >>> tst
-- "(0.25,\"11\",\"A\"),(0.25,\"12\",\"B\"),(0.15,\"21\",\"C\"),(0.15,\"22\",\"D\"),(0.2,\"23\",\"E\")"
tst :: String
tst = printLeaves . computeProb $ exTree

-- since we use Tree a as target, ana or cata are the same. We can fold as well as unfold
computeProb' :: ProbTree NodeProb a -> ProbTree CumProb a
computeProb' = cata fn
  where
      fn :: TreeF a (Tree a) -> Tree a
      fn (BranchesF n l xs) = Branches n l (fmap (over probability (* n)) xs)
      fn x = embed x

tst' :: String
tst' = printLeaves . computeProb' $ exTree

-- x :: Tree String
-- x = unfold  @ (Tree String) (project @ (Tree String)) exTree
