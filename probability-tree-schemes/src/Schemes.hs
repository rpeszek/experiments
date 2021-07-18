
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
printLeaves = fold fn
  where
      fn :: ProbTreeF p a String -> String
      fn (LeafF n lb a) = show (n, lb, a)
      fn (BranchesF _ _ xs) = L.intercalate "," xs

-- |
-- >>> printLeaves . computeProb $ exTree
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
computeProb' :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProb' = compWithFloats (unfold fn)
  where
    fn :: ProbTree Float a -> ProbTreeF Float a (ProbTree Float a)
    fn (Branches n l xs) = BranchesF n l (L.map (over probability (* n)) xs)
    fn x = project x

-- | since we use Tree a as target, ana or cata are the same. We can fold as well as unfold
-- >>> printLeaves . computeProb $ exTree
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
computeProb :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProb = compWithFloats (cata fn)
  where
      fn :: ProbTreeF Float a (ProbTree Float a) -> ProbTree Float a
      fn (BranchesF n l xs) = Branches n l (L.map (over probability (* n)) xs)
      fn x = embed x





-- * other examples


tstPrintIO :: IO String
tstPrintIO = printIO exTree

printIO :: forall p a. (Show p, Show a) => ProbTree p a -> IO String
printIO = fold fn
  where
      fn :: ProbTreeF p a (IO String) -> IO String
      fn (LeafF n lb a) = do 
          print (n, lb, a) -- as before
          return $ "done leaf " <> lb
      fn (BranchesF _ lb ixs) = do
           xs <- sequence ixs
           print $ "Processed so far: " ++ show xs
           return $ "done branch " <> lb
                            
printList :: forall a. (Show a) => [a] -> String
printList = fold fn
  where
      fn :: ListF a String -> String
      fn Nil = ""
      fn (Cons a str) = show a <> "," <> str

printList' :: forall a. (Show a) => [a] -> String
printList' = L.foldr (\a str -> show a <> "," <> str) "(START)"       
