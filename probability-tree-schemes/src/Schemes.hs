
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
import           Control.Monad.Reader



printLeaves :: forall p a. (Show p, Show a) => ProbTree p a -> String
printLeaves = fold fn
  where
      fn :: ProbTreeF p a String -> String
      fn (LeafF n lb a) = show (n, lb, a)
      fn (BranchesF _ _ xs) = L.intercalate "," xs

-- |
-- >>> printLeaves . computeProb $ exTree
-- "(0.125,\"111\",()),(0.125,\"112\",()),(0.25,\"121\",()),(0.1,\"21\",()),(0.2,\"22\",()),(0.1,\"231\",()),(0.1,\"232\",())"
computeProb :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProb = compWithFloats (unfold fn)
  where
    fn :: ProbTree Float a -> ProbTreeF Float a (ProbTree Float a)
    fn (Branches n l xs) = BranchesF n l (L.map (over probability (* n)) xs)
    fn x = project x

-- | since we use Tree a as target, ana or cata are the same. We can fold as well as unfold
-- but the recursion stops too early
-- >>> printLeaves . computeProbBad $ exTree
-- "(0.25,\"111\",()),(0.25,\"112\",()),(0.5,\"121\",()),(0.1,\"21\",()),(0.2,\"22\",()),(0.2,\"231\",()),(0.2,\"232\",())"
computeProbBad :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProbBad = compWithFloats (cata fn)
  where
      fn :: ProbTreeF Float a (ProbTree Float a) -> ProbTree Float a
      fn (BranchesF n l xs) = Branches n l (L.map (over probability (* n)) xs)
      fn x = embed x


-- >>> printLeaves . computeProb' $ exTree
-- "(0.125,\"111\",()),(0.125,\"112\",()),(0.25,\"121\",()),(0.1,\"21\",()),(0.2,\"22\",()),(0.1,\"231\",()),(0.1,\"232\",())"
computeProb' :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProb' = compWithFloats (`computeProbFn` 1)
  -- where 
  --   fn t = computeProbFloat t 1

computeProbFn :: ProbTree Float a -> Float -> ProbTree Float a
computeProbFn = cata fn
  where
    fn :: ProbTreeF Float a (Float -> ProbTree Float a) -> (Float -> ProbTree Float a)
    fn (BranchesF n l xs) p = Branches (p * n) l $ L.map (\fn -> fn . ( *n ) $ p ) xs
    fn (LeafF n l a) p = Leaf (p * n) l a


-- >>> printLeaves . computeProbRdr $ exTree
-- "(0.125,\"111\",()),(0.125,\"112\",()),(0.25,\"121\",()),(0.1,\"21\",()),(0.2,\"22\",()),(0.1,\"231\",()),(0.1,\"232\",())"
computeProbRdr :: ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProbRdr = compWithFloats (\t -> runReader (computePropRdr t) 1)

computePropRdr :: ProbTree Float a -> Reader Float (ProbTree Float a)
computePropRdr = cata fn
  where
    fn :: ProbTreeF Float a (Reader Float (ProbTree Float a)) -> Reader Float (ProbTree Float a)
    fn (BranchesF n l xs) = do
      p <- ask
      xss <- mapM (local (*n)) xs
      pure $ Branches (p * n) l xss
    fn (LeafF n l a) = do
      p <- ask
      pure $ Leaf (p * n) l a  
      
      --embed x
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
