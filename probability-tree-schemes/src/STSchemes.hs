
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

-- | Example of traditional recursion in Haskell
module STSchemes where

import           Types
import           Control.Monad.ST
import           Data.STRef
import           Control.Lens
import           Data.Functor.Foldable
import           Data.Traversable
import           Schemes
import           ST hiding (computeProbMutable, tstMutable)
import qualified Data.List as L


computeProbMutableBad :: forall a . ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProbMutableBad = compWithFloats (
    \t -> runST $ do
        mutable <- makeMutable t
        mutable' <- traverseOf probabilityT (pure . pure) mutable
        computeSTBad mutable'
        makeNonMutable mutable
    )


computeSTBad :: forall s a . ProbTree (ST s (MutableFloat s)) a -> ST s (MutableFloat s)
computeSTBad = fold fn
  where
      fn :: ProbTreeF (ST s (MutableFloat s)) a (ST s (MutableFloat s)) -> ST s (MutableFloat s)
      fn (LeafF n _ _) = n

      fn (BranchesF stmn lb ixs) = do
           mn :: MutableFloat s <- stmn
           n :: Float <- readSTRef mn -- read branch proability
           mxs :: [MutableFloat s] <- sequence ixs -- get access to mutable probablities for children 
           mapM_ (`modifySTRef` (n *)) mxs -- mutate the children's probabilities (This even sounds terrible!)
           stmn -- return back the untouched branch probability

-- |
-- >>> tstMutableBad
-- "(0.25,\"111\",()),(0.25,\"112\",()),(0.5,\"121\",()),(0.1,\"21\",()),(0.2,\"22\",()),(0.2,\"231\",()),(0.2,\"232\",())"
tstMutableBad :: String
tstMutableBad = printLeaves $ computeProbMutableBad exTree

printIO' :: forall p a. (Show p, Show a) => ProbTree p a -> IO String
printIO' = fold fn
  where
      fn :: ProbTreeF p a (IO String) -> IO String
      fn (LeafF n lb a) = do
          print (n, lb, a) -- as before
          return $ "done leaf " <> lb
      fn (BranchesF _ lb ixs) = do
           xs <- sequence ixs
           print $ "Processed so far: " ++ show xs
           return $ "done branch " <> lb


computeST :: forall s a . ProbTree (ST s (MutableFloat s)) a -> ST s [MutableFloat s]
computeST = fold fn
  where
      fn :: ProbTreeF (ST s (MutableFloat s)) a (ST s [MutableFloat s]) -> ST s [MutableFloat s]
      fn (LeafF n _ _) =  (:[]) <$> n
      
      fn (BranchesF stmn lb ixs) = do
        mn :: MutableFloat s <- stmn
        n :: Float <- readSTRef mn -- read branch proability
        mxs :: [MutableFloat s] <- concat <$> sequence ixs -- get access to mutable probablities for children 
        mapM_ (`modifySTRef` (n *)) mxs -- mutate the subtree's probabilities 
        pure $ mn : mxs  --return all mutable floats, including the current node


computeProbMutable :: forall a . ProbTree NodeProb a -> ProbTree CumulativeProb a
computeProbMutable = compWithFloats (
    \t -> runST $ do
        mutable <- makeMutable t
        mutable' <- traverseOf probabilityT (pure . pure) mutable
        computeST mutable'
        makeNonMutable mutable
    )

-- |
-- >>> tstMutable
-- "(0.125,\"111\",()),(0.125,\"112\",()),(0.25,\"121\",()),(0.1,\"21\",()),(0.2,\"22\",()),(0.1,\"231\",()),(0.1,\"232\",())"
tstMutable :: String
tstMutable = printLeaves $ computeProbMutable exTree
