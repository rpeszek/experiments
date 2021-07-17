
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

-- | Example of traditional recursion in Haskell
module ST where

import           Types
import           Control.Monad.ST
import           Data.STRef
import           Control.Lens
import           Schemes

type MutableFloat s = STRef s Float

computeProbMutable :: forall a . ProbTree Float a -> ProbTree Float a 
computeProbMutable tree = runST $ do
    mutable <- makeMutable tree
    mutableres <- recursion 1 mutable
    makeNonMutable mutableres
 where 
   recursion :: Float -> ProbTree (MutableFloat s) a -> ST s (ProbTree (MutableFloat s) a)   
   recursion n (Branches mutp l xs)  = do 
     modifySTRef mutp (n *) -- modify probability in place
     n1 <- readSTRef mutp   -- read modified probablity
     xs1 <- mapM (recursion n1) xs -- recursively apply modified value to children
     return $ Branches mutp l xs1  

   recursion n x@(Leaf mutp _ _) = do 
     modifySTRef mutp (n *)  -- on Leaf there is not much to do, only modify the propability
     return x

makeMutable :: ProbTree Float a ->  ST s (ProbTree (MutableFloat s) a)
makeMutable =  
   traverseOf probabilityT (newSTRef @Float)

makeNonMutable ::  ProbTree (MutableFloat s) a -> ST s (ProbTree Float a) 
makeNonMutable = traverseOf probabilityT readSTRef

-- >>> tstMutable
-- "(0.25,\"11\",()),(0.25,\"12\",()),(0.15,\"21\",()),(0.15,\"22\",()),(0.2,\"23\",())"
tstMutable :: String
tstMutable = printLeaves . computeProbMutable $ exTree     
