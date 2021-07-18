
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes  #-}
-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# LANGUAGE TypeApplications #-}

module Types where
import           Data.Functor.Foldable.TH
-- import           Data.Functor.Foldable
import           Control.Lens 
import qualified Data.List as L





-- probability tree diagram
data ProbTree p a =
    Leaf p String a
    | Branches p String [ProbTree p a]
    deriving (Eq, Show, Functor) --, Foldable, Traversable)


makeBaseFunctor ''ProbTree

deriving instance (Show a, Show p, Show b) => Show (ProbTreeF p a b)

-- | needs to be non-polymorphic because of nesting
probability :: Lens' (ProbTree p a) p
probability = lens get set 
  where
    get (Leaf p _ _) = p
    get (Branches p _ _) = p
    set (Leaf _ l x) r = Leaf r l x
    set (Branches _ l x) r = Branches r l x

-- | equivalent to:
-- probabilityT :: Traversal (ProbTree p a) (ProbTree r a) p r
probabilityT :: forall p r a f . Applicative f => (p -> f r) -> ProbTree p a -> f (ProbTree r a)
probabilityT fn (Leaf p l a) = Leaf <$> fn p <*> pure l <*> pure a
probabilityT fn (Branches p l xs) = Branches <$> fn p <*> pure l <*> fx xs
  where fx :: [ProbTree p a] -> f [ProbTree r a]
        fx xs = traverse (probabilityT fn) xs


-- | Create some illusion of type safety by separating Node Proabilities and Cumulative Probalities types
newtype NodeProb = NodeProb {unNodeProb :: Float} deriving Eq
newtype CumulativeProb = CumulativeProb {unCumulativeProb :: Float} deriving Eq

instance Show NodeProb where
    show = show . unNodeProb
instance Show CumulativeProb where
    show = show . unCumulativeProb

-- | Create some illusion of type safety by keeping 
compWithFloats :: (ProbTree Float a -> ProbTree Float a) -> ProbTree NodeProb a -> ProbTree CumulativeProb a
compWithFloats fn tree = over probabilityT CumulativeProb $ fn $ over probabilityT unNodeProb tree 



exTree :: ProbTree NodeProb ()
exTree = over probabilityT NodeProb $ Branches 1 "R" [
   Branches 0.5 "1" [
      Leaf 0.5 "11" ()
      , Leaf 0.5 "12" ()
   ]
   , Branches 0.5 "2" [
      Leaf 0.3 "21" ()
      , Leaf 0.3 "22" ()
      , Leaf 0.4 "23" ()
   ]
 ]