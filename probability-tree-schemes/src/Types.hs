
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
import           Data.Functor.Foldable
import           Control.Lens 
import qualified Data.List as L



type NodeProb = Float
type CumProb = Float

-- probability tree diagram
data ProbTree p a =
    Leaf p String a
    | Branches p String [ProbTree p a]
    deriving (Eq, Show, Functor, Foldable, Traversable)

makeBaseFunctor ''ProbTree

-- | needs to be non-polymorphic because of nesting
probability :: forall p r a . Lens' (ProbTree p a) p
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

--     lens get set 
--  where
--   get (Leaf p _ _) = p
--   get (Branches p _ _) = p
--   set :: ProbTree p a -> r -> ProbTree r a
--   set (Leaf _ l x) r = Leaf r l x
--   set (Branches _ l x) r = Branches r l (L.map (over (probabilityT tranf) tranf) x)


deriving instance (Show a, Show p, Show b) => Show (ProbTreeF p a b)

type Tree a = ProbTree NodeProb a
type TreeF a r = ProbTreeF NodeProb a r

exTree :: ProbTree NodeProb ()
exTree = Branches 1 "R" [
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