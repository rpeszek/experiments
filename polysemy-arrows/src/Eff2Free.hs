
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications, Arrows #-}
module Eff2Free where

import qualified Control.Arrow as Arr

import Control.Category
import qualified Control.Category as Cat

-- |
-- Free Category that can express arbitrary function
data Eff2Free eff a b where
  Pure :: (a -> b) -> Eff2Free eff a b
  Effect :: eff a b -> Eff2Free eff a b
  Seq :: Eff2Free eff a b -> Eff2Free eff b c -> Eff2Free eff a c

instance Category (Eff2Free eff) where
  id = Pure Cat.id
  (.) = flip Seq

liftCompKl :: Monad m => (forall x y . eff x y ->  Arr.Kleisli m x y) -> Eff2Free eff a b -> Arr.Kleisli m a b
liftCompKl _ (Pure f) =  Arr.Kleisli (pure Cat.. f)
liftCompKl fn (Effect eff) = fn eff
liftCompKl fn (Seq a1 a2) = liftCompKl fn a1 >>> liftCompKl fn a2





