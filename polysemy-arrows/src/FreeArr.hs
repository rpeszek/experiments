
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications, Arrows #-}
module FreeArr where

import qualified Control.Arrow as Arr

import Control.Category
import qualified Control.Category as Cat
import Control.Arrow


data FreeArr eff a b where
  Pure :: (a -> b) -> FreeArr eff a b
  Effect :: eff a b -> FreeArr eff a b
  Seq :: FreeArr eff a b -> FreeArr eff b c -> FreeArr eff a c
  Par :: FreeArr eff a1 b1 -> FreeArr eff a2 b2 -> FreeArr eff (a1, a2) (b1, b2)

  -- Plus :: FreeArr eff b c ->  FreeArr eff b' c' ->  FreeArr eff (Either b b') (Either c c') 

instance Category (FreeArr eff) where
  id = Pure Cat.id
  (.) = flip Seq

instance Arrow (FreeArr eff) where
  arr = Pure
  (***) = Par

-- instance ArrowChoice (FreeArr eff) where
--     (+++) = Plus

-- instance ArrowApply (FreeArr eff) where
--     app = undefined

arrEff :: eff a b -> FreeArr eff a b
arrEff = Effect


liftCompKl :: Monad m => (forall x y . eff x y ->  Arr.Kleisli m x y) -> FreeArr eff a b -> Arr.Kleisli m a b
liftCompKl _ (Pure f) =  Arr.Kleisli (pure Cat.. f)
liftCompKl fn (Effect eff) = fn eff
liftCompKl fn (Seq a1 a2) = liftCompKl fn a1 >>> liftCompKl fn a2
liftCompKl fn (Par a1 a2) = liftCompKl fn a1 *** liftCompKl fn a2

-- liftCompKl fn (Plus a1 a2) = liftCompKl fn a1 +++ liftCompKl fn a2




