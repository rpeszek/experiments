
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications, Arrows #-}
module SemArr where

import qualified Control.Arrow as Arr
import Polysemy
import Polysemy.Input
import Polysemy.Output    

import Teletype

-- | Do not get confused by name Kleisli does not imply monad
-- in particular:
--
-- @
-- Functor m => Functor (Kleisli m a)
-- Applicative m => Applicative (Kleisli m a)
-- @
--
-- We encode polysemy @Sem@ as @Arr.Kleisli (Sem r) a b@ to make it an arrow
type SemArr r a b = Arr.Kleisli (Sem r) a b

-- | convert between Sem and SemArr
constSemArr :: (Sem r) b -> SemArr r () b
constSemArr c = Arr.Kleisli (const c)

semArr :: (a -> (Sem r) b )-> SemArr r a b
semArr = Arr.Kleisli

-- |  transform @Sem@ compilation stacks to @SemArr@ compilation stacks.
-- semArrCompl'' :: (forall x . Sem (e ': r) x -> Sem r x) -> SemArr (e ': r) a b -> SemArr r a b 
-- semArrCompl'' comp (Arr.Kleisli fn) = Arr.Kleisli (comp . fn)
--
-- semArrCompl' :: (forall x . Sem r1 x -> Sem r2 x) -> SemArr r1 a b -> SemArr r2 a b 
-- semArrCompl' comp (Arr.Kleisli fn) = Arr.Kleisli (comp . fn)

semArrCompl :: (Sem r1 b1 -> Sem r2 b2) -> SemArr r1 a b1 -> SemArr r2 a b2 
semArrCompl comp (Arr.Kleisli fn) = Arr.Kleisli (comp . fn)
