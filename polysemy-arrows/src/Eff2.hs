
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds
          --  , PolyKinds
           , TypeApplications, Arrows #-}
module Eff2 where

import qualified Control.Arrow as Arr
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.CustomErrors
import Polysemy.Input
import Polysemy.Output    

import Teletype
import SemArr
import Data.Kind
import Control.Category
import qualified Control.Category as Cat
import Control.Arrow

import FreeArr


-- h (ArrowMonad h) = (h, ()) >>> Arr.app

-- eval :: forall arr1 arr2 m a b . (Arrow arr1, Arrow arr2) => (forall x y . arr1 x y -> arr2 x y) -> Eff2 arr1 m b -> arr2 a b 
-- eval fn (MkEff2 arr1) = undefined

-- |
-- Effect based on Arrow-like two parameter type constructors (i.e. Arrows, Profunctors)
data Eff2 arr r b where
  MkEff2 :: arr () b -> Eff2 arr r b

embedEff2 :: Member (Embed m) r => (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
embedEff2 = interpretEff2Kl embed

-- | represent arrow and m and interpret it as existing effect
interpretEff2Kl :: (forall x . m x -> Sem r x) -> (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
interpretEff2Kl comp fn = interpret \case
  MkEff2 arr   -> comp (Arr.runKleisli (fn arr) ())



-- | less interesting
interpretToArrowMonad:: Member (Embed (ArrowMonad arr)) r => Sem (Eff2 arr ': r) a -> Sem r a
interpretToArrowMonad = interpret \case
  MkEff2 arr  -> embed (ArrowMonad arr)


-- Direct compilation to SemArr
interpretEff2AsSemArr
    :: forall e m r a b . FirstOrder (Eff2 e) "interpret"
    => (forall x . m x -> Sem r x)
    -> (forall x y . e x y -> Arr.Kleisli m x y)
    -> SemArr (Eff2 e ': r) a b
    -> SemArr r a b
interpretEff2AsSemArr comp fn = semArrCompl (interpretEff2Kl comp fn)




