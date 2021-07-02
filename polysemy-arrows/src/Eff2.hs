
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds
          --  , PolyKinds
           , TypeApplications, Arrows #-}
module Eff2 where

import qualified Control.Arrow as Arr
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.CustomErrors

import SemArr

import Control.Arrow


-- type Effect2 arr r b = Eff2 (Eff2Free arr) r b

-- |
-- Effect based on Arrow-like two parameter type constructors (i.e. Arrows, Profunctors)
data Eff2 arr r b where
  MkEff2 :: arr () b -> Eff2 arr r b

-- | represent arrow and m and interpret it as existing effect
-- can be used to just reintrpret arrow effects (see Echoer2)
interpretEff2 :: (forall x . m x -> Sem r x) -> (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
interpretEff2 comp fn = interpret \case
  MkEff2 arr   -> comp (Arr.runKleisli (fn arr) ())

embedEff2 :: Member (Embed m) r => (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (Eff2 arr ': r) a -> Sem r a
embedEff2 = interpretEff2 embed



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
interpretEff2AsSemArr comp fn = semArrCompl (interpretEff2 comp fn)




