
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds
          --  , PolyKinds
           , TypeApplications, Arrows #-}
module ArrowEffects where

import qualified Control.Arrow as Arr
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.CustomErrors
import Polysemy.Input
import Polysemy.Output    

import Teletype
import ArrowConsumption
import Data.Kind
import Control.Category
import qualified Control.Category as Cat
import Control.Arrow

import FreeArr


-- h (ArrowMonad h) = (h, ()) >>> Arr.app

-- eval :: forall arr1 arr2 m a b . (Arrow arr1, Arrow arr2) => (forall x y . arr1 x y -> arr2 x y) -> ArrowEff arr1 m b -> arr2 a b 
-- eval fn (MkArrowEff arr1) = undefined

data ArrowEff arr r b where
  MkArrowEff :: arr () b -> ArrowEff arr r b

arrowEffToKl :: Member (Embed m) r => (forall x y . arr x y ->  Arr.Kleisli m x y) -> Sem (ArrowEff arr ': r) a -> Sem r a
arrowEffToKl fn = interpret \case
  MkArrowEff arr   -> embed (Arr.runKleisli (fn arr) ())


-- | not interesting
arrowToM :: Member (Embed (ArrowMonad arr)) r => Sem (ArrowEff arr ': r) a -> Sem r a
arrowToM = interpret \case
  MkArrowEff arr  -> embed (ArrowMonad arr)


data Teletype2 a b where
  ReadTTY2  :: Teletype2 () String
  WriteTTY2 :: Teletype2 String ()

readTTY2A :: forall (r :: [Effect]). Member (ArrowEff (FreeArr Teletype2)) r => SemArr r () String
readTTY2A =  constKl readTTY2

-- -- forall (r :: [Effect]).
readTTY2 :: forall (r :: [Effect]). Member (ArrowEff (FreeArr Teletype2)) r => Sem r String
readTTY2 =  send (MkArrowEff (Effect ReadTTY2) :: ArrowEff (FreeArr Teletype2) (Sem r) String) 

writeTTY2A :: forall (r :: [Effect]). Member (ArrowEff (FreeArr Teletype2)) r => SemArr r String ()
writeTTY2A = kl writeTTY2

writeTTY2 :: forall (r :: [Effect]). Member (ArrowEff (FreeArr Teletype2)) r => String -> Sem r ()
writeTTY2 s =  send (MkArrowEff (Pure (const s) >>> Effect WriteTTY2) :: ArrowEff (FreeArr Teletype2) (Sem r) ()) 

tele2ToKlIO :: Teletype2 a b -> Arr.Kleisli IO a b
tele2ToKlIO ReadTTY2 = Arr.Kleisli $ const getLine
tele2ToKlIO WriteTTY2 = Arr.Kleisli putStrLn

freeTele2ToKlIO :: FreeArr Teletype2 a b -> Arr.Kleisli IO a b
freeTele2ToKlIO = liftComp tele2ToKlIO


echo2A :: Member (ArrowEff (FreeArr Teletype2)) r => SemArr r () ()
echo2A = proc _ -> do
    i <- readTTY2A -< ()
    case i of 
       "" -> 
           writeTTY2A -< "Need some input"
       _ ->  
           writeTTY2A -< "You said " <> i

testA2 :: IO ()
testA2 = runM Cat.. arrowEffToKl freeTele2ToKlIO $ Arr.runKleisli echo2A ()


-- allternaives

klComp :: (forall x . Sem (e ': r) x -> Sem r x) -> SemArr (e ': r) a b -> SemArr r a b 
klComp comp (Arr.Kleisli fn) = Arr.Kleisli (comp Cat.. fn)

klComp' :: (forall x . Sem r1 x -> Sem r2 x) -> SemArr r1 a b -> SemArr r2 a b 
klComp' comp (Arr.Kleisli fn) = Arr.Kleisli (comp Cat.. fn)

-- not interesting
interKl
    :: forall e r a b . FirstOrder (ArrowEff e) "interpret"
    => (forall x y . e x y -> SemArr r x y)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> SemArr (ArrowEff e ': r) a b
    -> SemArr r a b
interKl fn = klComp' (interpret fnx)
 where 
  fnx :: forall y m. (ArrowEff e) m y -> Sem r y
  fnx (MkArrowEff e) = -- undefined
                          let kls  = fn e
                              x = Arr.runKleisli kls undefined
                          in x -- (MkArrowEff e) = undefined



