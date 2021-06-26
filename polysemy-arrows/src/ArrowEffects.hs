
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

import Data.Kind
-- import Control.Category
-- import qualified Control.Category as Cat
import Control.Arrow

import FreeArr
import SemArr
import Eff2


data Teletype2 a b where
  ReadTTY2  :: Teletype2 () String
  WriteTTY2 :: Teletype2 String ()

-- | arrow frienly combinator
readTTY2A :: forall (r :: [Effect]). Member (Eff2 (FreeArr Teletype2)) r => SemArr r () String
readTTY2A =  constSemArr readTTY2

-- | monad frienly combinator, can be consumed by monadic programs and effects
readTTY2 :: forall (r :: [Effect]). Member (Eff2 (FreeArr Teletype2)) r => Sem r String
readTTY2 =  send (MkEff2 (Effect ReadTTY2) :: Eff2 (FreeArr Teletype2) (Sem r) String) 

writeTTY2A :: forall (r :: [Effect]). Member (Eff2 (FreeArr Teletype2)) r => SemArr r String ()
writeTTY2A = semArr writeTTY2

writeTTY2 :: forall (r :: [Effect]). Member (Eff2 (FreeArr Teletype2)) r => String -> Sem r ()
writeTTY2 s =  send (MkEff2 (Pure (const s) >>> Effect WriteTTY2) :: Eff2 (FreeArr Teletype2) (Sem r) ()) 

tele2ToKlIO :: Teletype2 a b -> Arr.Kleisli IO a b
tele2ToKlIO ReadTTY2 = Arr.Kleisli $ const getLine
tele2ToKlIO WriteTTY2 = Arr.Kleisli putStrLn


echo2A :: Member (Eff2 (FreeArr Teletype2)) r => SemArr r () ()
echo2A = proc _ -> do
    i <- readTTY2A -< ()
    case i of 
       "" -> 
           writeTTY2A -< "Need some input"
       _ ->  
           writeTTY2A -< "You said " <> i


interpreter ::  r ~ '[Eff2 (FreeArr Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreter arr a = runM . embedEff2 (liftCompKl tele2ToKlIO)  $ Arr.runKleisli arr a

testA2 :: IO ()
testA2 = interpreter echo2A ()

-- | more arrow like interpreter that chains SemArr's not Sem's
interpreter' ::  r ~ '[Eff2 (FreeArr Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreter' arr = runM . Arr.runKleisli (interpretEff2AsSemArr embed (liftCompKl tele2ToKlIO) arr) 
  

-- reinterpreting Arrow Effects as Arrow Effects

data Echoer2 a b where
  DoEcho2 :: Echoer2 String String

doEcho2 :: forall (r :: [Effect]). Member (Eff2 (FreeArr Echoer2)) r => String -> Sem r String
doEcho2 s =  send (MkEff2 (Pure (const s) >>> Effect DoEcho2) :: Eff2 (FreeArr Echoer2) (Sem r) String) 

doEcho2A :: forall (r :: [Effect]). Member (Eff2 (FreeArr Echoer2)) r => SemArr r String String
doEcho2A = semArr doEcho2 

-- | nice use of arrows to create complex effects
echoer2ToKl :: Member (Eff2 (FreeArr Teletype2)) r => Echoer2 a b -> Arr.Kleisli (Sem r) a b
echoer2ToKl DoEcho2 = proc inp -> do
  _ <- writeTTY2A -< inp
  txt <- readTTY2A -< ()
  if txt == ""
  then arr id -< "No Input"
  else arr id -< "You said " <> txt

interpretEcho2 :: forall r a . Member (Eff2 (FreeArr Teletype2)) r => Sem (Eff2 (FreeArr Echoer2) ': r) a -> Sem r a
interpretEcho2 = interpretEff2Kl id (liftCompKl @ (Sem r) echoer2ToKl)

interpreterEchoer ::  r ~ '[Eff2 (FreeArr Echoer2), Eff2 (FreeArr Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreterEchoer arr a = runM . embedEff2 (liftCompKl tele2ToKlIO) . interpretEcho2 $ Arr.runKleisli arr a

testEchoerA :: String -> IO String
testEchoerA  = interpreterEchoer (doEcho2A >>> doEcho2A) 