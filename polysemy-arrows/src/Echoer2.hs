
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds
          --  , PolyKinds
           , TypeApplications, Arrows #-}

-- | reinterpreting Arrow Effects as Arrow Effects
module Echoer2 where

import qualified Control.Arrow as Arr
import Polysemy
import Polysemy.Internal

-- import Control.Category
-- import qualified Control.Category as Cat
import Control.Arrow

import Eff2Free
import SemArr
import Eff2
import Teletype2 hiding (interpreter)

 

data Echoer2 a b where
  DoEcho2 :: Echoer2 String String

doEcho2 :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Echoer2)) r => String -> Sem r String
doEcho2 s =  send (MkEff2 (Pure (const s) >>> Effect DoEcho2)) 

doEcho2A :: forall (r :: [Effect]). Member (Eff2 (Eff2Free Echoer2)) r => SemArr r String String
doEcho2A = semArr doEcho2 

-- | nice use of arrows to create complex effects
echoer2ToKl :: Member (Eff2 (Eff2Free Teletype2)) r => Echoer2 a b -> Arr.Kleisli (Sem r) a b
echoer2ToKl DoEcho2 = proc inp -> do
  _ <- writeTTY2A -< inp
  txt <- readTTY2A -< ()
  if txt == ""
  then arr id -< "No Input"
  else arr id -< "You said " <> txt

-- | interpretEff2 allows to intepret down the effect
echoer2ToTeletype2 :: forall r a . Member (Eff2 (Eff2Free Teletype2)) r => Sem (Eff2 (Eff2Free Echoer2) ': r) a -> Sem r a
echoer2ToTeletype2 = interpretEff2 id (liftCompKl @ (Sem r) echoer2ToKl)

interpreter ::  r ~ '[Eff2 (Eff2Free Echoer2), Eff2 (Eff2Free Teletype2), Embed IO] => SemArr r a b -> a -> IO b
interpreter arr a = runM . embedEff2 (liftCompKl tele2ToKlIO) . echoer2ToTeletype2 $ Arr.runKleisli arr a

testEchoerA :: String -> IO String
testEchoerA  = interpreter (doEcho2A >>> doEcho2A) 