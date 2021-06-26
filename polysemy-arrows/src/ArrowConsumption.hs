
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications, Arrows #-}
module ArrowConsumption where

import qualified Control.Arrow as Arr
import Polysemy
import SemArr  

import Teletype



readTTYA :: forall (r :: [Effect]). MemberWithError Teletype r => SemArr r () String
readTTYA = constSemArr readTTY

writeTTYA :: forall (r :: [Effect]). MemberWithError Teletype r => SemArr r String ()
writeTTYA = semArr writeTTY

echoA :: Member Teletype r => SemArr r () ()
echoA = proc _ -> do
    i <- readTTYA -< ()
    case i of 
       "" -> 
           writeTTYA -< "Need some input"
       _ ->  
           writeTTYA -< "You said " <> i

testA :: IO ()
testA = runM . teletypeToIO $ Arr.runKleisli echoA ()

