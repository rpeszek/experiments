
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications, Arrows #-}
module ArrowConsumption where

import qualified Control.Arrow as Arr
import Polysemy
import Polysemy.Input
import Polysemy.Output    

import Teletype

type SemArr r a b = Arr.Kleisli (Sem r) a b

constKl :: (Sem r) b -> SemArr r () b
constKl c = Arr.Kleisli (const c)

kl :: (a -> (Sem r) b )-> SemArr r a b
kl = Arr.Kleisli

readTTYA :: forall (r :: [Effect]). MemberWithError Teletype r => SemArr r () String
readTTYA = constKl readTTY

writeTTYA :: forall (r :: [Effect]). MemberWithError Teletype r => SemArr r String ()
writeTTYA = kl writeTTY

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

