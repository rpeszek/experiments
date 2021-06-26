{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

module Teletype where

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo


runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  -- For each WriteTTY in our program, consume an output by appending it to the
  -- list in a ([String], a)
  = runOutputMonoid pure
  -- Treat each element of our list of strings as a line of input
  . runInputList i
  -- Reinterpret our effect in terms of Input and Output
  . reinterpret2 \case
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg



-- Let's pretend
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

-- echo forever
test :: IO ()
test = runM . teletypeToIO $ echo
