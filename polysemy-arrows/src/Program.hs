{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications #-}

module Program where

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Resource
import Teletype

data CustomException = ThisException | ThatException deriving Show

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch @CustomException work \e -> writeTTY $ "Caught " ++ show e
 where
  work = bracket readTTY (const $ writeTTY "exiting bracket") \input -> do
    writeTTY "entering bracket"
    case input of
      "explode"     -> throw ThisException
      "weird stuff" -> writeTTY input *> throw ThatException
      _             -> writeTTY input *> writeTTY "no exceptions"

testRun :: IO (Either CustomException ())
testRun
  = runFinal
  . embedToFinal @IO
  . resourceToIOFinal
  . errorToIOFinal @CustomException
  . teletypeToIO
  $ program

testRun2 :: IO (Either CustomException ())
testRun2
  = runM
  . resourceToIO
  . runError @CustomException
  . teletypeToIO
  $ program  