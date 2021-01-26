{-# LANGUAGE OverloadedStrings #-}

module Laws where

import qualified Data.Attoparsec.ByteString as A
import Control.Applicative 


testSuccess :: A.Parser a -> A.Result a
testSuccess p = A.parse p "foo"

-- | Example invalidating u <|> empty  =  u
--
-- >>> testFail lhs
-- Fail "bar" [] "Failed reading: empty"
--
-- >>> testFail rhs
-- Fail "bar" [] "string"
testFail :: A.Parser a -> A.Result a
testFail p = A.parse p "bar"

u = A.string "foo"

lhs = u <|> empty
rhs = u




