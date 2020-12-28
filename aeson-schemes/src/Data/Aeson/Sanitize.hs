{-# LANGUAGE OverloadedStrings        #-}

-- |
-- Sometimes Haskell needs to talk to external application which is not very type savy.
-- For example, I have encountered missing timestamp being represented in JSON intermittently as
--
-- * @"some_date": Null@
-- * @"some_date": ""@
--
-- The second is clearly wrong but happens.  One way to handle this is to pre-process Aeson values.
-- This uses recursion schemes to do that
module Data.Aeson.Sanitize where

import           Data.Aeson.RecScheme (ValueF(..))
import qualified Data.Aeson as A
import           Data.Functor.Foldable

import           Data.Semigroup
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T
-- >>> let tst = A.object ["name" A..= ("name" :: String), "created_date" A..= (""::String), "nested" A..= A.object ["boo_date" A..= ("" :: String), "num" A..= (1 :: Int)]]
-- >>> :{
-- sanitizeEmptyDates :: A.Object -> A.Object
-- sanitizeEmptyDates = HM.mapWithKey sanitizefn
--   where 
--       sanitizefn k v = if T.isSuffixOf "_date" k && v == A.String "" then A.Null else v
-- :}

-- |
-- >>> mapObjects sanitizeEmptyDates tst
-- Object (fromList [("nested",Object (fromList [("num",Number 1.0),("boo_date",Null)])),("created_date",Null),("name",String "name")])
mapObjects :: (A.Object -> A.Object) -> A.Value -> A.Value
mapObjects fn = cata (ext fn)
   where 
       ext :: (A.Object -> A.Object) -> ValueF A.Value -> A.Value
       ext fn (ObjectF o) = A.Object $ fn o 
       ext _ x = embed x

