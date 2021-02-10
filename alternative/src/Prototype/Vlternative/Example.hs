{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- work in progress
module Prototype.Vlternative.Example where

import Prototype.Vlternative
import Alternative.Instances.ErrWarn
import Alternative.Example

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
-- import qualified Data.Attoparsec.Types as AT

-- $setup
-- >>> :set -XOverloadedStrings


-- |
-- ErrWarn outputs 
--
-- >>> runEW $ emplV "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> runEW $ emplV "id last-firs-name dept boss2"
-- Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]
--
-- >>> runEW $ emplV "id last-first-name dept boss"
-- Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplV :: B.ByteString -> ErrWarn [String] [String] Employee
emplV txt = 
   Employee 
   <$> ew idP 
   <*> (ew nameP1  <-> ew nameP2 )
   <*> ew deptP 
   <*> (ew bossP1  <-> ew bossP2 <-> ew bossP3)
   where
        ew p = cnvt p txt

        cnvt :: A.Parser a -> B.ByteString -> ErrWarn [String] [String] a
        cnvt p s = singleErr $ A.parseOnly p s

        singleErr :: Either e a -> ErrWarn [e] [e] a
        singleErr (Left e) = EW $ Left [e]
        singleErr (Right r) = EW $ Right ([], r)



    

