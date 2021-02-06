{-# LANGUAGE OverloadedStrings #-}
module Prototype.Vlternative.Example where

import Prototype.Vlternative
import Alternative.Instances.ErrWarn
import Alternative.Instances.REW
import Alternative.Example

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Types as AT
import qualified Data.List as L

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

-- |
-- convoluted monadic test that uses warnings
-- 
-- >>> runREW testV "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
-- >>> runREW testV "id first-last-name dept boss2"
-- Right ([Name1Err "\"last-first-name\": not enough input",Name1Err "Name1 Failed"],Employee {id = 123, name = "John Smith", dept = "Billing", boss = "Kim J"})
-- >>> runREW testV "id first-last-name dept boss1"
-- Left [Name1Err "Name1 Failed",OtherErr "\"boss2\": not enough input"]
-- >>> runREW testV "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
testV :: RdrWarnErr B.ByteString [EmployeeEx] [EmployeeEx] Employee
testV = do
        id <- rew OtherErr idP
        let rewNm = rew Name1Err nameP1 <-> rew Name2Err nameP2'
        nm <- rewNm
        wrn <- recoverErrors $ rewNm
        -- nameP1 errors go with boss2, rest with boss1 or boss3
        dep <- rew OtherErr deptP
        boss <- if L.any isName1Err wrn
                then 
                    warn [Name1Err "Name1 Failed"] $ rew OtherErr bossP2 -- TODO nice replace error semantics similar to warn
                else 
                    (rew OtherErr bossP1) <-> (rew OtherErr bossP3)   
        pure $ Employee id nm dep boss
    where
        rew :: (String -> EmployeeEx) -> AT.Parser B.ByteString a ->  RdrWarnErr B.ByteString [EmployeeEx] [EmployeeEx] a
        rew f p = REW $ singleErr f . A.parseOnly p

        singleErr :: (String -> EmployeeEx) -> Either String a -> Either [EmployeeEx] ([EmployeeEx], a)
        singleErr f (Left e) =  Left [f e]
        singleErr _ (Right r) = Right ([], r)

        nameP2' = "John Smith"  `onKeyword` "first-last-name"

    
data EmployeeEx = Name1Err String | Name2Err String | OtherErr String deriving (Eq, Show)

isName1Err :: EmployeeEx -> Bool
isName1Err (Name1Err _) = True
isName1Err _ = False
