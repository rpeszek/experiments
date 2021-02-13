{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- |
-- Extended examples supporting my blog post.
-- Examples showing improved instances of Applicative.
module Alternative.Example where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Types as AT
import qualified Data.Attoparsec.ByteString.Char8 as ACh
import           Control.Applicative 


import           Alternative.Instances.ErrWarn
import qualified Alternative.Instances.ErrWarnT as Trf

import qualified Alternative.Instances.TraditionalParser as Trad
import qualified Alternative.Instances.WarnParser as Warn
import           Control.Monad.Trans.Class

-- $setup
-- >>> :set -XOverloadedStrings


-- * attoparsec based example

idP = 123 `onKeyword` "id"
nameP1 = "Smith John"  `onKeyword` "last-first-name"
nameP2 = fail "first-last-name not implemented yet"
deptP =  "Billing" `onKeyword` "dept"
bossP1 = "Jim K" `onKeyword` "boss1"     
bossP2 = "Kim J" `onKeyword` "boss2"    
bossP3 = pure "Mij K bosses everyone" 

onKeyword :: a -> B.ByteString -> AT.Parser B.ByteString a
onKeyword val key = const val <$> A.manyTill ACh.anyChar
                    (A.lookAhead $ A.string key)
                    A.<?> show key


data Employee = Employee {
    id :: Int
    , name :: String
    , dept  :: String
    , boss :: String
   } deriving Show

-- |
-- Std parser outputs
--
-- >>> A.parseOnly emplP "id last-first-name dept boss1"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> A.parseOnly emplP "id last-firs-name dept boss2"
-- Left "Failed reading: first-last-name not implemented yet"
--
-- >>> A.parseOnly emplP "id last-first-name dept boss"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"}) 
emplP :: AT.Parser B.ByteString Employee
emplP = 
   Employee 
   <$> idP
   <*> (nameP1 <|> nameP2)
   <*> deptP
   <*> (bossP1 <|> bossP2 <|> bossP3)  

-- |
-- ErrWarn outputs 
--
-- >>> runEW $ emplP' "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> runEW $ emplP' "id last-firs-name dept boss2"
-- Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]
--
-- >>> runEW $ emplP' "id last-first-name dept boss"
-- Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplP' :: B.ByteString -> ErrWarn [String] [String] Employee
emplP' txt = 
   Employee 
   <$> ew idP 
   <*> (ew nameP1  <|> ew nameP2 )
   <*> ew deptP 
   <*> (ew bossP1  <|> ew bossP2 <|> ew bossP3)
   where
        ew :: A.Parser a  -> ErrWarn [String] [String] a
        ew p = singleErr $ A.parseOnly p txt

        singleErr :: Either e a -> ErrWarn [e] [e] a
        singleErr (Left e) = EW $ Left [e]
        singleErr (Right r) = EW $ Right ([], r)


-- |
-- Transformer @ErrWarnT@ annotates failures.
--
-- >>> A.parseOnly (Trf.runErrWarnT emplTrP) "id last-first-name dept boss1"
-- Right (Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"}))
--
-- >>> A.parseOnly (Trf.runErrWarnT emplTrP) "id last-firs-name dept boss2"
-- Right (Left [Name1Err "no parse",Name2Err "no parse"])
--
-- >>> A.parseOnly (Trf.runErrWarnT emplTrP) "id last-first-name dept boss"
-- Right (Right ([BossP1Err "no parse",BossP2Err "no parse"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"}))
emplTrP :: Trf.ErrWarnT [EmployeeParseErr T.Text] [EmployeeParseErr T.Text] (AT.Parser B.ByteString) Employee
emplTrP = 
   Employee 
   <$> lft [IdErr "no parse"] idP
   <*> (lft [Name1Err "no parse"] nameP1 <|> lft [Name2Err "no parse"] nameP2)
   <*> lft [OtherErr "deptP no parse"] deptP
   <*> (lft [BossP1Err "no parse"] bossP1 <|> lft [BossP2Err "no parse"] bossP2 <|> lft [BossP3Err "no parse"] bossP3)  
   where
        -- parser does not fail consuming the same input
        lft :: forall a . [EmployeeParseErr T.Text] -> AT.Parser B.ByteString a ->  Trf.ErrWarnT [EmployeeParseErr T.Text] [EmployeeParseErr T.Text] (AT.Parser B.ByteString) a
        lft msg p = do
           res :: Either a [EmployeeParseErr T.Text] <- lift $ A.eitherP p (pure msg)
           case res of 
              Right _ -> Trf.err msg -- fails in ErrWarnT
              Left r -> pure r

data EmployeeParseErr s = 
   IdErr s
   | Name1Err s
   | Name2Err s
   | BossP1Err s 
   | BossP2Err s 
   | BossP3Err s 
   | OtherErr s 
     deriving (Eq, Show, Functor)



-- * TraditionalParser based example

idTp = 123 `onKeywordTp` "id"
nameTp1 = "Smith John"  `onKeywordTp` "last-first-name"
nameTp2 = Trad.failParse "first-last-name not implemented yet"
deptTp =  "Billing" `onKeywordTp` "dept"
bossTp1 = "Jim K" `onKeywordTp` "boss1"     
bossTp2 = "Kim J" `onKeywordTp` "boss2"    
bossTp3 = pure "Mij K bosses everyone" 

-- |
-- >>> Trad.testParser (onKeywordTp True "some-key") "some-key boo" 
-- ("boo",Right True)
-- >>> Trad.runParser (onKeywordTp True "some-key" <|> onKeywordTp True "other-key") "some-key" 
-- Right True
-- >>> Trad.runParser (onKeywordTp True "some-key" <|> onKeywordTp True "other-key") "other-key" 
-- Right True
-- >>> Trad.runParser (onKeywordTp True "some-key" <|> onKeywordTp True "other-key") "diff-key" 
-- Left "other-key no parse"
-- >>> Trad.runParser ((,) <$> onKeywordTp True "some-key" <*> onKeywordTp True "other-key") "some-key other-key"
-- Right (True,True)
onKeywordTp :: a -> T.Text -> Trad.TraditionalParser T.Text String a
onKeywordTp val key = Trad.string key >> Trad.spaces >> pure val
    
-- |
-- TraditionalParser parser outputs
--
-- >>> Trad.runParser emplTp "id last-first-name dept boss1"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> Trad.runParser emplTp "id last-firs-name dept boss2"
-- Left "first-last-name not implemented yet"
--
-- >>> Trad.runParser emplTp "id last-first-name dept boss"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplTp :: Trad.TraditionalParser T.Text String Employee
emplTp = 
   Employee 
   <$> idTp
   <*> (nameTp1 <|> nameTp2)
   <*> deptTp
   <*> (bossTp1 <|> bossTp2 <|> bossTp3)      

-- * WarnParser based example

idWp = onKeywordWp 123 OtherErr "id"
nameWp1 = onKeywordWp "Smith John" OtherErr "last-first-name"
nameWp2 = Warn.failParse [OtherErr "first-last-name not implemented yet"]
deptWp =  onKeywordWp "Billing" OtherErr "dept"
bossWp1 = onKeywordWp "Jim K" BossP1Err "boss1"     
bossWp2 = onKeywordWp "Kim J" BossP2Err "boss2"    
bossWp3 = pure "Mij K bosses everyone" 

onKeywordWp :: a -> (T.Text -> EmployeeParseErr T.Text) -> T.Text -> Warn.WarnParser T.Text [EmployeeParseErr T.Text] [EmployeeParseErr T.Text] a
onKeywordWp val f key = Warn.string f key >> Warn.spaces >> pure val
    
-- |
-- TWarnParser parser outputs, similar to @Either e (e, _)@ outputs.
-- Example shows non-textual error output.
--
-- >>> Warn.runParser emplWp "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> Warn.runParser emplWp "id last-firs-name dept boss2"
-- Left [OtherErr "last-first-name no parse",OtherErr "first-last-name not implemented yet"]
--
-- >>> Warn.runParser emplWp "id last-first-name dept boss"
-- Right ([BossP1Err "boss1 no parse",BossP2Err "boss2 no parse"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplWp :: Warn.WarnParser T.Text [EmployeeParseErr T.Text] [EmployeeParseErr T.Text] Employee
emplWp = 
   Employee 
   <$> idWp
   <*> (nameWp1 <|> nameWp2)
   <*> deptWp
   <*> (bossWp1 <|> bossWp2 <|> bossWp3)      
