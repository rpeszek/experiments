{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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
import           Alternative.Instances.REW 
import           Alternative.Instances.Annotate

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
-- RdrWarnErr outputs 
--
-- >>> runREW emplP'' $ "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> runREW emplP'' "id last-firs-name dept boss2"
-- Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]
--
-- >>> runREW emplP'' "id last-first-name dept boss"
-- Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplP'' :: RdrWarnErr B.ByteString [String] [String] Employee
emplP'' = 
   Employee 
   <$> rew idP 
   <*> (rew nameP1  <|> rew nameP2 )
   <*> rew deptP 
   <*> (rew bossP1  <|> rew bossP2 <|> rew bossP3)
   where
        rew :: AT.Parser B.ByteString a ->  RdrWarnErr B.ByteString [String] [String] a
        rew p = REW $ singleErr . A.parseOnly p

        singleErr :: Either e a -> Either [e] ([e], a)
        singleErr (Left e) =  Left [e]
        singleErr (Right r) = Right ([], r)

-- |
-- Transformer @ErrWarnT@ annotates failures.
--
-- >>> A.parseOnly (Trf.runErrWarnT emplTrP) "id last-first-name dept boss1"
-- Right (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> A.parseOnly (Trf.runErrWarnT emplTrP) "id last-firs-name dept boss2"
-- Right (Left ["nameP1","nameP2"])
--
-- >>> A.parseOnly (Trf.runErrWarnT emplTrP) "id last-first-name dept boss"
-- Right (Right (["bossP1","bossP2"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})) 
emplTrP :: Trf.ErrWarnT [String] [String] (AT.Parser B.ByteString) Employee
emplTrP = 
   Employee 
   <$> lft ["idP"] idP
   <*> (lft ["nameP1"] nameP1 <|> lft ["nameP2"] nameP2)
   <*> lft ["deptP"] deptP
   <*> (lft ["bossP1"] bossP1 <|> lft ["bossP2"] bossP2 <|> lft ["bossP3"] bossP3)  
   where
        lft :: forall a . [String] -> AT.Parser B.ByteString a ->  Trf.ErrWarnT [String] [String] (AT.Parser B.ByteString) a
        lft msg p = do
           res :: Either a [String] <- lift $ A.eitherP p (pure msg)
           case res of 
              Right _ -> Trf.err msg
              Left r -> pure r



-- |
-- This is far less interesting than ErrWarnT transformer.
-- 
-- Annotate outputs to see which failed.
--
-- >>> check . emplAnn $ "id last-first-name dept boss1"
-- Just ([],Just (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"}))
--
-- Note more errors reported in this example (because <*> uses `Validation` like definition)
--
-- >>> check . emplAnn $ "id last-firs-name dept boss2"
-- Just (["nameP1","nameP2","bossP1"],Nothing)
--
-- >>> check . emplAnn $ "id last-first-name dept boss"
-- Just (["bossP1","bossP2"],Just (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"}))
emplAnn ::  B.ByteString -> Annotate Maybe [String] Employee
emplAnn txt = 
   Employee 
   <$> annotate ["idP"] (mb idP)
   <*> (annotate ["nameP1"] (mb nameP1) <|> annotate ["nameP2"] (mb nameP2))
   <*> annotate ["deptP"] (mb deptP)
   <*> (annotate ["bossP1"] (mb bossP1) <|> annotate ["bossP2"] (mb bossP2) <|> annotate ["bossP3"] (mb bossP3))  
   where 
     mb :: AT.Parser B.ByteString a -> Maybe a
     mb p = either (const Nothing) Just $ A.parseOnly p txt

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

idWp = 123 `onKeywordWp` "id"
nameWp1 = "Smith John"  `onKeywordWp` "last-first-name"
nameWp2 = Warn.failParse ["first-last-name not implemented yet"]
deptWp =  "Billing" `onKeywordWp` "dept"
bossWp1 = "Jim K" `onKeywordWp` "boss1"     
bossWp2 = "Kim J" `onKeywordWp` "boss2"    
bossWp3 = pure "Mij K bosses everyone" 



onKeywordWp :: a -> T.Text -> Warn.WarnParser T.Text [String] [String] a
onKeywordWp val key = Warn.string key >> Warn.spaces >> pure val
    
-- |
-- TWarnParser parser outputs, similar to @Either e (e, _)@ outputs.
--
-- >>> Warn.runParser emplWp "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> Warn.runParser emplWp "id last-firs-name dept boss2"
-- Left ["last-first-name no parse","first-last-name not implemented yet"]
--
-- >>> Warn.runParser emplWp "id last-first-name dept boss"
-- Right (["boss1 no parse","boss2 no parse"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplWp :: Warn.WarnParser T.Text [String] [String] Employee
emplWp = 
   Employee 
   <$> idWp
   <*> (nameWp1 <|> nameWp2)
   <*> deptWp
   <*> (bossWp1 <|> bossWp2 <|> bossWp3)      
