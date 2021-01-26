{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Instances where

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Types as AT
import qualified Data.Attoparsec.ByteString.Char8 as ACh
import Control.Applicative 
import Control.Monad

import Instances.REW 

-- $setup
-- >>> :set -XOverloadedStrings

-- * @Monoid e => Alternative (Either e)@
--
-- Following instance conflicts with deprecated instance defined in tranformers.
-- would need a new type to be more useful.
--
-- instance Monoid e => Alternative (Either e) where 
--     empty  = Left mempty
--     Left e1 <|> Left e2 = Left $ e1 <> e2
--     (Left _) <|> r = r
--     l  <|> _ = l


-- * Either e (w, a) instances

newtype ErrWarn e w a = EW {runEW :: Either e (w, a)} deriving (Eq, Show, Functor)

instance (Monoid w) => Applicative (ErrWarn e w) where
    pure x = EW $ Right (mempty, x)
    EW (Left e) <*> _ = EW $ Left e
    EW (Right (u, f)) <*> EW (Right (v, x)) = EW (Right (u <> v, f x))
    EW (Right (u, f)) <*> EW (Left e)  = EW $ Left e

instance (Monoid w) => Monad (ErrWarn e w) where   
    EW (Left e) >>= _  = EW $ Left e
    EW (Right (u, x)) >>= k = 
        case k x of 
            EW (Right (v, b)) -> EW (Right (u <> v, b))
            EW (Left e) -> EW (Left e)


instance (Monoid e) => Alternative (ErrWarn e e) where 
    empty  = EW $ Left mempty
    EW (Left e1) <|> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <|> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r)
    l@(EW (Right _)) <|> _ = l

instance (Monoid e) => MonadPlus (ErrWarn e e)


-- * example

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
-- >>> emplP' "id last-first-name dept boss1"
-- EW {runEW = Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})}
--
-- >>> emplP' "id last-firs-name dept boss2"
-- EW {runEW = Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]}
--
-- >>> emplP' "id last-first-name dept boss"
-- EW {runEW = Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})}
emplP' :: B.ByteString -> ErrWarn [String] [String] Employee
emplP' txt = 
   Employee 
   <$> ew idP 
   <*> (ew nameP1  <|> ew nameP2 )
   <*> ew deptP 
   <*> (ew bossP1  <|> ew bossP2 <|> ew bossP3)
   where
        ew p = cnvt p txt

        cnvt :: A.Parser a -> B.ByteString -> ErrWarn [String] [String] a
        cnvt p s = singleErr $ A.parseOnly p s

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

