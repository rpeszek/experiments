{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Obsolete, can be now replaced with transformer stack.
-- If I remember correctly, EWR is an airport in New Jersey.
--
-- Alternative instance for @r -> Either e (w, a)@ that accumulates errors as warnings.
--
-- Example use in "Alternative.Examples"

module Alternative.Instances.Old.EWR where

import qualified Data.ByteString as B
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.Types as AT
import           Control.Applicative 

import qualified Data.List as L

import           Alternative.Example
import           Prototype.Recover
import           Prototype.Vlternative


-- $setup
-- >>> :set -XOverloadedStrings

-- Conceptual prototype

newtype ErrWarnRdr r e w a = EWR {runEWR :: r -> Either e (w, a)} deriving Functor

instance (Monoid w) => Applicative (ErrWarnRdr r e w) where
    pure x = EWR . const $ Right (mempty, x)
    EWR f <*> EWR g = EWR (\r -> 
              case (f r, g r) of 
                  (Left e, _) -> Left e
                  (Right (u, f'), Right (v, x)) -> Right (u <> v, f' x)
                  (Right (u, f'), Left e) -> Left e
            )
       -- where 
           -- x = f `asTypeOf` _ -- r -> Either e (w, a -> b)
           -- y = g `asTypeOf` _    --  r -> Either e (w, a)

instance (Monoid w) => Monad (ErrWarnRdr r e w) where   
     (EWR f) >>= k = EWR (\r ->
           case f r of 
              Left e -> Left e
              Right (u, x) -> 
                    let EWR h = k x
                    in case h r of 
                            Right (v, b) -> Right (u <> v, b)
                            Left e -> Left e
        )

-- |
-- A more general @instance (Monoid e) => Alternative (ErrWarnRdr r e e)@ would be
-- questionable with some monoids like @First@ or @Last@.
-- 
instance Alternative (ErrWarnRdr r [e] [e]) where    
-- instance (Monoid e) => Alternative (ErrWarnRdr r e e) where 
    empty  = EWR . const $ Left mempty
    EWR f <|> EWR g = EWR (\r ->
             case (f r, g r) of 
                (Left e1, Left e2) -> Left $ e1 <> e2
                (Left e1, Right (w2, x)) -> Right (e1 <> w2, x)
                (l@(Right _), _) -> l
          )


-- * prototypes

instance (Monoid e) => Vlternative e (ErrWarnRdr r e) where 
    failure e = EWR . const $ Left e
    EWR f <-> EWR g = EWR (\r ->
             case (f r, g r) of 
                (Left e1, Left e2) -> Left $ e1 <> e2
                (Left e1, Right (w2, x)) -> Right (e1 <> w2, x)
                (l@(Right _), _) -> l
          )

instance (Monoid e) => Recover e e (ErrWarnRdr r e e) where           
    recover (EWR f) = EWR (\r -> Right (mempty, f r))




-- * examples

-- |
-- ErrWarnRdr outputs 
--
-- >>> runEWR emplP'' $ "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
--
-- >>> runEWR emplP'' "id last-firs-name dept boss2"
-- Left ["\"last-first-name\": not enough input","Failed reading: first-last-name not implemented yet"]
--
-- >>> runEWR emplP'' "id last-first-name dept boss"
-- Right (["\"boss1\": not enough input","\"boss2\": not enough input"],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"})
emplP'' :: ErrWarnRdr B.ByteString [String] [String] Employee
emplP'' = 
   Employee 
   <$> rew idP 
   <*> (rew nameP1  <|> rew nameP2 )
   <*> rew deptP 
   <*> (rew bossP1  <|> rew bossP2 <|> rew bossP3)
   where
        rew :: AT.Parser B.ByteString a ->  ErrWarnRdr B.ByteString [String] [String] a
        rew p = EWR $ singleErr . A.parseOnly p

        singleErr :: Either e a -> Either [e] ([e], a)
        singleErr (Left e) =  Left [e]
        singleErr (Right r) = Right ([], r)

-- |
-- convoluted monadic test that uses warnings
-- 
-- >>> runEWR testV "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
-- >>> runEWR testV "id first-last-name dept boss2"
-- Right ([Name1Err "\"last-first-name\": not enough input",Name1Err "Name1 Failed"],Employee {id = 123, name = "John Smith", dept = "Billing", boss = "Kim J"})
-- >>> runEWR testV "id first-last-name dept boss1"
-- Left [Name1Err "Name1 Failed",OtherErr "\"boss2\": not enough input"]
-- >>> runEWR testV "id last-first-name dept boss1"
-- Right ([],Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"})
testV :: ErrWarnRdr B.ByteString [EmployeeEx] [EmployeeEx] Employee
testV = do
        id <- rew OtherErr idP
        let rewNm = rew Name1Err nameP1 <-> rew Name2Err nameP2'
        nm <- rewNm
        wrn <- recoverErrorsAndWarns @ [EmployeeEx] $ rewNm
        -- nameP1 errors go with boss2, rest with boss1 or boss3
        dep <- rew OtherErr deptP
        boss <- if L.any isName1Err wrn
                then 
                    warn [Name1Err "Name1 Failed"] $ rew OtherErr bossP2 -- TODO nice replace error semantics similar to warn
                else 
                    (rew OtherErr bossP1) <-> (rew OtherErr bossP3)   
        pure $ Employee id nm dep boss
    where
        rew :: (String -> EmployeeEx) -> AT.Parser B.ByteString a ->  ErrWarnRdr B.ByteString [EmployeeEx] [EmployeeEx] a
        rew f p = EWR $ singleErr f . A.parseOnly p

        singleErr :: (String -> EmployeeEx) -> Either String a -> Either [EmployeeEx] ([EmployeeEx], a)
        singleErr f (Left e) =  Left [f e]
        singleErr _ (Right r) = Right ([], r)

        nameP2' = "John Smith"  `onKeyword` "first-last-name"        

data EmployeeEx = Name1Err String | Name2Err String | OtherErr String deriving (Eq, Show)

isName1Err :: EmployeeEx -> Bool
isName1Err (Name1Err _) = True
isName1Err _ = False        