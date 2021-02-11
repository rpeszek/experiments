
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | 
-- Experiments with possible alternatives to `Alternative`
-- goal is a principled design (with laws) which is Alternative like and
-- exposes error semantics
--
-- @many@ and @some@ are implemented in @WonadPlus@ at this moment only
module Prototype.Vlternative where

import           Control.Applicative

import           Prototype.Recover
import           Alternative.Instances.ErrWarn
import           Alternative.Instances.ErrWarnT
import qualified Alternative.Instances.TraditionalParser as Trad
import qualified Alternative.Instances.WarnParser as Warn

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications -XFlexibleContexts

-- |
-- Alternative with A upside down.
--
-- Example use in "Vlternative.Example"
--
-- Represents `Alternative` with a specified failure monoid type  
-- and ability to recover from failure  
-- instance has ability to accumulate errors as warnings.
--
-- translations:
--
-- empty = failure mempty
-- <|>   = <->
-- 
-- laws: (assume Recover instance)
--
-- (TODO type check the laws)
--
-- @
--
-- without critical errors:
-- 
-- recover (failure e) = pure (Left e)
-- recover (pure a) = pure (Right (mempty, a))   
-- recover (failure e <-> pure a) = pure (Right (e, a))  -- (1*)
-- recover (pure a <-> failure e) = recover (pure (Right (mempty, a))) = pure (Right (mempty, a))  -- (2*)
--
-- (?=) ignores warnings if any
--
-- def: x ?= y iff recoverResultMaybe x = recoverResultMaybe y 
--       where recoverResultMaybe = fmap (either (const Nothing) Just) . recoverResult
-- 
--
-- (failure e) <-> y ?=  y                       -- (1) follows from (1*)
-- x <-> (failure e) = x                         -- (2) stronger than (2*), obsoletes (2*)
-- u <-> (v <-> w)  =  (u <-> v) <-> w           -- (3)
--
-- f <*> failure e ?= failure e                   -- (4 gen right zero), warnings are optional, hence ?=
--
-- (a <-> b) <*> c  ?= (a <*> c) <-> (b <*> c)    -- (5) Left Distribution
-- (a <*> (b <|> c)) ?= (a <*> b) <|> (a <*> c)   -- (6) Right Distribution  
--
--
-- (pure a) <-> x = pure a                         -- (7 left catch) 
--
--
-- considering critical errors, needs more thinking
--
-- conceptually (1) will not hold and remaining laws will need to be relaxed, described in some way 
--
-- recover (crit) /= pure x - for any x
-- (crit <-> pure a) =  crit
-- recover (pure a <-> crit) = recover (pure a) = pure (Right (mempty, a))
-- @
--
--
class (Applicative (f e)) => Vlternative e f where
    failure :: e -> f e a  -- name @fail@ is taken, should terminate applicative, monad
    (<->)   :: f e a -> f e a -> f e a  -- ^ alternative like combinator with @|@ twisted
                                                      
    warn ::  e -> f e a -> f e a
    warn er a = failure er <-> a 


isSuccess :: forall w e f a. (Vlternative e f, Recover e w (f e)) => f e a -> f e Bool
isSuccess = fmap (either (const False) (const True)) . recoverResult @ e @ w 



-- * instances (TODO instances using [] instead of general Monoids to avoid likes of Data.Monoid.First)

instance  Vlternative [e] (ErrWarn [e]) where
    failure e = EW $ Left e
    a <-> b = a <|> b

instance (Monad m)=> Vlternative [e] (Reord ErrWarnT m [e]) where
    failure = Reord . err
    Reord a <-> Reord b = Reord $ a <|> b

instance Monoid e => Vlternative e (Trad.TraditionalParser s) where
    failure = Trad.failParse
    a <-> b = a <|> b

instance Monoid e => Vlternative e (Warn.WarnParser s e) where
    failure = Warn.failParse
    a <-> b = a <|> b


instance Vlternative () (F2Lift Maybe) where
    failure _ = F2Lift Nothing
    F2Lift a <-> F2Lift b = F2Lift $ a <|> b




