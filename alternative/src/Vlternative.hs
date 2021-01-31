
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

-- | 
-- Experiments with possible alternatives to `Alternative`
-- goal is a principled design (with laws) which is Alternative like and
-- exposes error semantics
module Vlternative where

import Control.Applicative
import Alternative.Instances.ErrWarn
import Alternative.Instances.REW


-- |
-- Alternative with A upside down.
--
-- Represents `Alternative` with a specified failure monoid type  
-- and abitily to recover from failure  
-- instance has ability to accumulate errors as warnings.
--
-- translations:
--
-- empty = failure mempty
-- <|>   = <->
-- 
-- laws: (TODO type check, verify these)
-- @
-- recover (failure e) = pure (e, Nothing)
-- recover (pure a) = pure (mempty, Just a)
-- recover (failure e <-> pure a) = pure (e, Just a)
-- recover (pure a <-> failure e) = recover (pure a) = pure (mempty, Just a)
--
--
-- x ?= y iff recoverResultMaybe x = recoverResultMaybe y
--       where recoverResultMaybe = either Nothing Just . recoverResult
--
-- (failure e) <-> y ?=  y   
-- x <-> (failure e) ?= x   
-- u <-> (v <-> w)  =  (u <-> v) <-> w           -- (3)
--
-- f <*> failure e ?= failure e                   -- (4 gen right zero)
--
-- (a <-> b) <*> c  ?= (a <*> c) <-> (b <*> c)    -- (5) Left Distribution
-- (a <*> (b <|> c)) ?= (a <*> b) <|> (a <*> c)   -- (6) Right Distribution  
--
--
-- (pure a) <-> x = pure a                         -- (7 left catch) 
-- @
--
-- possible enhancements:
--
-- mapping over e. 
--
--   *  mapping over e accumulated in a successful computation @x :: f e a@ (isSuccess x = pure True) 
--   *  mapping e in a failed computation @x :: f e a@ (toSuccess x = pure False) 
--
class (Monoid e, Applicative (f e)) => Vlternative e f where
    failure :: e -> f e a  -- name fail is taken, should terminate applicative, monad
    (<->)   :: f e a -> f e a -> f e a  -- ^ alternative like combinator with @|@ twisted
    recover :: f e a -> f e (e, Maybe a) -- returns accumulated failures, converts to non-failing with no accumulation
                                
     
warn :: forall e f a. Vlternative e f => e -> f e a -> f e a
warn er a = failure er <-> a 

recoverWarnings :: forall e f a. Vlternative e f => f e a -> f e e
recoverWarnings = fmap fst . recover

-- | this loses warnings!
recoverResult :: forall e f a. Vlternative e f => f e a -> f e (Either e a)
recoverResult = fmap cnrt . recover
    where
        cnrt :: (e, Maybe a) -> Either e a
        cnrt (_, Just r) = Right r  -- information loss
        cnrt (e, Nothing) = Left e

-- | 
-- needed for the laws
isSuccess :: forall e f a. Vlternative e f => f e a -> f e Bool
isSuccess = fmap (either (const False) (const True)) . recoverResult


-- * instances

-- |
-- For documentation, probably should not be used.
newtype Trivial f e a = Trivial (f a) deriving (Show, Eq, Functor, Applicative)

-- |
-- For documentation, probably should not be used.
-- questionable @recovery@
instance (Monoid e, Alternative f) => Vlternative e (Trivial f) where
    failure _ = Trivial empty
    Trivial a <-> Trivial b = Trivial (a <|> b)
    recover (Trivial f) = Trivial empty -- problem, it fails on recover

-- instance (Applicative f) => Applicative (Trivial f e) where


instance Monoid e => Vlternative e (ErrWarn e) where
    failure e = EW $ Left e

    EW (Left e1) <-> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <-> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r)
    l@(EW (Right _)) <-> _ = l

    recover (EW (Right (e, r))) = EW $ Right (mempty, (e, Just r))
    recover (EW (Left e)) = EW $  Right (mempty, (e, Nothing))

    
instance (Monoid e) => Vlternative e (RdrWarnErr r e) where 
    failure e = REW . const $ Left e
    REW f <-> REW g = REW (\r ->
             case (f r, g r) of 
                (Left e1, Left e2) -> Left $ e1 <> e2
                (Left e1, Right (w2, x)) -> Right (e1 <> w2, x)
                (l@(Right _), _) -> l
          )
    recover (REW f) = REW (\r -> 
            case f r of 
              Right (e, r) ->  Right (mempty, (e, Just r))
              Left e ->  Right (mempty, (e, Nothing))
            )

