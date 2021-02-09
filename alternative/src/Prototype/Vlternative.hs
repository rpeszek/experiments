
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | 
-- Experiments with possible alternatives to `Alternative`
-- goal is a principled design (with laws) which is Alternative like and
-- exposes error semantics
--
-- @many@ and @some@ are implemented in @WonadPlus@
module Prototype.Vlternative where

import           Control.Applicative
-- import           Control.Arrow
-- import           Data.Functor.Classes

import           Prototype.Recover
import           Alternative.Instances.ErrWarn
import           Alternative.Instances.REW
import           Alternative.Instances.Annotate
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
-- (TODO type check, verify these)
--
-- @
--
-- without critical errors:
--
-- recover (failure e) = pure (e, Nothing)
-- recover (pure a) = pure (mempty, Just a)
-- recover (failure e <-> pure a) = pure (e, Just a)
-- recover (pure a <-> failure e) = recover (pure a) = pure (mempty, Just a)
--
-- ?? (thinking about it) considering critical errors:
--
-- recover (pure a) = pure (mempty, Just a)
-- recover (failure mempty) = pure (e, Nothing)  - ?? can mempty be critical? probably not
-- recover (failure e) = 
--           pure (e, Nothing) -- non-critical
--           failure ? -- critical (or @failure mempty@ or @failure e@? - ?? probably @failure mempty@ allowing to catch critical)
-- recover (failure e <-> pure a) = 
--           pure (e, Just a)  -- non-critical
--           failure ?  -- critical
-- recover (pure a <-> failure e) = recover (pure a) = pure (mempty, Just a)
--
-- x ?= y iff recoverResultMaybe x = recoverResultMaybe y
--       where recoverResultMaybe = fmap (either (const Nothing) Just) . recoverResult
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
class (Applicative (f e)) => Vlternative e f where
    failure :: e -> f e a  -- name fail is taken, should terminate applicative, monad
    (<->)   :: f e a -> f e a -> f e a  -- ^ alternative like combinator with @|@ twisted
                                                      
    warn ::  e -> f e a -> f e a
    warn er a = failure er <-> a 



-- | 
-- needed for the laws
isSuccess :: forall w e f a. (Vlternative e f, Recover e w (f e)) => f e a -> f e Bool
isSuccess = fmap (either (const False) (const True)) . recoverResult @ e @ w 





-- * instances

-- TODO Recover instances are now orphan, keeping them here is convenient for prototyping



instance Monoid e => Vlternative e (ErrWarn e) where
    failure e = EW $ Left e

    EW (Left e1) <-> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <-> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r)
    l@(EW (Right _)) <-> _ = l

instance Monoid e => Recover e e (ErrWarn e e) where
    recover (EW x) = EW $ Right (mempty, x)


instance (Monoid e) => Vlternative e (RdrWarnErr r e) where 
    failure e = REW . const $ Left e
    REW f <-> REW g = REW (\r ->
             case (f r, g r) of 
                (Left e1, Left e2) -> Left $ e1 <> e2
                (Left e1, Right (w2, x)) -> Right (e1 <> w2, x)
                (l@(Right _), _) -> l
          )

instance (Monoid e) => Recover e e (RdrWarnErr r e e) where           
    recover (REW f) = REW (\r -> Right (mempty, f r))

-- |
-- Extend Alternative with static errors to Vlternative
--
-- >>> recover' @ String (annotate "boo" $ Just 1)
-- Annotate (Right "") (Just (Right ("",1)))
-- >>> recover' @ String (annotate "boo" $ Nothing)
-- Annotate (Right "") (Just (Left "boo"))
-- >>> recover' @ String ((annotate "boo" $ Nothing) <-> (annotate "" $ Just 1))
-- Annotate (Right "") (Just (Right ("boo",1)))
-- >>> recover' @ String ((annotate "foo" $ Nothing) <-> (annotate "bar" $ Nothing))
-- Annotate (Right "") (Just (Left "foobar"))
instance (Monoid e,  CheckSuccess f, AlternativeMinus f) => Vlternative e (Annotate f) where
    failure e = Annotate (Left e) noOpFail
    a <-> b = a <|> b

instance (Monoid e,  CheckSuccess f, Applicative f) => Recover e e (Annotate f e) where
    recover a = Annotate (Right mempty) $ check' a


-- TraditionalParser does not have much error handling and this does not make much sense
instance Monoid e => Vlternative e (Trad.TraditionalParser s) where
    failure = Trad.failParse
    a <-> b = a <|> b

instance Monoid e => Recover e () (Trad.TraditionalParser s e) where
    recover a = fmap (fmap ((),)) $ Trad.tryLookAhead a
   
instance Monoid e => Vlternative e (Warn.WarnParser s e) where
    failure = Warn.failParse
    a <-> b = a <|> b

instance Monoid e => Recover e e (Warn.WarnParser s e e) where
    recover a = Warn.tryLookAhead a   



newtype F2Lift f e a = F2Lift {unF2Lift :: f a} deriving (Show, Eq, Functor)

instance Applicative f => Applicative (F2Lift f e) where
    pure x = F2Lift $ pure x
    F2Lift a <*> F2Lift b = F2Lift $ a <*> b

instance Monad m => Monad (F2Lift m e) where
    F2Lift a >>= f = F2Lift $ a >>= (unF2Lift . f)    

instance  Vlternative () (F2Lift Maybe) where
    failure _ = F2Lift Nothing
    F2Lift a <-> F2Lift b = F2Lift $ a <|> b

instance Recover () () (F2Lift Maybe ()) where
    recover (F2Lift Nothing) = F2Lift $ Just $ Left ()
    recover (F2Lift (Just a)) = F2Lift $ Just $ Right ((), a)   
