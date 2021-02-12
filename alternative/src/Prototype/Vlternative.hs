
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TupleSections #-}

-- | 
-- Conceptual experiment with alternative to `Alternative`
-- goal is a principled design (with laws) which is Alternative like and
-- exposes error semantics
--
-- @many@ and @some@ are implemented in @WonadPlus@ at this moment only
module Prototype.Vlternative where

import           Control.Applicative
import           Control.Arrow
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
-- Represents `Alternative` with a specified failure type  
--
-- translations:
--
-- empty = failure mempty
-- <|>   = <->
-- 

class (Applicative (f e)) => Vlternative e f where
    failure :: e -> f e a  -- name @fail@ is taken, should terminate applicative, monad
    (<->)   :: f e a -> f e a -> f e a  -- ^ alternative like combinator with @|@ twisted
                                                      
    warn ::  e -> f e a -> f e a
    warn er a = failure er <-> a 

-- | note no need for monoid contraint with nonsensical Left mempty
instance Semigroup e => Vlternative e Either where
    failure = Left
    Left e1 <-> Left e2 = Left $ e1 <> e2 
    Left e1 <-> Right a = Right a
    Right a <-> _ = Right a

-- note this needs monoid because e is both error and accumulating warning, mempty means no warnings have accumulated
instance Monoid e => Vlternative e (ErrWarn e) where
    failure e = EW $ Left e
    (<->) =  altEw

instance (Monad m, Monoid e)=> Vlternative e (Reord ErrWarnT m e) where
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




-- * VlternativeCollect

class (Semigroup e, Vlternative e f) => VlternativeCollect e f where
    (<-->)   :: f e (e, a) -> f e (e, a) -> f e (e, a) 
    -- ^ 
    -- expected to accumulate encountered failures as @first e@


noWarn :: (Monoid e, Functor (f e)) => f e a -> f e (e, a)
noWarn a = fmap (mempty,) a

recoverVlternativeCollect :: forall e f a . (Monad (f e), Monoid e, Recover e e (f e)) => f e (e, a) -> f e (e, a) -> f e (e, a) 
recoverVlternativeCollect  ae be = do
        era <- recover @e @e ae
        case era of 
            Right _ -> ae -- run full a effect
            Left e -> fmap ((e <>) *** id) be

-- | Left bias version, collect all version should also be possible
instance Semigroup e => VlternativeCollect e Either where
    Left e1 <--> Left e2 = Left $ e1 <> e2 
    Left e1 <--> Right (e2, a) = Right (e1 <> e2, a)
    Right (e1, a) <--> _ = Right (e1, a)

instance  Monoid e => VlternativeCollect e (ErrWarn e) where
    (<-->) = recoverVlternativeCollect

instance (Monad m, Monoid e) => VlternativeCollect e (Reord ErrWarnT m e) where
    (<-->) = recoverVlternativeCollect

instance Monoid e => VlternativeCollect e (Warn.WarnParser s e) where
    (<-->) = recoverVlternativeCollect

-- not very good
instance  VlternativeCollect () (Trad.TraditionalParser s) where
    (<-->) = recoverVlternativeCollect

instance VlternativeCollect () (F2Lift Maybe) where
    (<-->) = recoverVlternativeCollect


-- |
-- laws: (assume Recover instance and Collect instance)
--
-- (TODO type check the laws)
--
-- @
--
-- without critical errors:
-- 
-- recover (failure e) = pure (Left e)
-- recover (pure a) = pure (Right (mempty, a))   
--
-- ((failure e1) <-> pure (e2, a)) = pure (Right (e1 <> e2, a))   (1) 
-- ((failure e1) <-> (failure e2)) = failure (e1 <> e2)           (1) 
-- (pure (e1, a) <-> (failure e1)) = pure (?, a) -- typically ? = e1, could be relaxed (2) 
--
-- (?=) ignores warnings if any
--
-- def: x ?= y iff recovered value is the same after errors / warnings are ignored
-- 
--
-- u <-> (v <-> w)  =  (u <-> v) <-> w           -- (3)
--
-- f <*> failure e  ?= failure e                   -- (4 gen right zero) some failure for some ?
--
-- (a <-> b) <*> c  ?= (a <*> c) <-> (b <*> c)    -- (5) Left Distribution
-- (a <*> (b <|> c)) ?= (a <*> b) <|> (a <*> c)   -- (6) Right Distribution  
--
--
-- (pure (e, a)) <-> x = pure (e, a)              -- (7 left catch) 
-- (pure (e, a)) <-> x = pure (?, a)              -- relaxed 7
--
--
-- considering critical errors, needs more thinking
--
-- conceptually (1) will not hold and remaining laws will need to be relaxed, described in some way 
--
-- recover (crit) /= pure x - for any x
-- (crit <-> pure a) =  crit
-- (pure (e,a) <-> crit) = (pure (e,a)) 
-- @
--
laws = () :: ()


