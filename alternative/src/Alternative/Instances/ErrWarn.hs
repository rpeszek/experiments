{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- blue print instance for Alternative with error accumulation
--
module Alternative.Instances.ErrWarn where

import Control.Applicative 
import Control.Monad


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


instance Monoid e => Alternative (ErrWarn e e) where 
-- instance (Monoid e) => Alternative (ErrWarn e e) where 
    empty  = EW $ Left mempty
    (<|>) = altEw

-- instance (Monoid e) => MonadPlus (ErrWarn e e)
instance Monoid e => MonadPlus (ErrWarn e e)

altEw :: Semigroup e => ErrWarn e e a -> ErrWarn e e a -> ErrWarn e e a
(EW (Left e1))  `altEw` (EW (Left e2)) = EW (Left $ e1 <> e2)
(EW (Left e1))  `altEw` (EW (Right (w2, r))) = EW $ Right (e1 <> w2, r)
l@(EW (Right _)) `altEw`  _ = l

