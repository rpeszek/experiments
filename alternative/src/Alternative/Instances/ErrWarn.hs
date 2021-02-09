{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Alternative.Instances.ErrWarn where

import Control.Applicative 
import Control.Monad



-- * @Monoid e => Alternative (Either e)@
--
-- Following instance conflicts with deprecated instance defined in transformers.
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

-- |
-- A more general @instance (Monoid e) => Alternative (ErrWarn e e)@ would be
-- questionable with some monoids like @First@ or @Last@.
--
-- However using `Max` would be interesting!
instance Alternative (ErrWarn [e] [e]) where 
-- instance (Monoid e) => Alternative (ErrWarn e e) where 
    empty  = EW $ Left mempty
    EW (Left e1) <|> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <|> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r)
    l@(EW (Right _)) <|> _ = l

-- instance (Monoid e) => MonadPlus (ErrWarn e e)
instance MonadPlus (ErrWarn [e] [e])

