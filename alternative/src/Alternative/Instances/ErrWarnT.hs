{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Alternative.Instances.ErrWarnT where

import Alternative.Instances.ErrWarn
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer -- strict writer
import Data.Tuple

newtype ErrWarnT e w m a = ErrWarnT {runErrWarnT :: m (Either e (w,a))} deriving (Functor)

errWarn :: Monad m => Either e (w,a) -> ErrWarnT e w m a
errWarn m = ErrWarnT (pure m)

err :: Monad m => e -> ErrWarnT e w m a
err e = errWarn (Left e)

-- |
-- apply ErrWarn semantics to ErrWarnT
instance (Monad m) => Alternative (ErrWarnT [e] [e] m) where
   empty = ErrWarnT $ pure $ Left mempty
   (ErrWarnT m1) <|> (ErrWarnT m2) = ErrWarnT $ do
       e1 <- m1
       e2 <- m2
       pure $ runEW $ EW e1 <|> EW e2 

-- * rest is boilerplate

-- ExceptT needs Monad contraint to be applicative
instance (Monoid w, Monad m) => Applicative (ErrWarnT e w m) where
    pure x = ErrWarnT $ pure $ Right (mempty, x)
    x <*> y = fromTransf (toTransf x <*> toTransf y)

instance (Monoid w, Monad m) => Monad (ErrWarnT e w m) where
    x >>= f = fromTransf $  (toTransf x) >>= (toTransf . f) 

instance Monoid w => MonadTrans (ErrWarnT e w) where
    lift :: forall m a . Monad m => m a -> ErrWarnT e w m a
    lift = fromTransf @e @w @m . lift . lift




toTransf :: forall e w m a . Functor m => ErrWarnT e w m a -> WriterT w (ExceptT e m) a
toTransf (ErrWarnT mewa) = WriterT (ExceptT $ fmap (fmap swap) mewa)

fromTransf :: forall e w m a . (Functor m) => WriterT w (ExceptT e m) a -> ErrWarnT e w m a
fromTransf (WriterT (ExceptT x)) = ErrWarnT (fmap (fmap swap) x)

