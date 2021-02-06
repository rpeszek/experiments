{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | recover typeclasses provide semantics for learning about errors and warnings
-- TODO Recover instances are orphan kept in Prototype.Vlternative for now
module Prototype.Recover where

import           Data.Functor.Identity 
-- import           Data.Either   
-- import           Data.Functor.Classes
import           Control.Arrow


class (Functor (f e)) => Recover e f where
    recover :: f e a -> f e (Either e (e, a)) 
    -- ^ if error is recoverable, @recover@ converts to a non-failing  @f e@  with no error accumulation as Left
    -- for successful computations @Right (w, a)@ is returned with possibly accumulated warnings.     

-- | recovers errors and warnings
recoverErrors :: forall e f a. Recover e f => f e a -> f e e
recoverErrors = fmap (id ||| fst) . recover

-- | this loses warnings!
recoverResult :: forall e f a. Recover e f => f e a -> f e (Either e a)
recoverResult = fmap (fmap snd) . recover


class (Functor (f e), Functor g) => Recoverable e f g where
    recoverIn :: f e a -> g (Either e (e, a))     

checkRecoverableSuccess :: (Recoverable e f Identity) => f e a -> Bool
checkRecoverableSuccess = either (const False) (const True) . runIdentity . recoverIn




