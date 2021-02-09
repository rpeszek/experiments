{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | @Recover@ typeclasses provide semantics for learning about errors and warnings
--
-- TODO Recover instances are orphan kept in Prototype.Vlternative for now
module Prototype.Recover where

import           Data.Functor.Identity 
-- import           Data.Either   
-- import           Data.Functor.Classes
import           Control.Arrow


-- |

-- TODO consider typing @e w@ in the superclass, e.g  @(Functor (f e w)) =>@
-- this will remove need to use AllowAmbiguousTypes and TypeApplications but will need extra work to use with types not parametrized by @e@ and @w@
-- Vlternative and WonadPlus are currently typed with only @e@ type variable

class (Functor f) => Recover e w f where
    recover :: f a -> f (Either e (w, a)) 
    -- ^ if error is recoverable, @recover@ converts to a non-failing  @f e@  with no error accumulation as Left
    -- for successful computations @Right (w, a)@ is returned with possibly accumulated warnings.     



recoverF :: forall e w f a c. Recover e w f => (Either e (w, a) -> c) -> f a -> f c
recoverF f = fmap f . recover

recoverA :: forall e w f a c. (Recover e w f, Applicative f) => f (Either e (w, a) -> c) -> f a -> f c
recoverA f a = f <*> recover a

recoverM :: forall e w f a c. (Recover e w f, Monad f) => (Either e (w, a) -> f c) -> f a -> f c
recoverM f a = recover a >>= f


-- |
-- avoid TypeApplications of both @w and @e if recovering @Either e (e,_)@
recover' :: forall e f a. Recover e e f => f a -> f (Either e (e, a)) 
recover' = recover


recoverErrorsAndWarns :: forall e f a. Recover e e f => f a -> f e
recoverErrorsAndWarns = fmap (id ||| fst) . recover

-- | this loses warnings!
recoverResult :: forall e w f a. Recover e w f => f a -> f (Either e a)
recoverResult = fmap (fmap snd) . recover @e @w


class (Functor f, Functor g) => Recoverable e w f g where
    recoverIn :: f a -> g (Either e (w, a))     

checkRecoverableSuccess :: forall e w f a. (Recoverable e w f Identity) => f a -> Bool
checkRecoverableSuccess = either (const False) (const True) . runIdentity . recoverIn @e @w




