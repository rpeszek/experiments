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
-- using e ~ w and removing w from the definition would prevent the need
-- for AllowAmbiguousTypes and TypeApplications (but I like AllowAmbiguousTypes)
-- and I like w being explicit
class (Functor (f e)) => Recover e w f where
    recover :: f e a -> f e (Either e (w, a)) 
    -- ^ if error is recoverable, @recover@ converts to a non-failing  @f e@  with no error accumulation as Left
    -- for successful computations @Right (w, a)@ is returned with possibly accumulated warnings.     


-- |
-- avoid TypeApplications if recovering @Either e (()),_)@
recoverTrivial :: forall e f a. Recover e () f => f e a -> f e (Either e ((), a)) 
recoverTrivial = recover

-- |
-- avoid TypeApplications if recovering @Either e (e,_)@
recover' :: forall e f a. Recover e e f => f e a -> f e (Either e (e, a)) 
recover' = recover


recoverErrorsAndWarns :: forall e f a. Recover e e f => f e a -> f e e
recoverErrorsAndWarns = fmap (id ||| fst) . recover

-- | this loses warnings!
recoverResult :: forall w e f a. Recover e w f => f e a -> f e (Either e a)
recoverResult = fmap (fmap snd) . recover @e @w


class (Functor (f e), Functor g) => Recoverable e w f g where
    recoverIn :: f e a -> g (Either e (w, a))     

checkRecoverableSuccess :: forall w e f a. (Recoverable e w f Identity) => f e a -> Bool
checkRecoverableSuccess = either (const False) (const True) . runIdentity . recoverIn @e @w




