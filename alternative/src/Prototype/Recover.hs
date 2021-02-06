{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

-- | recover type classes provide semantics for learning about errors and warnings
module Prototype.Recover where

import Data.Functor.Identity 
import Data.Either   
import Data.Functor.Classes

class (Functor (f e)) => Recover e f where
    recover :: f e a -> f e (Either e (e, a)) 
    -- ^ if error is recoverable, @recover@ converts to a non-failing  @f e@  with no error accumulation as Left
    -- for successful computations @Right (w, a)@ is returned with possibly accumulated warnings.     

class (Functor (f e), Functor g) => Recoverable e f g where
    recoverIn :: f e a -> g (Either e (e, a))     



class CheckSuccess f where
    checkSuccess :: f a -> Bool

-- | CheckSuccess is quite restrictive but will allow any `Eq1` instance 
newtype EQ1 f a = EQ1 (f a) 
instance (Applicative f, Eq1 f) => CheckSuccess (EQ1 f) where
    checkSuccess (EQ1 fa) = fmap (const ()) fa `eq1` pure ()

instance CheckSuccess Maybe where
    checkSuccess = maybe False (const True)

checkRecoverableSuccess :: (Recoverable e f Identity) => f e a -> Bool
checkRecoverableSuccess = either (const False) (const True) . runIdentity . recoverIn