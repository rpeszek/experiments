{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}


-- | 
-- @Recover@ typeclasses provide semantics for learning about errors and warnings
--
-- @Recover@ is stronger than @MonadPlus@
--
-- @Recover@ can help in defining laws for alternative replacements
--
module Prototype.Recover where

import           Prototype.Common    

import           Data.Functor.Identity 
import           Control.Arrow
import           Control.Applicative
-- import           Control.Monad


import           Alternative.Instances.ErrWarn
import           Alternative.Instances.ErrWarnT
import qualified Alternative.Instances.TraditionalParser as Trad
import qualified Alternative.Instances.WarnParser as Warn

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> import qualified Data.Text as T

-- |
-- Conceptual experiment
--
-- If type constructor f stores error and/or warning information then
-- "recover" that information.
--
-- If @f@ is applicative and w is monoid, the expected behavior is
--
-- @
-- recover (pure a) = pure (Right (mempty, a))
-- @
--
-- if, in addition, errors of type @f a@ can be constructed using @fail :: e -> f a@
-- then the expected behavior is:
--
-- @
-- recover (fail e) = pure (Left e)
-- @
-- 
-- E.g.
-- 
-- @ 
-- recover (Left e) = Right (Left e)
-- recover (Right r) = Right (Right (mempty, r))
-- @
--
-- For computations like Parsers this is expected to backtrack when evaluated. That is:
--
-- @
-- fmap snd $ (,) <$> recover x <*> c  = c  
-- recover x >> c = c
-- @
--
-- Laws are documented with Vlternative laws
-- Recover helps to express Vlternative laws
--
-- Since the result is still in @f@, we could use @recover@ to define critical errors as errors that do not recover.
--
-- @
-- recover (critfail e) /= pure x for any x
-- @
--
-- TODO consider typing @e w@ in the superclass, e.g  @(Functor (f e w)) =>@
-- this will remove need to use AllowAmbiguousTypes and TypeApplications but will need extra work to use with types not parametrized by @e@ and @w@
-- Vlternative and WonadPlus are currently typed with only @e@ type variable

class (Functor f) => Recover e w f where
    recover :: f a -> f (Either e (w, a)) 
    -- ^ if error is recoverable, @recover@ converts to a non-failing  @f e@  with no error accumulation as Left
    -- for successful computations @Right (w, a)@ is returned with possibly accumulated warnings.  
    -- For computations like Parsers this is expected to backtrack when evaluated.    


instance Recover e () (Either e) where
    recover (Left e) = Right (Left e)
    recover (Right r) = Right (Right ((), r))

instance Recover () () Maybe where
    recover (Just r) = Just (Right ((), r))
    recover Nothing = Just (Left ())

instance Monoid w => Recover e w (ErrWarn e w) where
    recover (EW x) = EW $ Right (mempty, x)

instance (Monad m, Monoid w) => Recover e w (ErrWarnT e w m) where
    recover = recoverEwt

instance Recover e () (Trad.TraditionalParser s e) where
    recover a = fmap (fmap ((),)) $ Trad.tryLookAhead a

instance Monoid w => Recover e w (Warn.WarnParser s e w) where
    recover = Warn.tryLookAhead   


instance Recover () () (F2Lift Maybe ()) where
    recover (F2Lift Nothing) = F2Lift $ Just $ Left ()
    recover (F2Lift (Just a)) = F2Lift $ Just $ Right ((), a)   

instance Recover e w (f e w m) => Recover e w (Reord f m e w) where
   recover (Reord fa) = Reord $ recover fa    


-- * combinators

-- | Recover is stronger than MonadPlus (for parser this will auto-backtrack when first computation fails)
-- this matches e.g. trifecta's backtracking behavior
recoverMplus :: forall e w m a. (Recover e w m, Monad m) => m a -> m a -> m a
recoverMplus a b = do
        era :: Either e (w, a) <- recover a
        case era of 
            Right _ -> a -- run full a effect
            Left _ -> b

-- |
-- Consumes input and recovers, this assumes that a <|> b will consume input on a.
-- if <|> backtracks on a failure this will also backtrack on failures.
-- Applicative style.
--
-- could be useful on typeclass level    
--     
-- >>> Warn.testParser (recoverConsumeA @ [T.Text] @ [T.Text] (Warn.string id "sk" <|> Warn.string id "ss")) "sskboo"
-- ("kboo",Right (["sk no parse"],Right (["sk no parse"],"ss")))
recoverConsumeA :: forall w e f a . (Recover e w (f e), Alternative (f e)) => f e a -> f e (Either e (w,a))
recoverConsumeA a = go <$> (recover @e @w a) <*> ( (fmap (consume) a) <|> (fmap ignore $ recover @e @w a)) 
  where 
     go :: Either e (w, a) -> () -> Either e (w, a)
     go r _ = r
     -- mostly not needed, const works
     consume :: b -> ()
     consume x = seq x ()
     ignore :: b -> ()
     ignore = const ()

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

isSuccess :: forall e w f a. (Recover e w f) => f a -> f Bool
isSuccess = fmap (either (const False) (const True)) . recoverResult @e @w

-- |
-- more general version, currently not used
class (Functor f, Functor g) => Recoverable e w f g where
    recoverIn :: f a -> g (Either e (w, a))     

checkRecoverableSuccess :: forall e w f a. (Recoverable e w f Identity) => f a -> Bool
checkRecoverableSuccess = either (const False) (const True) . runIdentity . recoverIn @e @w


