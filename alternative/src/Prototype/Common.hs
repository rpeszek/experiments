{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor #-}

module Prototype.Common where

-- | 
-- list that terminates with information `e` at the end
--
-- needed for @wsome@ and @wsome@
data SList e a = Last e | Snoc (SList e a) a deriving (Show, Eq, Functor)

stake :: Int -> SList e a -> (Maybe e, [a])
stake n (Last e) = (Just e, [])
stake 0 (Snoc lst a) = (Nothing, [])
stake n (Snoc lst a) = fmap (a :) (stake (n-1) lst) 

-- * convenience wrapper types

-- |
-- convenience new type allows to treat something like @Maybe a@ as @Maybe err e@
newtype F2Lift f e a = F2Lift {unF2Lift :: f a} deriving (Show, Eq, Functor)

instance Applicative f => Applicative (F2Lift f e) where
    pure x = F2Lift $ pure x
    F2Lift a <*> F2Lift b = F2Lift $ a <*> b

instance Monad m => Monad (F2Lift m e) where
    F2Lift a >>= f = F2Lift $ a >>= (unF2Lift . f)  


newtype Reord f (m :: * -> *) e w a = Reord {unReord :: f e w m a} deriving (Show, Eq, Functor)

instance Applicative (f e w m) => Applicative (Reord f m e w) where
    pure x = Reord $ pure x
    Reord a <*> Reord b = Reord $ a <*> b

instance Monad (f e w m) => Monad (Reord f m e w) where
    Reord a >>= f = Reord $ a >>= (unReord . f)     

