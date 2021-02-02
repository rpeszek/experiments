{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- | 
-- Add static error information to existing Alternative
module Alternative.Instances.Annotate where

import Control.Applicative 
import Data.Functor.Classes

-- |
-- For documentation, probably should not be used.
data Annotate f e a = Annotate e (f a) deriving (Show, Eq, Functor)

instance (Applicative f, Monoid e) => Applicative (Annotate f e) where
    pure a = Annotate mempty (pure a)
    (Annotate _ a1) <*> (Annotate _ a2) = Annotate mempty (a1 <*> a2) -- fmap (Annotate mempty) (a1 <*> a2)



instance (Monoid e,  Eq1 f, Alternative f) => Alternative (Annotate f e) where
    empty = Annotate mempty empty
    (Annotate e1 a) <|> (Annotate e2 b) = 
        if which `eq1` pure True
        then Annotate e1 res
        else Annotate (e1 <> e2) res
       where 
           r  = ((True,) <$> a) <|> ((False,) <$> b)
           which = fst <$> r
           res = snd <$> r

-- |
-- >>> check (Annotate "boo" $ Just 1)
-- Just ("boo",Just 1)
-- >>> check (Annotate "boo" $ Nothing)
-- Just ("boo",Nothing)
-- >>> check ((Annotate "boo" $ Nothing) <|> (Annotate "" $ Just 1))
-- Just ("boo",Just 1)
-- >>> check ((Annotate "foo" $ Nothing) <|> (Annotate "bar" $ Nothing))
-- Just ("foobar",Nothing)
check :: (Eq1 f, Applicative f, Monoid e, Eq e) =>
       Annotate f e b -> f (e, Maybe b)
check (Annotate e fa) =
        if succ `eq1` pure ()
        then fmap ((e,) . Just) fa
        else pure (e, Nothing)
    where 
        r = ((),) <$> fa
        succ = fst <$> r