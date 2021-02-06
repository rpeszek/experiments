{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

-- | 
-- Add static error information to existing Alternative
-- This will work with simple alternatives like Maybe.
--
-- Example use in "Alternative.Examples"
module Alternative.Instances.Annotate where

import Control.Applicative 
import Data.Functor.Classes
import Control.Arrow
import qualified Data.Foldable as F
import Data.Either
import Data.Maybe
import Data.Monoid
import Prototype.Recover 

-- MonadZero alike
class Applicative f => AlternativeMinus f where
    noOpFail :: f a

instance AlternativeMinus Maybe where
    noOpFail = Nothing  



data Annotate f e a = Annotate (Either e e) (f a) deriving (Show, Eq, Functor)

-- |
-- Annotate computations of type @f a@ with a label @e@
-- labeling is done using Right, any error accumulations switches annotation(s) to Left
annotate :: e -> f a -> Annotate f e a
annotate err fa = Annotate (Right err) fa



instance (Applicative f, CheckSuccess f, Monoid e) => Applicative (Annotate f e) where
    pure a = Annotate (Right mempty) (pure a)
    a1 <*> a2 = case (normalize a1, normalize a2)
          of (Annotate e1 ax1, Annotate e2 ax2) ->
              Annotate (e1 `acLefts` e2) (ax1 <*> ax2) -- this example uses `Validation` like accumulation for <*>
      where
       acLefts :: Either e e -> Either e e -> Either e e  
       acLefts a b =  Left . F.fold $ lefts [a,b]


instance (Monoid e, CheckSuccess f, AlternativeMinus f) => Alternative (Annotate f e) where
    empty = Annotate (Left mempty) noOpFail
    a1 <|> a2 = alt a1 a2            


-- |
-- >>> check (annotate "boo" $ Just 1)
-- Just ("",Just 1)
-- >>> check (annotate "boo" Nothing)
-- Just ("boo",Nothing)
-- >>> check $ annotate "foo" Nothing <|> annotate "foo" (Just 1)
-- Just ("foo",Just 1)
-- >>> check $ annotate "foo" Nothing  <|> annotate "bar" Nothing 
-- Just ("foobar",Nothing)
-- >>> check $ annotate "foo" (Just 1) <|> annotate "foo" (Just 2)
-- Just ("",Just 1)
-- >>> check (annotate "foo" Nothing <*> annotate "bar" Nothing)
-- Just ("foobar",Nothing)
-- >>> check $ annotate "foo" Nothing <*> annotate "bar" (Just 1) 
-- Just ("foo",Nothing)
-- >>> check $ annotate "foo" Nothing <*> annotate "bar" (Just 1)  <*> annotate "baz" Nothing
-- Just ("foobaz",Nothing)
--
-- check $ annotate "bar" (Just ()) <*> annotate "foo" Nothing
-- Just ("bar",Nothing)
check :: (CheckSuccess f, Applicative f, Monoid e) =>
     Annotate f e a -> f (e, Maybe a)
check =  fmap ((id ||| const mempty) *** id) . runAnnotate 

check' :: (CheckSuccess f, Applicative f, Monoid e) =>
     Annotate f e a -> f (Either e (e, a))
check' =  fmap cvrt . runAnnotate
  where
   cvrt (Left err, Just a) = Right (err, a)
   cvrt (Left err, Nothing) = Left err
   cvrt (Right _, Just a) = Right (mempty, a)
   cnrt (Right w, Nothing) = undefined -- Left mempty -- impossible

runAnnotate :: (CheckSuccess f, Applicative f) =>
     Annotate f e a -> f (Either e e, Maybe a)
runAnnotate (Annotate e fa) =
        if checkSuccess fa 
        then fmap ((e,) . Just) fa
        else pure (either Left Left $ e, Nothing)

   
alt :: (AlternativeMinus f, CheckSuccess f, Monoid e) => Annotate f e a -> Annotate f e a -> Annotate f e a
alt a b = case (normalize a, normalize b) of
    (Annotate e1 ax1, Annotate e2 ax2) ->
        case (e1, e2) of
            (Right _, _) -> Annotate e1 ax1 -- a
            (Left _, Right _) -> Annotate e1 ax2 
            (Left m1, Left m2) -> Annotate (Left $ m1 <> m2) noOpFail 

normalize :: (Applicative f, CheckSuccess f) => Annotate f e a -> Annotate f e a
normalize (Annotate e fa) = 
    if checkSuccess fa 
    then Annotate e fa
    else Annotate (either Left Left e) fa 


