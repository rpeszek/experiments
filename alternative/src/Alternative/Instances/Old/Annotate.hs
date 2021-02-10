{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
-- ErrWarnT mostly obsoletes this, keeping it around as a useful conceptual easy prototype
--
-- Add static error information to existing Alternative
-- This will work with simple alternatives like Maybe.
--
-- 
module Alternative.Instances.Old.Annotate where

import Control.Applicative 
import Data.Functor.Classes
import Control.Arrow
import qualified Data.Foldable as F
import Data.Either
-- import Data.Maybe
-- import Data.Monoid


import qualified Data.ByteString as B
-- import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as A
-- import qualified Data.Attoparsec.Combinator as A
import qualified Data.Attoparsec.Types as AT

import           Alternative.Example hiding (id)
import           Prototype.Recover
import           Prototype.Vlternative

-- $setup
-- >>> :set -XOverloadedStrings -XTypeApplications -XFlexibleContexts

-- ApplicativeZero 
class Applicative f => AlternativeMinus f where
    noOpFail :: f a

instance AlternativeMinus Maybe where
    noOpFail = Nothing  

class CheckSuccess f where
    checkSuccess :: f a -> Bool

-- | CheckSuccess is quite restrictive but will allow any `Eq1` instance 
newtype EQ1 f a = EQ1 (f a) 
instance (Applicative f, Eq1 f) => CheckSuccess (EQ1 f) where
    checkSuccess (EQ1 fa) = fmap (const ()) fa `eq1` pure ()

instance CheckSuccess Maybe where
    checkSuccess = maybe False (const True)

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


-- * prototypes

-- |
-- Extend Alternative with static errors to Vlternative
--
-- >>> recover' @ String (annotate "boo" $ Just 1)
-- Annotate (Right "") (Just (Right ("",1)))
-- >>> recover' @ String (annotate "boo" $ Nothing)
-- Annotate (Right "") (Just (Left "boo"))
-- >>> recover' @ String ((annotate "boo" $ Nothing) <-> (annotate "" $ Just 1))
-- Annotate (Right "") (Just (Right ("boo",1)))
-- >>> recover' @ String ((annotate "foo" $ Nothing) <-> (annotate "bar" $ Nothing))
-- Annotate (Right "") (Just (Left "foobar"))
instance (Monoid e,  CheckSuccess f, AlternativeMinus f) => Vlternative e (Annotate f) where
    failure e = Annotate (Left e) noOpFail
    a <-> b = a <|> b

instance (Monoid e,  CheckSuccess f, Applicative f) => Recover e e (Annotate f e) where
    recover a = Annotate (Right mempty) $ check' a


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
   cvrt :: Monoid e => (Either e e, Maybe a) -> Either e (e, a)   
   cvrt (Left err, Just a) = Right (err, a)
   cvrt (Left err, Nothing) = Left err
   cvrt (Right _, Just a) = Right (mempty, a)
   cvrt (Right _, Nothing) =  Left mempty -- impossible


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


-- * example

-- |
-- This is far less interesting than ErrWarnT transformer.
-- 
-- Annotate outputs to see which failed.
--
-- >>> check . emplAnn $ "id last-first-name dept boss1"
-- Just ([],Just (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Jim K"}))
--
-- Note more errors reported in this example (because <*> uses `Validation` like definition)
--
-- >>> check . emplAnn $ "id last-firs-name dept boss2"
-- Just (["nameP1","nameP2","bossP1"],Nothing)
--
-- >>> check . emplAnn $ "id last-first-name dept boss"
-- Just (["bossP1","bossP2"],Just (Employee {id = 123, name = "Smith John", dept = "Billing", boss = "Mij K bosses everyone"}))
emplAnn ::  B.ByteString -> Annotate Maybe [String] Employee
emplAnn txt = 
   Employee 
   <$> annotate ["idP"] (mb idP)
   <*> (annotate ["nameP1"] (mb nameP1) <|> annotate ["nameP2"] (mb nameP2))
   <*> annotate ["deptP"] (mb deptP)
   <*> (annotate ["bossP1"] (mb bossP1) <|> annotate ["bossP2"] (mb bossP2) <|> annotate ["bossP3"] (mb bossP3))  
   where 
     mb :: AT.Parser B.ByteString a -> Maybe a
     mb p = either (const Nothing) Just $ A.parseOnly p txt
