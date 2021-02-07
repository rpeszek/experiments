{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PartialTypeSignatures #-}
-- |
-- WarnParser is a simple parser prototype that demonstrates
-- right-catch with warnings semantics,  compare it against TraditionalParser module.
-- 
-- Example use in "Alternative.Examples"
module Alternative.Instances.WarnParser where

-- import           Data.Either    
import           Control.Applicative 
-- import           Control.Monad
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings

newtype WarnParser s e w a = P { unP :: s -> (s, Either e (w,a)) } deriving Functor

tryLookAhead :: forall s e w a . WarnParser s e w a -> WarnParser s e w (Either e (w,a))
tryLookAhead (P f) = P g
   where
   g :: s -> (s, Either e (w, Either e (w,a)))
   g s = undefined --(s, Right . snd $ f s)

instance Monoid w => Applicative (WarnParser s e w) where
  pure a = P (\s -> (s, Right (mempty, a)))
  P ff <*> P xx = P $ \s0 -> case ff s0 of   
    (s1, Left err) -> (s1, Left err)
    (s1, Right (w1,f) ) -> case xx s1 of          
      (s2, Left err) -> (s2, Left err)
      (s2, Right (w2,x) ) -> (s2, Right (w1 <> w2, f x))   

instance Monoid w => Monad (WarnParser s e w) where
    P f >>= ph = P g
     where 
        g s = 
          let (s1, ewa) = f s             
              eh = fmap (fmap ph) ewa
          in  case eh of
                  Left err -> (s1, Left err)
                  Right (w :: w, P h) -> 
                    let (s2, ewa2) = h s1
                    in (s2,  fmap (\(w2,a) -> (w <> w2, a)) ewa2) -- lat line problem

instance Monoid e => Alternative (WarnParser s e e) where
  empty = failParse mempty
  (<|>) = orElse        

-- -- * combinators

orElse :: Semigroup e => WarnParser s e e a -> WarnParser s e e a -> WarnParser s e e a
orElse (P f1) (P f2) = P $ \s0 -> case f1 s0 of
  (s1, Left err) -> 
       case f2 s1 of
         (s2, Right (w,a)) -> (s2, Right (err<> w, a)) 
         (s2, Left e2) -> (s2, Left (err<> e2)) 
  x -> x             

runParser :: WarnParser s e w a -> s -> (Either e (w,a))
runParser (P p) s = snd $ p s 

testParser :: WarnParser s e w a -> s -> (s, Either e (w,a))
testParser (P p) s = p s 


failParse :: e -> WarnParser s e w a
failParse e = P $ \stream -> (stream, Left e) 

-- | does not consume input if failed
-- >>> testParser (string "some-key") "some-key boo" 
-- (" boo",Right ((),"some-key"))
string :: Monoid w => T.Text -> WarnParser T.Text [String] w T.Text 
string txt = P (\s -> case T.stripPrefix txt s of
      Just rest -> (rest, Right (mempty, txt))
      Nothing -> (s, Left [T.unpack $ txt <> " no parse"])) 

-- consumes one or more spaces
spaces :: Monoid w =>  WarnParser T.Text e w ()
spaces = P (\s -> (T.stripStart s, Right (mempty, ())))     