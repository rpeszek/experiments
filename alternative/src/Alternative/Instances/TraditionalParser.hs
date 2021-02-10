{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

-- |
-- TraditionalParser is a simple parser prototype that has typical <|> implementation.
-- 
-- Example use shown in "Alternative.Examples"
module Alternative.Instances.TraditionalParser where

-- import           Data.Either    
import           Control.Applicative 
-- import           Control.Monad
import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings

newtype TraditionalParser s e a = P { unP :: s -> (s, Either e a) } deriving Functor

tryLookAhead :: forall s e a . TraditionalParser s e a -> TraditionalParser s e (Either e a)
tryLookAhead (P f) = P g
   where
   g :: s -> (s, Either e (Either e a))
   g s = (s, Right . snd $ f s)

instance Applicative (TraditionalParser s e) where
  pure a = P (\s -> (s, Right a))
  P ff <*> P xx = P $ \s0 -> case ff s0 of   
    (s1, Left err) -> (s1, Left err)
    (s1, Right f ) -> case xx s1 of          
      (s2, Left err) -> (s2, Left err)
      (s2, Right x ) -> (s2, Right (f x))   

instance Monad (TraditionalParser s e) where
    P f >>= ph = P g
     where 
        g s = 
          let (s1, ea) = f s             
              eh = fmap ph ea
          in  case eh of
                  Left err -> (s1, Left err)
                  Right (P h) -> h s1

instance Monoid e => Alternative (TraditionalParser s e) where
  empty = failParse mempty
  (<|>) = orElse        

-- * combinators

orElse :: TraditionalParser s e a -> TraditionalParser s e a -> TraditionalParser s e a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left err) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)               

runParser :: TraditionalParser s e a -> s -> Either e a
runParser (P p) s = snd $ p s 

testParser :: TraditionalParser s e a -> s -> (s, Either e a)
testParser (P p) s = p s 


failParse :: e -> TraditionalParser s e a
failParse e = P $ \stream -> (stream, Left e) 

-- | does not consume input if failed
-- >>> testParser (string "some-key") "some-key boo" 
-- (" boo",Right "some-key")
string :: T.Text -> TraditionalParser T.Text String T.Text 
string txt = P (\s -> case T.stripPrefix txt s of
      Just rest -> (rest, Right txt)
      Nothing -> (s, Left (T.unpack $ txt <> " no parse"))) 

-- consumes one or more spaces
spaces ::  TraditionalParser T.Text String ()
spaces = P (\s -> (T.stripStart s, Right ()))     