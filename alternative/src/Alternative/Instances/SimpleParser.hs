{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
-- |
-- just a simple instance to play with
module Alternative.Instances.SimpleParser where

import Data.Either    
import Control.Applicative 
import Control.Monad


newtype SimpleParser e a = P { unP :: String -> (String, Either e a) }

wtryparser :: forall e a . SimpleParser e a -> SimpleParser e (Either e a)
wtryparser (P f) = P g
   where
   g :: String -> (String, Either e (Either e a))
   g s = (s, Right . snd $ f s)

instance Functor (SimpleParser e) where
  fmap f (P h) = P $ \s -> case h s of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

instance Applicative (SimpleParser e) where
  pure a = P (\s -> (s, Right a))
  P ff <*> P xx = P $ \s0 -> case ff s0 of   
    (s1, Left err) -> (s1, Left err)
    (s1, Right f ) -> case xx s1 of          
      (s2, Left err) -> (s2, Left err)
      (s2, Right x ) -> (s2, Right (f x))   

instance Monad (SimpleParser e) where
    P f >>= ph = P g
     where 
        g s = 
          let (s1, ea) = f s             
              eh = fmap ph ea
          in  case eh of
                  Left err -> (s1, Left err)
                  Right (P h) -> h s1

instance Alternative (SimpleParser String) where
  empty = P $ \stream -> (stream, Left "empty")
  (<|>) = orElse        

orElse :: SimpleParser e a -> SimpleParser e a -> SimpleParser e a
orElse (P f1) (P f2) = P $ \stream0 -> case f1 stream0 of
  (stream1, Left err) -> f2 stream1
  (stream1, Right a ) -> (stream1, Right a)               
