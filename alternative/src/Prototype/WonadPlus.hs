{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}

-- |
-- Experiments with possible alternatives to `MonadPlus`
module Prototype.WonadPlus where
   
import qualified Alternative.Instances.TraditionalParser as Trad
import qualified Alternative.Instances.WarnParser as Warn

import           Prototype.Vlternative () -- temp orphans 
import           Prototype.Recover

-- >>> :set -XOverloadedStrings

class (Monad (m e), Recover e m) => WonadPlus e m where
    wfail :: e -> m e a

    wplus :: m e a -> (e -> m e a) -> m e a
    wplus a f = do 
        er <- recover a
        case er of
            Left e -> f e
            Right _ -> a


-- | needed for @wsome@ and @wsome@
data SList e a = Last e | Snoc (SList e a) a deriving (Show, Eq, Functor)

stake :: Int -> SList e a -> (Maybe e, [a])
stake n (Last e) = (Just e, [])
stake 0 (Snoc lst a) = (Nothing, [])
stake n (Snoc lst a) = fmap (a :) (stake (n-1) lst) 

-- | @some@ and @many@ make sense only for some computations
class WonadPlus e m => WonadPlusStream e m where

    -- NOTE standard @some v = (:) <$> v <*> many v@ is problematic as we need to evaluate actual @v@, not @many@
    wsome :: m e a -> m e (SList e a)
    wsome v = do 
        er <- recover v 
        case er of
            Left e -> do
                 _ <- v -- consume v (this should fail so the next line is not needed)
                 wfail e
            Right _ -> do
                r <- v -- consume v
                Snoc <$> wmany v <*> (pure r)
    wmany :: m e a -> m e (SList e a)
    wmany v = wsome v `wplus` (pure . Last)


-- * instances


instance Monoid e => WonadPlus e (Trad.TraditionalParser s) where
     wfail = Trad.failParse

-- |
--
-- >>> Trad.runParser (wsome (Trad.string "sk")) "skskboo"
-- Right (Snoc (Snoc (Last "sk no parse") "sk") "sk")
instance Monoid e => WonadPlusStream e (Trad.TraditionalParser s) where 

instance Monoid e => WonadPlus e (Warn.WarnParser s e) where
     wfail = Warn.failParse

instance Monoid e => WonadPlusStream e (Warn.WarnParser s e) where 
