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

-- $setup
-- >>> :set -XOverloadedStrings

class Monad (m e) => WonadPlus e m where
    wfail :: e -> m e a

    wplus :: m e a -> (e -> m e a) -> m e a


recoverWplus :: (Monad (m e), Recover e m) => m e a -> (e -> m e a) -> m e a
recoverWplus a f = do 
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
class (WonadPlus e m, Recover e m) => WonadPlusStream e m where
    wsome :: m e a -> m e (SList e a)

    wmany :: m e a -> m e (SList e a)
    wmany v = wsome v `wplus` (pure . Last)

-- | 
-- NOTE standard implementation @some v = Snoc <$> many <*> v@ is non-termination prone as we need to evaluate actual @v@, not @many@
recoverWsome :: (WonadPlusStream e m, Recover e m) => m e a -> m e (SList e a)
recoverWsome v = do 
        er <- recover v 
        case er of
            Left e -> do
                 _ <- v -- consume v (this should fail so the next line is not needed)
                 wfail e
            Right _ -> do
                r <- v -- consume v
                Snoc <$> wmany v <*> (pure r)


-- * instances


instance Monoid e => WonadPlus e (Trad.TraditionalParser s) where
     wfail = Trad.failParse
     wplus = recoverWplus

-- |
--
-- >>> Trad.runParser (wsome (Trad.string "sk")) "skskboo"
-- Right (Snoc (Snoc (Last "sk no parse") "sk") "sk")
instance Monoid e => WonadPlusStream e (Trad.TraditionalParser s) where 
    wsome = recoverWsome

instance Monoid e => WonadPlus e (Warn.WarnParser s e) where
    wfail = Warn.failParse
    wplus = recoverWplus

instance Monoid e => WonadPlusStream e (Warn.WarnParser s e) where 
    wsome = recoverWsome
