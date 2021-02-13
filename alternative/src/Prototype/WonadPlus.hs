{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Conceptual experiment with alternative to `MonadPlus`
module Prototype.WonadPlus where
   
import           Prototype.Common
import           Prototype.Recover

import qualified Alternative.Instances.TraditionalParser as Trad
import qualified Alternative.Instances.WarnParser as Warn
import           Alternative.Instances.ErrWarnT
import           Alternative.Instances.ErrWarn



-- $setup
-- >>> :set -XOverloadedStrings

class Monad (m e) => WonadPlus e m where
    wfail :: e -> m e a

    wplus :: m e a -> (e -> m e a) -> m e a

    wplus' :: m e a -> m e a -> m e a
    wplus' a b = wplus a (const b)


-- In case of parsers first param `a` will be backtracked when it fails (@a@ runs in @recover@)
-- this matches e.g. trifecta's backtracking behavior
recoverWplus :: forall w e m a. (Monad (m e), Recover e w (m e)) => m e a -> (e -> m e a) -> m e a
recoverWplus a f = do 
        er <- recover @e @w a
        case er of
            Left e -> do 
                f e
            Right _ -> a




-- | 
-- @some@ and @many@ make sense only for some computations
-- are kept in this (separate) typeclass
class (WonadPlus e m) => WonadPlusStream e m where
    wsome :: m e a -> m e (SList e a)

    wmany :: m e a -> m e (SList e a)
    wmany v = wsome v `wplus` (pure . Last)

-- | 
-- NOTE standard implementation @some v = Snoc <$> many <*> v@ is non-termination prone as we need to evaluate actual @v@, not @many@
recoverWsome :: forall w e m a. (WonadPlusStream e m, Recover e w (m e)) => m e a -> m e (SList e a)
recoverWsome v = do 
        er <- recover @e @w v 
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
     wplus = recoverWplus @() 

-- |
--
-- >>> Trad.runParser (wsome (Trad.string "sk")) "skskboo"
-- Right (Snoc (Snoc (Last "sk no parse") "sk") "sk")
instance Monoid e => WonadPlusStream e (Trad.TraditionalParser s) where 
    wsome = recoverWsome @()

instance Monoid e => WonadPlus e (Warn.WarnParser s e) where
    wfail = Warn.failParse
    wplus = recoverWplus @e

instance Monoid e => WonadPlusStream e (Warn.WarnParser s e) where 
    wsome = recoverWsome @e

instance (Monad m, Monoid e) => WonadPlus e (Reord ErrWarnT m e) where
    wfail = Reord . err
    wplus = recoverWplus @ e

instance (Monad m, Monoid e) =>  WonadPlusStream e (Reord ErrWarnT m e) where 
    wsome = recoverWsome @ e


instance Monoid e => WonadPlus e (ErrWarn e) where
    wfail e = EW $ Left e
    wplus = recoverWplus @ e

instance Monoid e => WonadPlusStream e (ErrWarn e) where 
    wsome = recoverWsome @ e