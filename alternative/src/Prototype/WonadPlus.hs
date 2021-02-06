{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Experiments with possible alternatives to `MonadPlus`
module Prototype.WonadPlus where
   
import qualified Alternative.Instances.TraditionalParser as Trad
import qualified Alternative.Instances.WarnParser as Warn
import           Prototype.Vlternative


class (Monad (m e), Recover e m) => WonadPlus e m where
    wfail :: e -> m e a

    wplus :: m e a -> (e -> m e a) -> m e a
    wplus a f = do 
        er <- recover a
        case er of
            Left e -> f e
            Right _ -> a


-- * instances

instance Monoid e => WonadPlus e (Trad.TraditionalParser s) where
     wfail = Trad.failParse

instance Monoid e => WonadPlus e (Warn.WarnParser s e) where
     wfail = Warn.failParse