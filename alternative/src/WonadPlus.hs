{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Experiments with possible alternatives to `MonadPlus`
module WonadPlus where
   
import Alternative.Instances.TraditionalParser


class Monad m => WonadPlus e m where
    wfail :: e -> m a

    wtry :: m a -> m (Either e a)  -- should be non-destructive, not consume parser input, etc.

    wplus :: m a -> (e -> m a) -> m a
    wplus a f = do 
        er <- wtry a
        case er of
            Left e -> f e
            Right a -> pure a 


-- * instances

instance WonadPlus e (TraditionalParser s e) where
     wfail e = P (\s -> (s, Left e))
     wtry p = tryLookAhead p


