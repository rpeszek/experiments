
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 
-- Experiments with possible alternatives to `Alternative`
-- goal is a principled design (with laws) which is Alternative like and
-- exposes error semantics
module Vlternative where

import Instances
import Instances.REW


-- |
-- Alternative with A twisted.
--
-- laws: 
-- toWarnings . failure e = pure e
-- toSuccess . pure e = Just <$> pure e
-- toSuccess . failure e = pure Nothing
--
-- x <-> y = y if   toSuccess x = pure Nothing   -- (1 gen zero)
-- x <-> y = x if   toSuccess x != pure Nothing  -- (2 gen zero)
--
-- u <-> (v <-> w)  =  (u <-> v) <-> w           -- (3)
--
-- toSuccess $ f <*> failure e = pure Nothing   -- (4 gen right zero)
-- (pure a) <-> x = pure a                      -- (7 left catch) 
--
-- toSuccess $ (a <-> b) <*> c = toSuccess $ (a <*> c) <-> (b <*> c)     -- (5) Left Distribution
-- toSuccess $ a <*> (b <|> c) = toSuccess $ (a <*> b) <|> (a <*> c)  -- (6) Right Distribution  -- (6) Right Distribution

class (Monoid e, Applicative (f e)) => Vlternative e f where
    failure     :: e -> f e a  -- name fail is taken, should terminate applicative, monad
    (<->)       :: f e a -> f e a -> f e a  -- ^ alternative like combinator with @|@ twisted
    toWarnings :: f e a -> f e e -- returns accumulated failures that were silenced by <->, converts to non-failing with no accumulation
    toSuccess  :: f e a -> f e (Maybe a) -- returns success if present, converts to non-failing with no accumulation
                                
     
warn :: forall e f a. Vlternative e f => e -> f e a -> f e a
warn er a = failure er <-> a 



-- newtype ErrWarn e w a = EW {runEW :: Either e (w, a)}

instance Monoid e => Vlternative e (ErrWarn e) where
    failure e = EW $ Left e

    EW (Left e1) <-> EW (Left e2) = EW (Left $ e1 <> e2)
    EW (Left e1) <-> EW (Right (w2, r)) = EW $ Right (e1 <> w2, r)
    l@(EW (Right _)) <-> _ = l

    toWarnings (EW (Right (e, r))) = EW $ Right (mempty, e)
    toWarnings (EW (Left e)) = EW $  Right (mempty, e)

    toSuccess (EW (Right (e, r))) = EW $ Right (mempty, Just r)
    toSuccess (EW (Left e)) = EW $ Right (mempty, Nothing)
    
instance (Monoid e) => Vlternative e (RdrWarnErr r e) where 
    failure e = REW . const $ Left e
    REW f <-> REW g = REW (\r ->
             case (f r, g r) of 
                (Left e1, Left e2) -> Left $ e1 <> e2
                (Left e1, Right (w2, x)) -> Right (e1 <> w2, x)
                (l@(Right _), _) -> l
          )
    toWarnings (REW f) = REW (\r -> 
            case f r of 
              Right (e, r) ->  Right (mempty, e)
              Left e ->  Right (mempty, e)
            )
    toSuccess (REW f) = REW (\r ->
            case f r of 
               Right (e, r) -> Right (mempty, Just r)
               Left e -> Right (mempty, Nothing)
         )
