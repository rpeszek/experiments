{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Instances.REW where

import Control.Applicative 



-- conceptual prototype, could be useful, e.g. naturally transforms from to aeson like parsers

newtype RdrWarnErr r e w a = REW {runREW :: r -> Either e (w, a)} deriving Functor

instance (Monoid w) => Applicative (RdrWarnErr r e w) where
    pure x = REW . const $ Right (mempty, x)
    REW f <*> REW g = REW (\r -> 
              case (f r, g r) of 
                  (Left e, _) -> Left e
                  (Right (u, f'), Right (v, x)) -> Right (u <> v, f' x)
                  (Right (u, f'), Left e) -> Left e
            )
       -- where 
           -- x = f `asTypeOf` _ -- r -> Either e (w, a -> b)
           -- y = g `asTypeOf` _    --  r -> Either e (w, a)

instance (Monoid w) => Monad (RdrWarnErr r e w) where   
     (REW f) >>= k = REW (\r ->
           case f r of 
              Left e -> Left e
              Right (u, x) -> 
                    let REW h = k x
                    in case h r of 
                            Right (v, b) -> Right (u <> v, b)
                            Left e -> Left e
        )


instance (Monoid e) => Alternative (RdrWarnErr r e e) where 
    empty  = REW . const $ Left mempty
    REW f <|> REW g = REW (\r ->
             case (f r, g r) of 
                (Left e1, Left e2) -> Left $ e1 <> e2
                (Left e1, Right (w2, x)) -> Right (e1 <> w2, x)
                (l@(Right _), _) -> l
          )

