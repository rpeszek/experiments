{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module Complete where

import Data.Proxy    
import Data.Typeable

data Exp = 
      Tuple Exp Exp
    | Sum Exp Exp
    | Single 

type family (a :: [*]) ++ (b :: [*]) :: [*] where
   '[] ++ ys = ys
   (a ': xs) ++ ys = a ': (xs ++ ys)

data Val s (t :: Exp) where
    SingleV :: a -> Val (a ': '[]) Single
    UnknownV :: Proxy a -> Val (a ': '[]) Single  -- could be used to replace proxies with Val in LeftV and RightV
    LeftV :: Val x1 l -> Proxy x2 -> Proxy r -> Val (x1 ++ x2) (Sum l r)
    RightV :: Proxy x1 -> Proxy l -> Val x2 r -> Val (x1 ++ x2) (Sum l r)     
    TupleV :: Val x1 a -> Val x2 b -> Val (x1 ++ x2) (Tuple a b)     

tst = (TupleV (SingleV "test") (SingleV 2.0))
tst2 = LeftV (SingleV "hi") (Proxy :: Proxy [String, Float]) (Proxy :: Proxy (('Tuple 'Single 'Single)))

data Val2 (t :: Exp) where
    SingleV2 :: Typeable a => a -> Val2 Single
    LeftV2 :: Val2 l -> Proxy r -> Val2 (Sum l r)
    RightV2 :: Proxy l-> Val2 r -> Val2 (Sum l r)     
    TupleV2 :: Val2 a -> Val2 b -> Val2 (Tuple a b)         

test = (TupleV2 (SingleV2 "test") (SingleV2 (2.0 :: Double)))
test2 = LeftV2 (SingleV2 "hi") (Proxy :: Proxy ('Tuple 'Single 'Single))