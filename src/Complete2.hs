{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Complete2 where

import Data.Proxy    
import Data.Typeable

data Exp s where
   Tuple :: Exp a -> Exp b -> Exp (a ++ b)
   Sum  :: Exp a -> Exp b -> Exp (a ++ b)
   Single :: a -> Exp (a ': '[]) 

type family (a :: [*]) ++ (b :: [*]) :: [*] where
   '[] ++ ys = ys
   (a ': xs) ++ ys = a ': (xs ++ ys)

data Val (t :: Exp s) where
    SingleV :: a -> Val (Single a)
    -- UnknownV :: Proxy a -> Val (Single a) -- could be used to replace proxies with Val in LeftV and RightV
    LeftV :: Val l -> Proxy r -> Val (Sum l r)
    RightV :: Proxy l -> Val r -> Val (Sum l r)     
    TupleV :: Val a -> Val b -> Val (Tuple  a b)    

tst = (TupleV (SingleV "test") (SingleV 2.0))
tst2 = LeftV (SingleV "hi") (Proxy :: Proxy ('Tuple ('Single [Char]) ('Single Double)))

-- ghci
-- *Complete2> :t tst
-- tst :: Val ('Tuple ('Single [Char]) ('Single Double))
-- *Complete2> :t tst2
-- tst2
--   :: Val
--       ('Sum ('Single [Char]) ('Tuple ('Single [Char]) ('Single Double)))

valPrxy :: forall expr . Val expr -> Proxy expr
valPrxy _ = (Proxy :: Proxy expr)

-- *Complete2> :t valPrxy tst
-- valPrxy tst :: Proxy ('Tuple ('Single [Char]) ('Single Double))
