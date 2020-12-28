{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable        #-}
{-# LANGUAGE StandaloneDeriving       #-}

module Data.Aeson.RecScheme where 
    
import qualified Data.Aeson as A
import           Data.Functor.Foldable.TH
import qualified Data.Fix as Fx
import           Data.Functor.Foldable

makeBaseFunctor ''A.Value

-- generated type:
--
-- data ValueF r
--   = ObjectF !(HashMap Text r)
--   | ArrayF !(Vector r)
--   | StringF !Text
--   | NumberF !Scientific
--   | BoolF !Bool
--   | NullF 

deriving instance Show a => Show (ValueF a) 

asFix :: A.Value -> Fx.Fix ValueF
asFix = Fx.ana project


fromFix :: Fx.Fix ValueF -> A.Value
fromFix = Fx.cata embed

     