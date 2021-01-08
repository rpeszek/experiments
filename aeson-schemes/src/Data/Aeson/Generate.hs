{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications        #-}

module Data.Aeson.Generate where

import qualified Data.Aeson as A
import           Data.Functor.Foldable
import qualified Data.Fix as Fx

import           Data.Aeson.RecScheme
import           Data.Maybe (fromMaybe)
import           Data.Semigroup
import           Control.Applicative

import qualified Data.Text as T


-- $setup
-- >>> import           Test.QuickCheck
-- >>> import           Test.QuickCheck.Instances.Text()
-- >>> import           Test.QuickCheck.Instances.Time()
-- >>> import           Test.QuickCheck.Instances.Scientific()
-- >>> import           Data.Time.Calendar
-- >>> import           Data.Time.Clock
-- >>> :set -XOverloadedStrings -XTypeApplications
-- >>> let prototype = A.object ["name" A..= ("LastName" :: String), "created_date" A..= ("TimeStamp"::String), "nested" A..= A.object ["boo_date" A..= ("Day" :: String), "num" A..= (1 :: Int)]]
-- >>> :{
-- exmplGenDay :: SimpleTxtGen Gen
-- exmplGenDay "Day" = Just (T.pack . show <$> arbitrary @Day)
-- exmplGenDay _  = Nothing
-- :}
--
-- >>> :{
-- exmplGenTs :: SimpleTxtGen Gen
-- exmplGenTs "TimeStamp" = Just (T.pack . show <$> arbitrary @UTCTime)
-- exmplGenTs _  = Nothing 
-- :}
--
-- >>> :{
-- exmplGenLastName :: SimpleTxtGen Gen
-- exmplGenLastName "LastName" = Just (pure "Smith")
-- exmplGenLastName _ = Nothing 
-- :}
--
-- >>> let myGenerators = genStr exmplGenDay <> genStr exmplGenTs <> genStr exmplGenLastName
-- >>> let incompleteGen = genStr exmplGenDay <> genStr exmplGenTs


type GenName = T.Text

type SimpleTxtGen m = GenName -> Maybe (m T.Text)
newtype Generators m a = Generators {unGenerators :: GenName -> Maybe (m a)} 

instance Semigroup (Generators m a) where
   Generators f <> Generators h = Generators (\x -> f x <|> h x)


genStr :: Functor m => SimpleTxtGen m -> Generators m A.Value
genStr fn = Generators (fmap (fmap A.String) . fn)




-- |
-- Usefull to generate values with effects like QuickCheck Gen monad.
--
-- Values match the shape of protototype JSON structure, String values in the prototype define named generators to use
-- 
-- Prototype String values specify name of the generator to employ
-- 
-- * If not found default generator is applied
-- * If prototype value is not a String it is kept as is (E.g. if prototype value is A.Null the generated value will be always A.Null)
--
-- >>> generate (cookiecutter defNull myGenerators prototype)  
-- Object (fromList [("nested",Object (fromList [("num",Number 1.0),("boo_date",String "...")])),("created_date",String "..."),("name",String "Smith")])  
-- >>> generate (cookiecutter defNull incompleteGen prototype)  
-- Object (fromList [("nested",Object (fromList [("num",Number 1.0),("boo_date",String "...")])),("created_date",String "..."),("name",Null)])  
-- >>> generate (cookiecutter defKeepValue incompleteGen prototype)  
-- Object (fromList [("nested",Object (fromList [("num",Number 1.0),("boo_date",String "...")])),("created_date",String "..."),("name",String "LastName")])  
cookiecutter ::  Monad m => 
   (GenName -> m A.Value)  -- ^ default generator
   -> Generators m A.Value -- ^ generators
   -> A.Value          -- ^ prototype
   -> m A.Value
cookiecutter defv gen prototype = Fx.cataM (genV defv gen) (asFix prototype)
   where
    genV :: Applicative m =>  (GenName -> m A.Value) ->  Generators m A.Value -> ValueF A.Value -> m A.Value
    genV defv g  (StringF x) = fromMaybe (defv x) (unGenerators g x)
    genV _ _ x = pure . embed $ x


-- |
-- Useful as first param of cookiecutter
-- keeps value picked from prototype value if generator is not found
defKeepValue :: Applicative m => GenName -> m A.Value
defKeepValue x = pure $ A.String x

-- |
-- Useful as first param of cookiecutter
-- keeps prototype value if generator is not found
defNull :: Applicative m => GenName -> m A.Value
defNull x = pure A.Null

-- |
-- Useful as first param of cookiecutter
-- fails if generator is not found
defFail :: MonadFail m => GenName -> m A.Value
defFail nm = fail . T.unpack $ "no generator for " <> nm

-- |
-- Useful as first param of cookiecutter
-- Unfortunatelly monads like QuickCheck Gen do not have MonadFail instance
hackFail ::  GenName -> m A.Value
hackFail nm = error . T.unpack $ "no generator for " <> nm

