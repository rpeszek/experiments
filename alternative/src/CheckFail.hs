module CheckFail where


-- | check if computation has failed.
-- 
-- assumption: 
-- fmap (const()) fa = pure () iff checkFailure fa = pure True
class Applicative f => CheckFail f where
   checkFailure :: f a -> Bool


   
