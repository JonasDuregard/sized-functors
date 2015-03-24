{-# LANGUAGE GADTs #-}
module Control.Sized.Free.Complete where

import Control.Sized

-- | The complete Sized functor, with constructors for all supported functions
data Complete a where
  Pure      :: a -> Complete a
  Empty     :: Complete a
  (:<|>:)   :: Complete a -> Complete a -> Complete a
  (:<*>:)   :: Complete (a -> b) -> Complete a -> Complete b
  (:$:)     :: (a -> b) -> Complete a -> Complete b
  (:*:)     :: Complete a -> Complete b -> Complete (a,b)
  Aconcat   :: [Complete a] -> Complete a
  FinBits   :: Integer -> Complete Integer
  Naturals  :: Complete Integer
  Fin       :: Integer -> Complete Integer
  Pay       :: Complete a -> Complete a
  
instance Functor Complete where
  fmap = (:$:)

instance Applicative Complete where
  pure = Pure
  (<*>) = (:<*>:)

instance Alternative Complete where
  empty = Empty
  (<|>) = (:<|>:)
  
instance Sized Complete where
  pay      = Pay
  naturals = Naturals
  finBits  = FinBits
  aconcat  = Aconcat
  pair     = (:*:)
  fin      = Fin

