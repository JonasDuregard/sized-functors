{-# LANGUAGE GADTs, DeriveDataTypeable, DeriveFunctor #-}
module Control.Sized.Free where

import Control.Sized
import Data.Typeable
-- import Data.Hole
-- import Data.Tree

-- | The FreeSized Sized functor, with a sensible set of constructors
data FreeSized a where
  (:+:)     :: FreeSized a -> FreeSized a -> FreeSized a
  (:$:)     :: (a -> b) -> FreeSized a -> FreeSized b
  (:*:)     :: FreeSized a -> FreeSized b -> FreeSized (a,b)
  Pay       :: FreeSized a -> FreeSized a

--  (:<*>:)   :: FreeSized (a -> b) -> FreeSized a -> FreeSized b
--  Aconcat   :: [FreeSized a] -> FreeSized a
--  Kbits     :: Int -> FreeSized Integer
--  Naturals  :: FreeSized Integer
--  Fin       :: Integer -> FreeSized Integer
  Pure      :: a -> FreeSized a
  Empty     :: FreeSized a
--  AssocR    :: FreeSized ((a,b),c) -> FreeSized (a,(b,c))
  deriving Typeable
  
instance Functor FreeSized where
  fmap = (:$:)

instance Applicative FreeSized where
  pure     = Pure
  x <*> y  = fmap (\(f,a) -> f a) (pair x y)

instance Alternative FreeSized where
  empty = Empty
  (<|>) = (:+:)
  
instance Sized FreeSized where
  pay      = Pay
--  fin      = Fin
  pair     = (:*:)




