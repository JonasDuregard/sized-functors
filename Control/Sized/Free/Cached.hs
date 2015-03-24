{-# LANGUAGE GADTs, DeriveDataTypeable, DeriveFunctor #-}
module Control.Sized.Free.Cached where

import Control.Sized
import Data.Typeable
-- import Data.Hole
-- import Data.Tree

-- | The Cached Sized functor, with a sensible set of constructors
data Cached f a where
  (:+:)     :: Cached a -> Cached f a -> Cached f a
  (:$:)     :: (a -> b) -> Cached f a -> Cached f b
  (:*:)     :: Cached f a -> Cached f b -> Cached f (a,b)
  Pay       :: Cached f a -> (f a) -> Cached f a

--  (:<*>:)   :: Cached (a -> b) -> Cached a -> Cached b
--  Aconcat   :: [Cached a] -> Cached a
--  Kbits     :: Int -> Cached Integer
--  Naturals  :: Cached Integer
--  Fin       :: Integer -> Cached Integer
  Pure      :: a -> Cached f a
  Empty     :: Cached f a
--  AssocR    :: Cached ((a,b),c) -> Cached (a,(b,c))
  deriving Typeable
  
instance Functor (Cached f) where
  fmap = (:$:)

instance Applicative (Cached f) where
  pure     = Pure
  x <*> y  = fmap (\(f,a) -> f a) (pair x y)

instance Alternative (Cached f) where
  empty = Empty
  (<|>) = (:+:)
  
instance Sized (Cached f) where
  pay a    = Pay a (pay (cached a)) 
--  fin      = Fin
  pair     = (:*:)

cached :: Sized f => Cached f a -> f a
cached (Pay _ c) = c
cached (a :+: b) = cached a <|> cached b
cached (a :*: b) = pair (cached a) (cached b) 
cached (f :$: a) = f <$> cached a
cached (Pure x)  = pure x
cached Empty     = empty

