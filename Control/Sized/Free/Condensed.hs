{-# LANGUAGE GADTs #-}
module Control.Sized.Free.Condensed where

import Control.Sized

-- | The condensed Sized functor, with constructors for a minimal set of functions
data Complete a where
  Pure      :: a -> Complete a
--  Empty     :: Complete a
--  (:<|>:)   :: Complete a -> Complete a -> Complete a
  (:<*>:)   :: Complete (a -> b) -> Complete a -> Complete b
--  (:$:)     :: (a -> b) -> Complete a -> Complete b
--  (:*:)     :: Complete a -> Complete b -> Complete (a,b)
  Aconcat   :: [Complete a] -> Complete a
--  Kbits     :: Int -> Complete Integer
--  Naturals  :: Complete Integer
--  Fin       :: Integer -> Complete Integer
  Pay       :: Complete a -> Complete a
  
instance Functor Complete where
  fmap f = (pure f <*>)

instance Applicative Complete where
  pure = Pure
  (<*>) = (:<*>:)

instance Alternative Complete where
  empty      = Aconcat []
  x <|> y    = Aconcat [x,y]
  
instance Sized Complete where
  pay      = Pay

