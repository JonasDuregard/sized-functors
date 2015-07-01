{-#LANGUAGE DeriveDataTypeable#-}
module Control.Enumerable.Values
  ( values
  , values'
  , Values (..)
  )  where

import Control.Enumerable

-- | Constructs all values of a given size. 
values :: Enumerable a => Int -> [a]
values = runValues global

-- | Constructs all values up to a given size.
values' :: Enumerable a => Int -> [[a]]
values' i = let f = runValues global in [f x|x <- [0..i]]

newtype Values a = Values {runValues :: Int -> [a]} deriving Typeable

instance Functor Values where
  fmap f = Values . fmap (fmap f) . runValues

instance Applicative Values where
  pure x     = Values $ \i -> if i == 0 then [x] else []
  fs <*> xs  = fmap (uncurry ($)) (pair fs xs)
  
instance Alternative Values where
  empty     = Values $ \_ -> []
  xs <|> ys = Values $ \i -> runValues xs i ++ runValues ys i

instance Sized Values where
  pay xs       = Values $ \i -> if i > 0 then runValues xs (i-1) else []
  pair xs ys   = Values $ \i -> [(x,y)|n <- [0..i], x <- runValues xs n, y <- runValues ys (i-n)]
  
  fin n        = Values $ \i -> if i == 0 then [0..n-1] else []
  aconcat []   = empty
  aconcat [x]  = x
  aconcat xss  = Values $ \i -> concatMap (($ i) . runValues) xss






-- -- | All values are memoised. Warning: Using this may be faster but potentially uses a lot of memory. 
-- data Memoised a = Memoised {unMemoised :: [[a]]}
