{-#LANGUAGE DeriveDataTypeable#-}

module Control.Enumerable.Count (
  Count(..),
  (!!*),
  (</>),
  module Control.Enumerable
  ) where

import Control.Enumerable
import Control.Sized
import Data.Monoid(Monoid(..))
import Data.List
import Data.Typeable(Typeable)

-- | Counts the number of values of a all sizes. Usage: @global :: Count [Bool]
newtype Count a = Count {count :: [Integer]} deriving (Typeable, Show)


-- Switch phantom type
untyped :: Count a -> Count b
untyped (Count x) = Count x

-- countparam :: Enumerable a => f a -> Count a
-- countparam _ = global

compact :: Count a -> Count a
compact = Count . reverse . dropWhile (==0) . reverse . count

-- | Counts the number of values of a given size, 0 if out of bounds.
(!!*) :: Count a -> Int -> Integer
(Count []) !!* n = 0
(Count (x:xs)) !!* n | n < 0  = 0
                        | n == 0 = x -- TODO: Check only once
                        | otherwise = Count xs !!* (n-1)

-- Undecidable for some 0-lists, for instance datatype with only infinite values
-- instance Eq (Count a) where
--  a == b = f a == f b
--    where f = count . compact

-- Typically infinite, perhaps it should have some hard-coded limit.
--instance Show (Count a) where
--  show = show . count


instance Functor Count where
  fmap _ (Count xs) = Count xs

instance Applicative Count where
  pure _ = Count [1]

  (Count [])  <*> (Count _)      = empty
  (Count _)  <*> (Count [])      = empty
  (Count (0:xs)) <*> ys             = pay $ Count xs <*> ys
  xs <*> (Count (0:ys))             = pay $ xs <*> Count ys
  (Count xs0@(_:xs0'))  <*> (Count ys)  = Count $ run (drop 1 $ reversals' ys) where
    mult = conv xs0
    run []     = []
    run (r:rs) = go r rs
    go r rs  = mult r :  case rs of
                           [] -> go' r xs0'
                           (r':rs') -> go r' rs'
    go' r []         = []
    go' r xs@(_:xs') = conv r xs : go' r xs'

instance Alternative Count where
  empty = Count []
  ~(Count xs) <|> ~(Count ys) = Count $ zipWithL (+) xs ys where
    zipWithL f (x:xs) (y:ys) = f x y : zipWithL f xs ys
    zipWithL _ [] ys = ys
    zipWithL _ xs [] = xs

instance Monoid (Count a) where
  mempty = empty
  mappend = (<|>)

instance Sized Count where
  pay    = Count . (0:) . count
  fin i  = Count [i]
  aconcat []  = empty
  aconcat [x] = x
  aconcat [x,y] = x <|> y
  aconcat xss = Count $ map sum $ transpose (map count xss)

{-
  finBits i | i <= 0  = Count []
  finBits i           = Count $ 1 : go 1 where
    go n | n <= lim      = n : go (2*n)
    go n | n >= i        = []
    go n                 = [i-n]
    lim = i `div` 2
-}

infixl 3 <->
(Count xs) <-> (Count ys) = Count $ zipWithLL op xs ys where
  op n m = max 0 (n-m)
  zipWithLL f xs []         = xs
  zipWithLL f [] _          = []
  zipWithLL f (x:xs) (y:ys) = f x y : zipWithLL f xs ys

infixl 4 </>
(</>) :: Count a -> Count a -> Count a
(Count xs)   </> (Count [])      = error "Vector division by zero"
(Count xs)   </> (Count (0:ys))  = Count (drop 1 xs) </> Count ys
(Count xs0)  </> (Count (y:ys0)) = Count ds where
  ds = go xs0 (reversals' ys0)
  go [] yrs          = []
  go (xs) []         = go' xs (tail ds)
  go (x:xs) (yr:yrs) =  ((x - conv yr ds) `div` y) : go xs yrs

  revy = reverse ys0
  go' [] _ = []
  go' (x:xs) (ds') = ((x - (conv revy ds' )) `div` y) : go' xs (tail ds')

-- Yap is the inverse of pay.
yap :: Count a -> Count a
yap (Count xs) = Count (drop 1 xs)


reversals' :: [a] -> [[a]]
reversals' = go [] where
  go rs xs = rs : case xs of
      [] -> []
      (x:xs) -> go (x:rs) xs


first k = Count . take k . (++ repeat 0) . count
rev x = Count $ reverse $ count x

-- Dissapointingly, this is the fastest version of conv I have discovered so far
conv :: [Integer] -> [Integer] -> Integer
conv xs = sum . zipWith (*) xs
{-#INLINE conv#-}

conv' :: [Integer] -> [Integer] -> Integer
conv' (_:xs) (0:ys) = conv' xs ys
conv' (0:xs) (_:ys) = conv' xs ys
conv' xs     ys = sum (zipWith (*) xs ys)
{-#INLINE conv'#-}


cardTake :: Int -> Count a -> Count a
cardTake k (Count xs) = Count (take k xs)

cardRev :: Count a -> Count a
cardRev (Count xs) = Count (reverse xs)
-------------------------------------
-- Specialized products and divisions
-------------------------------------

strict :: Count a -> Count a
strict (Count xs) = Count $ strictL xs

strictL :: [Integer] -> [Integer]
strictL xs = foldr seq xs xs

cap :: Int -> Count a -> Count a
cap k c = strict (cardTake (k+1) c)

-- Computes the k'th index in a product
mult :: Count a -> Count b -> Int -> Integer
-- mult (Count [1]) cl n = cl !!* n
-- mult cl (Count [1]) n = cl !!* n
-- mult (Count (0:xs)) cl n = mult (Count xs) cl (n-1)
mult cl (Count [1]) n = cl !!* n
mult cl1 cl2 n           = conv sub1 rev where
  sub2 = take (n+1) $ count cl2
  rl = length sub2
  rev = reverse sub2
  sub1 = drop (n+1-rl) (count cl1)


prodsK, prodsK' :: [Count a] -> Int -> Count a
prodsK []  _  = Count [1]
prodsK [x] k  = x  -- No length guarantee
prodsK xs k   = cardRev $ prodsKR xs k
-- Produces the reversed K-product of all given lists
-- By removing all pays we get a much smaller actual k for the products.
-- Intermediate lists need to be freed up for garbage collection.
prodsKR :: [Count a] -> Int -> Count a
--prodsKR []              k  = Count [1]
prodsKR (Count x:xs) k  = strict $
  Count $ foldl' prodR (reverse $ take (k+1) (x ++ repeat 0)) xs where

  (xs', p) = baseCosts 0 [] xs


  prodR :: [Integer] -> Count a -> [Integer]
--  prodR [] _                  = []
--  prodR r@(_,r') (Count x) = conv x r : prodR r x
  prodR rs (Count x) = map (conv x) (initTails rs)

prodsK' xs0 k = cap k $ go xs0 where
  go []     = Count [1]
  go [x]    = x
  go (x:xs) = untyped x <*> go xs


-- prop_ProdsK k cls = prodsK smallK cls ==
--  smallK = k `mod` 20

ultraDiv :: [Count ()] -> [Count ()] -> Int -> Count ()
ultraDiv xs ys k = let
  x = cardRev (prodsKR xs k)
  y = cardRev (prodsKR ys k)
  in cap k $ x </> y

initTails :: [a] -> [[a]]
initTails [] = []
initTails xs@(_:xs') = xs : initTails xs'

baseCosts acc1 acc2 []     = (acc1,acc2)
baseCosts acc1 acc2 (Count x:xs) = baseCosts (acc1 + length zs) (Count x' : acc2) xs where
    (zs, x') = break (/=0) x








{-
-- Testing
prop_multTerm a b (NonNegative n) = (count (a <*> b)) !!* n == mult a b n where
  [] !!* n = 0
  (x:xs) !!* 0 = x
  (x:xs) !!* n = xs !!* (n-1)

prop_multCom a b = (untyped a <*> b) == (untyped b <*> a)

prop_div :: Count (a -> a) -> Count a -> Property
prop_div a b = any (/= 0) (count b) ==> ((a <*> b) </> b) == untyped a

prop_div_id a = any (/= 0) (count a) ==> (a </> a) == pure ()
-}
