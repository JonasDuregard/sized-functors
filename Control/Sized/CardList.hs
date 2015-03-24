{-#LANGUAGE DeriveDataTypeable#-}

module Control.Sized.CardList where

import Control.Sized
import Data.Monoid(Monoid(..))
import Data.List
import Data.Typeable(Typeable)

-- The type is just a phantom
newtype CardList a = CardList {cardList :: [Integer]} deriving Typeable
  -- deriving (Arbitrary)

-- Switch phantom type
untyped :: CardList a -> CardList b
untyped (CardList x) = CardList x

compact :: CardList a -> CardList a
compact = CardList . reverse . dropWhile (==0) . reverse . cardList

(!!*) :: CardList a -> Int -> Integer
(CardList []) !!* n = 0
(CardList (x:xs)) !!* n | n < 0  = 0
                        | n == 0 = x -- TODO: Check only once
                        | otherwise = CardList xs !!* (n-1)

-- Undecidable for some 0-lists, for instance datatype with only infinite values
instance Eq (CardList a) where 
  a == b = f a == f b 
    where f = cardList . compact

-- Typically infinite, perhaps it should have some hard-coded limit.
instance Show (CardList a) where
  show = show . cardList


instance Functor CardList where
  fmap _ (CardList xs) = CardList xs

instance Applicative CardList where 
  pure _ = CardList [1]

  (CardList [])  <*> (CardList _)      = empty
  (CardList _)  <*> (CardList [])      = empty
  (CardList (0:xs)) <*> ys             = pay $ CardList xs <*> ys
  xs <*> (CardList (0:ys))             = pay $ xs <*> CardList ys
  (CardList xs0@(_:xs0'))  <*> (CardList ys)  = CardList $ run (drop 1 $ reversals' ys) where
    mult = conv xs0
    run []     = []
    run (r:rs) = go r rs
    go r rs  = mult r :  case rs of 
                           [] -> go' r xs0'
                           (r':rs') -> go r' rs'
    go' r []         = []
    go' r xs@(_:xs') = conv r xs : go' r xs'

instance Alternative CardList where 
  empty = CardList []
  ~(CardList xs) <|> ~(CardList ys) = CardList $ zipWithL (+) xs ys where
    zipWithL f (x:xs) (y:ys) = f x y : zipWithL f xs ys
    zipWithL _ [] ys = ys
    zipWithL _ xs [] = xs

instance Monoid (CardList a) where 
  mempty = empty
  mappend = (<|>)

instance Sized CardList where
  pay    = CardList . (0:) . cardList
  fin i  = CardList [i]
  aconcat []  = empty
  aconcat [x] = x
  aconcat [x,y] = x <|> y
  aconcat xss = CardList $ map sum $ transpose (map cardList xss)


  
infixl 3 <-> 
(CardList xs) <-> (CardList ys) = CardList $ zipWithLL op xs ys where
  op n m = max 0 (n-m)
  zipWithLL f xs []         = xs
  zipWithLL f [] _          = []
  zipWithLL f (x:xs) (y:ys) = f x y : zipWithLL f xs ys

infixl 4 </>
(</>) :: CardList a -> CardList a -> CardList a
(CardList xs)   </> (CardList [])      = error "Vector division by zero"
(CardList xs)   </> (CardList (0:ys))  = CardList (drop 1 xs) </> CardList ys
(CardList xs0)  </> (CardList (y:ys0)) = CardList ds where
  ds = go xs0 (reversals' ys0)
  go [] yrs          = []
  go (xs) []         = go' xs (tail ds)
  go (x:xs) (yr:yrs) =  ((x - conv yr ds) `div` y) : go xs yrs

  revy = reverse ys0
  go' [] _ = []
  go' (x:xs) (ds') = ((x - (conv revy ds' )) `div` y) : go' xs (tail ds')
  
-- Yap is the inverse of pay.
yap :: CardList a -> CardList a
yap (CardList xs) = CardList (drop 1 xs)


reversals' :: [a] -> [[a]]
reversals' = go [] where
  go rs xs = rs : case xs of 
      [] -> []
      (x:xs) -> go (x:rs) xs


first k = CardList . take k . (++ repeat 0) . cardList
rev x = CardList $ reverse $ cardList x

-- Dissapointingly, this is the fastest version of conv I have discovered so far
conv :: [Integer] -> [Integer] -> Integer
conv xs = sum . zipWith (*) xs  
{-#INLINE conv#-}

conv' :: [Integer] -> [Integer] -> Integer
conv' (_:xs) (0:ys) = conv' xs ys
conv' (0:xs) (_:ys) = conv' xs ys
conv' xs     ys = sum (zipWith (*) xs ys) 
{-#INLINE conv'#-}


cardTake :: Int -> CardList a -> CardList a
cardTake k (CardList xs) = CardList (take k xs)

cardRev :: CardList a -> CardList a
cardRev (CardList xs) = CardList (reverse xs)
-------------------------------------
-- Specialized products and divisions
-------------------------------------

strict :: CardList a -> CardList a
strict (CardList xs) = CardList $ strictL xs

strictL :: [Integer] -> [Integer]
strictL xs = foldr seq xs xs

cap :: Int -> CardList a -> CardList a 
cap k c = strict (cardTake (k+1) c)

-- Computes the k'th index in a product
mult :: CardList a -> CardList b -> Int -> Integer
-- mult (CardList [1]) cl n = cl !!* n
-- mult cl (CardList [1]) n = cl !!* n
-- mult (CardList (0:xs)) cl n = mult (CardList xs) cl (n-1)
mult cl (CardList [1]) n = cl !!* n
mult cl1 cl2 n           = conv sub1 rev where
  sub2 = take (n+1) $ cardList cl2
  rl = length sub2
  rev = reverse sub2
  sub1 = drop (n+1-rl) (cardList cl1)

  
prodsK, prodsK' :: [CardList a] -> Int -> CardList a
prodsK []  _  = CardList [1]
prodsK [x] k  = x  -- No length guarantee
prodsK xs k   = cardRev $ prodsKR xs k
-- Produces the reversed K-product of all given lists 
-- By removing all pays we get a much smaller actual k for the products.
-- Intermediate lists need to be freed up for garbage collection.
prodsKR :: [CardList a] -> Int -> CardList a
--prodsKR []              k  = CardList [1]
prodsKR (CardList x:xs) k  = strict $ 
  CardList $ foldl' prodR (reverse $ take (k+1) (x ++ repeat 0)) xs where
  
  (xs', p) = baseCosts 0 [] xs


  prodR :: [Integer] -> CardList a -> [Integer]
--  prodR [] _                  = []
--  prodR r@(_,r') (CardList x) = conv x r : prodR r x
  prodR rs (CardList x) = map (conv x) (initTails rs)

prodsK' xs0 k = cap k $ go xs0 where
  go []     = CardList [1]
  go [x]    = x
  go (x:xs) = untyped x <*> go xs 
  
  
-- prop_ProdsK k cls = prodsK smallK cls == 
--  smallK = k `mod` 20

ultraDiv :: [CardList ()] -> [CardList ()] -> Int -> CardList ()
ultraDiv xs ys k = let
  x = cardRev (prodsKR xs k)
  y = cardRev (prodsKR ys k)
  in cap k $ x </> y

initTails :: [a] -> [[a]]
initTails [] = []
initTails xs@(_:xs') = xs : initTails xs'

baseCosts acc1 acc2 []     = (acc1,acc2)
baseCosts acc1 acc2 (CardList x:xs) = baseCosts (acc1 + length zs) (CardList x' : acc2) xs where
    (zs, x') = break (/=0) x








{-
-- Testing
prop_multTerm a b (NonNegative n) = (cardList (a <*> b)) !!* n == mult a b n where
  [] !!* n = 0
  (x:xs) !!* 0 = x
  (x:xs) !!* n = xs !!* (n-1)
    
prop_multCom a b = (untyped a <*> b) == (untyped b <*> a)
  
prop_div :: CardList (a -> a) -> CardList a -> Property
prop_div a b = any (/= 0) (cardList b) ==> ((a <*> b) </> b) == untyped a

prop_div_id a = any (/= 0) (cardList a) ==> (a </> a) == pure ()
-}





