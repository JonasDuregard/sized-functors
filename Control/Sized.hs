module Control.Sized (module Control.Applicative, Sized(..), kbits) where
import Control.Applicative

-- | Minimal complete definition: pay.
class Alternative f => Sized f where
  pay :: f a -> f a

  -- | Default: @pair a b = (,) <$> a <*> b@. 
  pair :: f a -> f b -> f (a,b)
  pair a b = (,) <$> a <*> b

  -- | Default: @aconcat = foldr (<|>) empty@
  aconcat :: [f a] -> f a
  aconcat []   = empty
  aconcat xs   = foldr1 (<|>) xs

  -- | 
  -- fins :: [Integer] -> f (Int, Integer)

  -- Finite numeric types. This definition is flat, all values have the same type. 
  fin :: Integer -> f Integer
  fin n = aconcat (map pure [0..n-1])

  {- |Same as 'fin' but the size of an integer is the number of digits in its binary representation. 
  In other words, 0 has size zero, the values for size k>0 in @finBits n@ are in the interval 
  @(2^(k-1),min (2^k-1) n)@. -}
  finBits :: Integer -> f Integer
  finBits = stdFinBits

  -- Non-negative integers. The size of an integer is the number of digits in its binary representation. 
  naturals :: f Integer
  naturals = stdNaturals



sizes :: Sized f => [f a] -> f a
sizes []     = empty
sizes (x:xs) = x <|> pay (sizes xs)




stdNaturals :: Sized f => f Integer
stdNaturals = pure 0 <|> go 0 where
  go n = pay $ ((2^n)+) <$> fin (2^n) <|> (go (n+1))

stdNaturals' :: Sized f => f Integer
stdNaturals' = pure 0 <|> go 1 where
  go n   = pay $ (n+) <$> fin n <|> go (2*n)


stdFinBits :: Sized f => Integer -> f Integer
stdFinBits i | i <= 0  = empty
stdFinBits i           = pure 0 <|> go 1 where
  go n | n <= lim   = pay $ (n+) <$> fin n <|> go (2*n)
  go n | n <= 0     = empty
  go n              = pay $ (n+) <$> fin (i-n)
  lim = i `div` 2

-- Non-negative integers of a given maximal number of bits. 
kbits :: Sized f => Int -> f Integer
kbits k = finBits (2^k)




