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

  -- Finite numeric types. @fin n@ contains all non-negative numbers below n. This definition is flat, all integers have the same size. 
  fin :: Integer -> f Integer
  fin n = aconcat (map pure [0..n-1])
  
  {- |Same as 'fin' but the size of values may differ. 
  By default, the size of an integer is the number of significant bits in its binary representation. In other words, 0 has size zero, the values for size k>0 in @finBits n@ are in the interval 
  @(2^(k-1),min (2^k-1) n)@. -}
  finSized :: Integer -> f Integer
  finSized = stdFinBits

  -- Non-negative integers. By default, the size of an integer is the number of digits in its binary representation. 
  naturals :: f Integer
  naturals = stdNaturals


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
  go n | n >= i     = empty
  go n              = pay $ (n+) <$> fin (i-n)
  lim = i `div` 2

-- Non-negative integers of a given maximal number of bits. 
kbits :: Sized f => Int -> f Integer
kbits k = finSized (2^k)




-- integers :: Sized f => (Int -> (Integer,Integer)) -> f Integer
-- integers = 

-- ints :: Sized f => Int -> (Int -> (Integer,Integer)) -> f Integer

