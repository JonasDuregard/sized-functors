module Control.Sized (module Control.Applicative, Sized(..)) where
import Control.Applicative

-- | Minimal complete definition: pay.
class Alternative f => Sized f where
  pay :: f a -> f a

  -- aconcat = foldr (<|>) empty 
  aconcat :: [f a] -> f a
  -- aconcat [x] = x
  aconcat xs       = foldr (<|>) empty xs

  pair :: f a -> f b -> f (a,b)
  pair a b = (,) <$> a <*> b

  -- Finite numeric types
  fin :: Integer -> f Integer
  fin n = aconcat (map pure [0..n-1])

  -- Non-negative integers
  naturals :: f Integer
  naturals = stdNaturals

  -- Non-negative integers of at most k significant bits. 
  kbits :: Int -> f Integer
  kbits = stdKbits


stdNaturals :: Sized f => f Integer
stdNaturals = pay $ pure 0 <|> go 0 where
  go n = pay $ ((2^n)+) <$> fin (2^n) <|> (go (n+1))


stdKbits :: Sized f => Int -> f Integer
stdKbits k = pay $ pure 0 <|> go 0 where
  go n | n < k  = pay $ ((2^n)+) <$> fin (2^n) <|> (go (n+1))
  go _          = empty 



class Enumerable a where
  enum :: Sized f => f a


