

{-|

This module provides the 'Sized' class. Instances of this class are typically collection data types for infinite sets of values with a finite number of values of any given size.

A simple example is "Control.Enumerable.Count" that just counts the number of values of each size. "Control.Enumerable.Values" provides all values of a given size.
<https://hackage.haskell.org/package/testing-feat FEAT> provides any value in the set much more efficiently.

-}
module Control.Sized (module Control.Applicative, Sized(..), kbits) where
import Control.Applicative

-- | A sized functor is an applicative functor extended with a notion of cost/size of contained values. This is useful for any type of bounded recursion over infinite sets, most notably for various kind of enumerations.
--
-- The intention is that every sized functor definition models a (usually) infinite set (technically a bag) with a finite number of values of any given size. As long as every cyclic (recursive) definition has at least one application of pay, this invariant is guaranteed.
--
-- The module "Control.Enumerable" provides sized functor definitions for a lot of data types, such that the size of a value is the number of constructor applications it contains. It also allows deriving these functors for any user defined data type (using Template Haskell).
class Alternative f => Sized f where
  -- | Increases the cost/size of all values in the given set.
  pay :: f a -> f a

  -- | Default: @pair a b = (,) <$> a <*> b@.
  pair :: f a -> f b -> f (a,b)
  pair a b = (,) <$> a <*> b

  -- | Default: @aconcat = foldr (\<|>) empty@
  aconcat :: [f a] -> f a
  aconcat []   = empty
  aconcat xs   = foldr1 (<|>) xs

  {- | Finite numeric types. @fin n@ contains all non-negative numbers below n. This definition is flat, all integers have the same size.
  Implementing this function efficiently will have a great impact on applications that use a lot of bounded numeric types (e.g. Int).

  Default: aconcat (map pure [0..n-1]) -}
  fin :: Integer -> f Integer
  fin n = aconcat (map pure [0..n-1])

  {- |Same as 'fin' but the size of values may differ.
  By default, the size of an integer is the number of significant bits in its binary representation. In other words, 0 has size zero, the values for size k>0 in @finBits n@ are in the interval
  @(2^(k-1),min (2^k-1) n)@. -}
  finSized :: Integer -> f Integer
  finSized = stdFinBits

  -- | Non-negative integers. By default, the size of an integer is the number of digits in its binary representation.
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
