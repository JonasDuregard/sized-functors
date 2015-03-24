module Control.Sized.Enumerable 
  ( Enumerable(..)
  -- * Class based construction
  , datatype, c0, c1, c2, c3, c4, c5, c6, c7
  -- * Access
  , global, local
  
  -- * Non-class construction
  , access, share, Shared, Shareable, Typeable, module Control.Sized
  )where
import Control.Sized
import Data.ClassSharing

-- For instances
import Data.Modifiers
import Data.Bits
import Data.Word
import Data.Int
import Data.Ratio

instance (Typeable f, Sized f) => Sized (Shareable f) where
  pay = Shareable . fmap pay . run

class Typeable a => Enumerable a where
  enumerate :: (Typeable f, Sized f) => Shared f a

datatype :: (Typeable a, Sized f, Typeable f) => [Shareable f a] -> Shared f a
datatype = share . pay . aconcat

access :: (Enumerable a, Sized f, Typeable f) => Shareable f a
access = unsafeAccess enumerate

local :: (Typeable f, Sized f, Enumerable a) => f a
local = run access (unsafeNewRef ())

{-#NOINLINE gref#-}
gref :: Ref
gref = unsafeNewRef ()

global :: (Typeable f, Sized f, Enumerable a) => f a
global = run access gref


c0 :: Sized f => a -> Shareable f a
c0 = pure

c1 :: (Enumerable a, Sized f, Typeable f) => (a -> x) -> Shareable f x
c1 f = fmap f access

c2 :: (Enumerable a, Enumerable b, Sized f, Typeable f) => (a -> b -> x) -> Shareable f x
c2 f = c1 (uncurry f)

c3 :: (Enumerable a, Enumerable b, Enumerable c, Sized f, Typeable f) => (a -> b -> c -> x) -> Shareable f x
c3 f = c2 (uncurry f)

c4 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Sized f, Typeable f) => (a -> b -> c -> d -> x) -> Shareable f x
c4 f = c3 (uncurry f)

c5 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Sized f, Typeable f) => (a -> b -> c -> d -> e -> x) -> Shareable f x
c5 f = c4 (uncurry f)

c6 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable g, Sized f, Typeable f) => (a -> b -> c -> d -> e -> g -> x) -> Shareable f x
c6 f = c5 (uncurry f)

c7 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable g, Enumerable h, Sized f, Typeable f) => (a -> b -> c -> d -> e -> g -> h -> x) -> Shareable f x
c7 f = c6 (uncurry f)

-- More than seven constructor components? Uncurry your constructor!



instance Enumerable () where
  enumerate = share (pay $ pure ())

instance (Enumerable a, Enumerable b) => Enumerable (a,b) where
  enumerate = share $ pair access access -- Pairs are free

instance Enumerable Bool where
  enumerate = datatype [pure False, pure True]

instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
  enumerate = datatype [c1 Left, c1 Right]

instance Enumerable a => Enumerable [a] where
  enumerate = datatype [pure [], c2 (:)]

instance Enumerable a => Enumerable (Maybe a) where
  enumerate = datatype [pure Nothing, c1 Just]

instance Enumerable Ordering where
  enumerate = datatype [pure LT, pure EQ, pure GT]

instance Enumerable Integer where
  enumerate = share $ c1 nat <|> c1 (\(Nat n) -> -n-1)

instance Enumerable Word where enumerate = share word

instance Enumerable Word8 where enumerate = share word -- enumerate = share (fromInteger <$> fin 256) -- Flat definition

instance Enumerable Word16 where enumerate = share word

instance Enumerable Word32 where enumerate = share word

instance Enumerable Word64 where enumerate = share word

instance Enumerable Int where enumerate = share int

instance Enumerable Int8 where enumerate = share int

instance Enumerable Int16 where enumerate = share int

instance Enumerable Int32 where enumerate = share int

instance Enumerable Int64 where enumerate = share int

-- | ASCII characters
instance Enumerable Char where
  enumerate = share $ (toEnum . fromIntegral) <$> kbits 7
   -- Swap the printable characters to the start of the enumeration
   -- swapCharacters :: Word8 -> Char

-- | Not a proper injection
instance Enumerable Float where
  enumerate = share $ c2 $ \b a -> encodeFloat a (fromIntegral (b :: Int8))

-- | Not a prober injection
instance Enumerable Double where
  enumerate = share $ encodeFloat <$> access <*> e where
    e = negate <$> enumerateBounded (-1) (-lo)  <|> enumerateBounded 0 hi
    (lo,hi) = floatRange (0 :: Double) 

class (Typeable a, Integral a) => Infinite a
instance Infinite Integer

instance Infinite a => Enumerable (Ratio a) where
  enumerate = share (c1 $ rat . nat)
  
-- | Bijection into the rationals
rat :: Integral a => Integer -> Ratio a
rat i | i < 0 = error "Index out of bounds"
rat i = go 1 1 i where
  go a b 0 = a % b
  go a b i = let (i',m) = i `divMod` 2 in if m == 1 then go (a+b) b i' else go a (a +b) i'



-- | A class of infinite precision integral types. 'Integer' is the principal 
-- class member.

-- From Data.Modifiers:


instance Enumerable Unicode where
  enumerate = datatype [fmap Unicode $ enumerateBounded 
    (fromEnum (minBound :: Char)) 
    (fromEnum (maxBound :: Char))]

instance Enumerable Printable where
  enumerate = share $ fmap Printable $ enumerateBounded 32 126

enumerateBounded :: (Sized f, Enum a) => Int -> Int -> f a
enumerateBounded lo hi = trans <$> finBits (toInteger (hi - lo)) where
  trans i = toEnum $ fromInteger (toInteger lo + i)



-- * modifiers:
-- Enumerable Printable
-- Enumerable Unicode
-- (Infinite a, Enumerable a) => Enumerable (NonZero a)
-- Infinite a => Enumerable (Nat a)
-- Enumerable a => Enumerable (NonEmpty a)

instance Infinite integer => Enumerable (Nat integer) where
  enumerate = share (Nat . fromInteger <$> naturals)

instance Enumerable a => Enumerable (NonEmpty a) where
  enumerate = datatype [c2 $ mkNonEmpty]


word :: (FiniteBits a, Integral a, Sized f) => f a 
word = let e = fromInteger <$> kbits (bitSize' e) in e

int :: (FiniteBits a, Integral a, Sized f) => f a 
int = let e = fromInteger <$> kbs <|> (\n -> fromInteger (-n-1)) <$> kbs
          kbs = kbits (bitSize' e - 1)
      in e


bitSize' :: FiniteBits a => f a -> Int
bitSize' f = hlp (error "Enumerable: This is not supposed to be inspected") f where
  hlp :: FiniteBits a => a -> f a -> Int
  hlp a _ = finiteBitSize a


function :: (Typeable a, Enumerable b, Sized f, Typeable f) => Shareable f (a -> b) -> Shared f (a -> b)
function f = datatype [ c1 const, f]

instance (CoEnumerable a, Enumerable b) => Enumerable (a -> b) where
  enumerate = coEnumerate

class Typeable a => CoEnumerable a where
  coEnumerate :: (Enumerable b,Sized f, Typeable f) => Shared f (a -> b)

instance CoEnumerable Bool where
  coEnumerate = function $ c2 $ \x y b -> if b then x else y 

instance CoEnumerable a => CoEnumerable [a] where
  coEnumerate = function $ c2 $ 
    \uf cf xs -> case xs of 
       []      -> uf
       (x:xs)  -> cf x xs

