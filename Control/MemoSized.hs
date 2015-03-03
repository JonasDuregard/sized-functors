{-#LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Control.MemoSized 
  ( Closed, open, Memo(..), datatype
  , Enumerable(..), access, c1, c2, c3, c4, c5, c6, c7
  , global, ioMemo, ioMemo', noMemo, debug -- , debug, IOMemo, NoMemo
  , module Control.Sized, Typeable
  ) where

import Data.List (sort)
import Control.Sized
import Data.Typeable
import System.IO.Unsafe
import System.IO
import Control.Monad.Fix
import Data.IORef
import Data.Dynamic
import qualified Data.Map as M

-- For instances
import Data.Word
import Data.Bits
import Data.Int


newtype Closed a = Closed {open :: a}

class (Typeable f, Sized f) => Memo f where
  close :: Typeable a => f a -> Closed (f a)

datatype :: (Memo f, Typeable a) => [f a] -> Closed (f a)
datatype = close . pay . aconcat

class Typeable a => Enumerable a where
  enumerate :: Memo f => Closed (f a)

access :: (Enumerable a, Memo f) => f a
access = open enumerate



c1 :: (Enumerable a, Memo f) => (a -> x) -> f x
c1 f = fmap f access

c2 :: (Enumerable a, Enumerable b, Memo f) => (a -> b -> x) -> f x
c2 f = c1 (uncurry f)

c3 :: (Enumerable a, Enumerable b, Enumerable c, Memo f) => (a -> b -> c -> x) -> f x
c3 f = c2 (uncurry f)

c4 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Memo f) => (a -> b -> c -> d -> x) -> f x
c4 f = c3 (uncurry f)

c5 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Memo f) => (a -> b -> c -> d -> e -> x) -> f x
c5 f = c4 (uncurry f)

c6 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable g, Memo f) => (a -> b -> c -> d -> e -> g -> x) -> f x
c6 f = c5 (uncurry f)

c7 :: (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable g, Enumerable h, Memo f) => (a -> b -> c -> d -> e -> g -> h -> x) -> f x
c7 f = c6 (uncurry f)

-- More than seven constructor components? Uncurry your constructor!



instance Enumerable () where
  enumerate = close (pay $ pure ())

instance (Enumerable a, Enumerable b) => Enumerable (a,b) where
  enumerate = close $ pair access access -- Pairs are free

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
  enumerate = close $ c1 nat <|> c1 (\(Nat n) -> -n-1)

instance Enumerable Word where enumerate = close word

instance Enumerable Word8 where enumerate = close word -- enumerate = close (fromInteger <$> fin 256) -- Flat definition

instance Enumerable Word16 where enumerate = close word

instance Enumerable Word32 where enumerate = close word

instance Enumerable Word64 where enumerate = close word

instance Enumerable Int where enumerate = close int

instance Enumerable Int8 where enumerate = close int

instance Enumerable Int16 where enumerate = close int

instance Enumerable Int32 where enumerate = close int

instance Enumerable Int64 where enumerate = close int

-- ASCII characters
instance Enumerable Char where
  enumerate = close $ (toEnum . fromIntegral) <$> kbits 7
-- Swap the printable characters to the start of the enumeration
-- swapCharacters :: Word8 -> Char

-- instance Enumerable Double

-- instance Enumerable Float

-- (Infinite a, Enumerable a) => Enumerable (Ratio a)



-- * modifiers:
-- Enumerable Printable
-- Enumerable Unicode
-- (Infinite a, Enumerable a) => Enumerable (NonZero a)
-- Infinite a => Enumerable (Nat a)
-- Enumerable a => Enumerable (NonEmpty a)
data Nat = Nat {nat :: Integer} deriving Typeable
instance Enumerable Nat where
  enumerate = close (Nat <$> naturals)



word :: (FiniteBits a, Integral a, Sized f) => f a 
word = let e = fromInteger <$> kbits (bitSize' e) in e

int :: (FiniteBits a, Integral a, Sized f) => f a 
int = let e = fromInteger <$> kbs <|> (\n -> fromInteger (-n-1)) <$> kbs
          kbs = kbits (bitSize' e - 1)
      in e

bitSize' :: FiniteBits a => f a -> Int
bitSize' f = hlp undefined f where
  hlp :: FiniteBits a => a -> f a -> Int
  hlp a _ = finiteBitSize a


function :: (Typeable a, Enumerable b, Memo f) => f (a -> b) -> Closed (f (a -> b))
function f = datatype [ c1 const, f]

instance (CoEnumerable a, Enumerable b) => Enumerable (a -> b) where
  enumerate = coEnumerate

class Typeable a => CoEnumerable a where
  coEnumerate :: (Enumerable b,Memo f) => Closed (f (a -> b))

instance CoEnumerable Bool where
  coEnumerate = function $ c2 $ \x y b -> if b then x else y 

instance CoEnumerable a => CoEnumerable [a] where
  coEnumerate = function $ c2 $ 
    \uf cf xs -> case xs of 
       []      -> uf
       (x:xs)  -> cf x xs











newtype NoMemo f a = NoMemo {noMemo :: f a} deriving Typeable

instance Sized f => Functor (NoMemo f) where fmap f = NoMemo . fmap f . noMemo 
instance Sized f => Applicative (NoMemo f) where
  pure = NoMemo . pure
  (NoMemo a) <*> (NoMemo b) = NoMemo (a <*> b)
instance Sized f => Alternative (NoMemo f) where
  empty = NoMemo empty
  (NoMemo a) <|> (NoMemo b) = NoMemo (a <|> b)
instance Sized f => Sized (NoMemo f) where
  pay = NoMemo . pay . noMemo 
  fin = NoMemo . fin
  aconcat = NoMemo . aconcat . map noMemo
  pair (NoMemo a) (NoMemo b) = NoMemo (pair a b)

instance (Typeable f, Sized f) => Memo (NoMemo f) where
  close = Closed

unMemoized :: (Typeable f, Sized f, Enumerable a) => f a
unMemoized = noMemo access

typeArg :: Typeable a => f a -> TypeRep
typeArg = typeOf . peel where
  peel :: f a -> a
  peel = undefined

newtype Debug f a = Debug {debugNoMemo :: NoMemo f a} deriving (Functor, Applicative, Alternative, Sized, Typeable)
instance (Sized f, Typeable f) => Memo (Debug f) where
  close x = Closed y where
    y = unsafePerformIO $ putStrLn ("accessing: " ++ (show $ typeArg x)) >> return x

debug :: Debug f a -> f a
debug = noMemo . debugNoMemo


type Ref = IORef DynMap
newtype IOMemo f a = IOMemo {unsafeIoMemo :: Ref -> f a}  deriving Typeable

instance Sized f => Functor (IOMemo f) where 
  fmap f a = IOMemo $ fmap f . unsafeIoMemo a
instance Sized f => Applicative (IOMemo f) where
  pure = IOMemo . const . pure
  (IOMemo a) <*> (IOMemo b) = IOMemo $ \r -> (a r <*> b r)
instance Sized f => Alternative (IOMemo f) where
  empty = IOMemo (const empty)
  (IOMemo a) <|> (IOMemo b) = IOMemo $ \r -> (a r <|> b r)
instance Sized f => Sized (IOMemo f) where
  pay x = IOMemo (pay . unsafeIoMemo x) 
  fin = IOMemo . const . fin
  aconcat xs = IOMemo $ \r -> aconcat (map (`unsafeIoMemo` r) xs)
  pair (IOMemo a) (IOMemo b) = IOMemo $ \r -> (pair (a r) (b r))

instance (Typeable f, Sized f) => Memo (IOMemo f) where
  close (IOMemo x) = Closed (IOMemo (mem x)) where
    mem :: (Typeable a, Typeable f) => (Ref -> f a) -> Ref -> f a
    mem x r = unsafePerformIO $ protect (x r) r


ioMemo' :: (Typeable f, Sized f, Enumerable a) => f a
ioMemo' = unsafePerformIO ioMemo

ioMemo :: (Typeable f, Sized f, Enumerable a) => IO (f a)
ioMemo = do
  ref <- newIORef dynEmpty
  return (unsafeIoMemo access ref)


global :: (Typeable f, Sized f, Enumerable a) => f a
global = unsafeIoMemo access globalRef

globalRef :: Ref
{-# NOINLINE globalRef #-}
globalRef = unsafePerformIO (newIORef dynEmpty)




protect :: Typeable a => a -> Ref -> IO a
protect x ref = do
  m <- readIORef ref
  case dynLookup m of
    Just y   ->  return y
    Nothing  ->  -- putStrLn ("accessing: " ++ (show $ typeOf x)) >> 
                 writeIORef ref (dynInsert x m) >> return x

-- |  A dynamic map with type safe
-- insertion and lookup.
newtype DynMap = 
  DynMap (M.Map TypeRep Dynamic) 
  deriving Show

dynEmpty :: DynMap
dynEmpty = DynMap M.empty  
  
dynInsert :: Typeable a => a -> DynMap -> DynMap
dynInsert a (DynMap m) = DynMap (M.insert (typeOf a) (toDyn a) m)

dynLookup :: Typeable a => DynMap -> Maybe a
dynLookup (DynMap m) = hlp fun undefined where 
    hlp :: Typeable a => 
      (TypeRep -> Maybe a) -> a -> Maybe a
    hlp f a = f (typeOf a)
    fun tr = M.lookup tr m >>= fromDynamic




