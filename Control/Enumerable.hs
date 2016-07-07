{-#LANGUAGE TemplateHaskell#-}

{-| 
This module provides the 'Enumerable' class, which has a simple purpose: Provide any enumeration for any instance type. The prerequisite is that the enumeration data type is a sized functor (see "Control.Sized") with the enumerated type as the type parameter. The general idea is that the size of a value is the number of constructor applications it contains. 

Because Sized functors often rely of memoization, sharing is important. Since class dictionaries are not always shared, a mechanism is added that guarantees optimal sharing (it never creates two separate instance members for the same type). This is why the type of 'enumerate' is @Shared f a@ instead of simply @f a@. The technicalities of this memoization are not important, but it means there are two modes for accessing an enumeration: 'local' and 'global'. The former means sharing is guaranteed within this value, but subsequent calls to local may recreate dictionaries. The latter guarantees optimal sharing even between calls. It also means the enumeration will never be garbage collected, so use with care in programs that run for extended periods of time and contains many (especially non-regular) types. 

Once a type has an instance, it can be enumerated in several ways (by instantiating 'global' to different types). For instance @global :: Count [Maybe Bool]@ would only count the number of lists of Maybe Bool of each size (using "Control.Enumerable.Count"). @global :: Values [Maybe Bool] would give the actual values for all sizes as lists. See <https://hackage.haskell.org/package/testing-feat FEAT> for a more elaborate enumeration type that allows access to any value in the enumeration (given an index) in polynomial time, uniform selection from a given size etc. 

Instances can be constructed in three ways:

1: Manually by passing 'datatype' a list where each element is an application of the constructor functions 'c0', 'c1' etc, so a data type like Maybe would have @enumerate = datatype [c0 Nothing, c1 Just]@. This assumes all field types of all constructors are enumerable (recursive constructors work fine). The functions passed to @cX@ do not have to be constructors, but should be injective functions (if they are not injective the enumeration will contain duplicates). So "smart constructors" can be used, for instance the @Rational@ datatype is defined by an injection from the natural numbers. 

2: Automatically with Template Haskell ('deriveEnumerable'). A top level declaration like @deriveEnumerable ''Maybe@ would derive an instance for the @Maybe@ data type. 

3: Manually using the operations of a sized functor (see "Control.Sized") to build a @Shareable f a@ value, then apply 'share' to it. To use other instances of 'Enumerable' use 'access'. 

-}
module Control.Enumerable 
  ( Enumerable(..)
  -- * Class based construction
  , datatype, c0, c1, c2, c3, c4, c5, c6, c7
  -- * Access
  , global, local
  
  -- * Automatic derivation
  , deriveEnumerable
  , dAll, dExcluding, dExcept, ConstructorDeriv, deriveEnumerable'
  
  -- * Non-class construction
  , access, share, Shared, Shareable, Typeable, module Control.Sized
  
  -- * Enumerating functions
  , function, CoEnumerable(..)
  
  -- * Other stuff (required for instances)
  , Infinite
  )where
import Control.Sized
import Data.ClassSharing

-- For instances
import Data.Modifiers
import Data.Bits
import Data.Word
import Data.Int
import Data.Ratio
import Control.Enumerable.Derive hiding (global)

instance (Typeable f, Sized f) => Sized (Shareable f) where
  pay       = Shareable . fmap pay . run
  fin       = Shareable . const . fin
  pair x y  = Shareable $ \r -> pair (run x r) (run y r)

class Typeable a => Enumerable a where
  enumerate :: (Typeable f, Sized f) => Shared f a


-- | Used instead of enumerate when manually building instances.
access :: (Enumerable a, Sized f, Typeable f) => Shareable f a
access = unsafeAccess enumerate

-- | Guarantees local sharing. All enumerations are shared inside each invokation of local, but may not be shared between them. 
{-#INLINE local#-}
local :: (Typeable f, Sized f, Enumerable a) => f a
local = run access (unsafeNewRef ())

{-#NOINLINE gref#-}
gref :: Ref
gref = unsafeNewRef ()

-- | This is the primary way to access enumerations for usage. Guarantees global sharing of enumerations of the same type. Note that this means the enumerations are never garbage collected. 
global :: (Typeable f, Sized f, Enumerable a) => f a
global = run access gref

-- | Builds an enumeration of a data type from a list of constructors (see c0-c7)
datatype :: (Typeable a, Sized f, Typeable f) => [Shareable f a] -> Shared f a
datatype = share . pay . aconcat


-- | Takes a constructor with arity 0 (a pure value)
c0 :: Sized f => a -> Shareable f a
c0 = pure

-- | Takes a constructor of arity 1
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


-- | The unit constructor is free
instance Enumerable () where
  enumerate = share (pure ())

-- All tuple constructors are free
instance (Enumerable a, Enumerable b) => Enumerable (a,b) where
  enumerate = share $ pair access access -- Pairs are free

instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a,b,c) where
  enumerate = share $ c1 $ \(a,(b,c)) -> (a,b,c)

instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d) 
             => Enumerable (a,b,c,d) where
  enumerate = share $ c1 $ \(a,(b,(c,d))) -> (a,b,c,d)

instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e) 
             => Enumerable (a,b,c,d,e) where
  enumerate = share $ c1 $ \(a,(b,(c,(d,e)))) -> (a,b,c,d,e)


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

-- | Not a proper injection
instance Enumerable Double where
  enumerate = share $ encodeFloat <$> access <*> e where
    e = negate <$> enumerateBounded (-1) (-lo)  <|> enumerateBounded 0 hi
    (lo,hi) = floatRange (0 :: Double) 


-- | A class of infinite precision integral types. 'Integer' is the principal 
-- class member.
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


-- From Data.Modifiers:
instance Enumerable Unicode where
  enumerate = datatype [fmap Unicode $ enumerateBounded 
    (fromEnum (minBound :: Char)) 
    (fromEnum (maxBound :: Char))]

instance Enumerable Printable where
  enumerate = share $ fmap Printable $ enumerateBounded 32 126

enumerateBounded :: (Sized f, Enum a) => Int -> Int -> f a
enumerateBounded lo hi = trans <$> finSized (toInteger (hi - lo)) where
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
          kbs = finSized (2^(bitSize' e - 1))
      in e


bitSize' :: FiniteBits a => f a -> Int
bitSize' f = hlp (error "Enumerable: This is not supposed to be inspected") f where
  hlp :: FiniteBits a => a -> f a -> Int
  hlp a _ = finiteBitSize a


-- | Work in progress
class Typeable a => CoEnumerable a where
  coEnumerate :: (Enumerable b,Sized f, Typeable f) => Shared f (a -> b)

-- | Builds a suitable definition for @coEnumerate@ given an pattern matching function for a data type (see source for examples). 
function :: (Typeable a, Enumerable b, Sized f, Typeable f) => Shareable f (a -> b) -> Shared f (a -> b)
function f = datatype [ c1 const, f]


instance (CoEnumerable a, Enumerable b) => Enumerable (a -> b) where
  enumerate = coEnumerate

instance CoEnumerable Bool where
  coEnumerate = function $ c2 $ \x y b -> if b then x else y 

instance CoEnumerable a => CoEnumerable [a] where
  coEnumerate = function $ c2 $ 
    \uf cf xs -> case xs of 
       []      -> uf
       (x:xs)  -> cf x xs






deriveEnumerable :: Name -> Q [Dec]
deriveEnumerable = deriveEnumerable' . dAll


type ConstructorDeriv = (Name, [(Name, ExpQ)])
dAll :: Name -> ConstructorDeriv
dAll n = (n,[])
dExcluding :: Name -> ConstructorDeriv -> ConstructorDeriv
dExcluding n (t,nrs) = (t,(n,[|empty|]):nrs)
dExcept :: Name -> ExpQ -> ConstructorDeriv -> ConstructorDeriv
dExcept n e (t,nrs) = (t,(n,e):nrs)

-- | Derive an instance of Enumberable with Template Haskell, with 
-- rules for some specific constructors
deriveEnumerable' :: ConstructorDeriv -> Q [Dec]
deriveEnumerable' (n,cse) =
  fmap return $ instanceFor ''Enumerable [enumDef] n 
  where
    enumDef :: [(Name,[Type])] -> Q Dec
    enumDef cons = do
      sanityCheck
      fmap mk_freqs_binding [|datatype $ex |] 
      where
        ex = listE $ map cone cons
        cone xs@(n,_) = maybe (cone' xs) id $ lookup n cse
        cone' (n,[]) = [|c0 $(conE n)|]
        cone' (n,_:vs) = 
          [|c1 $(foldr appE (conE n) (map (const [|uncurry|] ) vs) )|]
        mk_freqs_binding :: Exp -> Dec
        mk_freqs_binding e = ValD (VarP 'enumerate ) (NormalB e) []
        sanityCheck = case filter (`notElem` map fst cons) (map fst cse) of
          [] -> return ()
          xs -> error $ "Invalid constructors for "++show n++": "++show xs
        


