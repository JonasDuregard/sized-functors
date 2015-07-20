{-#LANGUAGE GADTs, TypeOperators, DeriveDataTypeable, DeriveFunctor, StandaloneDeriving #-}

module Control.Enumerable.Functions
 ( (:->)
 , Parameter(..)
 , signature, p0, p1, p2, p3, p4, p5
 -- * Various
 , Function
 )where

import Control.Sized
import Control.Enumerable
import Control.Enumerable.Values
import Control.Enumerable.Count

import Control.Monad(msum)
import Data.List(intercalate, transpose)
import Data.Either(partitionEithers)

-- | Type alias for avoiding TypeOperators.
type Function a b = a :-> b


-- | Showable functions from a to b
data a :-> b where
  Constant    :: b -> (a :-> b)
  Case        :: [Pattern a b] -> (a :-> b)
  Uncurry     :: (a :-> (b :-> c)) -> ((a,b) :-> c)
  deriving (Typeable)

infixr 5 :->

-- | Case alternatives matching on values of type a, mapping to values of type b
data Pattern a b where
  Pattern :: (String,Int) -> (a -> Maybe x) -> (x :-> b) -> Pattern a b
  NilPat  :: String -> (a -> Bool) -> b -> Pattern a b
  deriving (Typeable)

deriving instance Functor ((:->) a)
deriving instance Functor (Pattern a)

rhss :: (a :-> b) -> [b]
rhss (Constant x) = [x]
rhss (Case ps)    = concatMap rhsPattern ps
rhss (Uncurry f)  = concatMap rhss (rhss f)

rhsPattern :: Pattern a b -> [b]
rhsPattern (Pattern _ _ f)     = rhss f
rhsPattern (NilPat _ _ x)  = [x] 

-- Is this function minimal in the sense that it has no "false" case distinctions (yielding the same value for all cases)
mini :: Eq b => Function a b -> Bool
mini = either (const True) id . miniF

miniF :: Eq b => Function a b -> Either b Bool
miniF (Constant x) = Left x
miniF (Case ps)    = miniP ps
miniF (Uncurry f)  = resolve $ map miniF $ rhss f

miniP :: Eq b => [Pattern a b] -> Either b Bool
miniP ps = resolve (map minP ps)
  where  minP :: Eq b => Pattern a b -> Either b Bool
         minP (NilPat _ _ x)   = Left x
         minP (Pattern _ _ f)  = miniF f

resolve :: Eq b => [Either b Bool] -> Either b Bool
resolve ebs = case bs of
  [] -> case xs of  -- There are only leafs
    [x]    -> Left x
    (x:xs) -> Right $ not $ all (x ==) xs
  _  -> Right (and bs) -- All subpatterns are minimal
  where  (xs,bs) = partitionEithers ebs


instance Show b => Show (a :-> b) where
  show            = showFunction
  showsPrec p f   = showParen True (showFunction f ++)


data Binding = Pair Binding Binding
             | Var String
             | Wild

-- Unify patterns on the same sequence of values
unif :: [[Binding]] -> [Binding]
unif = map unif' . transpose where
  unif' :: [Binding] -> Binding
  unif' = foldr greatest Wild
  greatest :: Binding -> Binding -> Binding
  greatest Wild x                   = x
  greatest y Wild                   = y
  greatest (Var x) (Var y) | x == y = Var x
  greatest (Pair x y) (Pair a b)    = mkPair (greatest x a) (greatest y b)

uncurried :: [Binding] -> [Binding]
uncurried []         = []
uncurried [x]        = [mkPair x Wild]
uncurried (x:y:xys)  = mkPair x y : xys

mkPair Wild Wild = Wild
mkPair x y = Pair x y

singbind :: [Binding] -> Binding
singbind []   = Wild
singbind [x]  = x

var n = 'x':show n
var' n = ' ' : var n


instance Show Binding where
  show Wild = "_"
  show (Var s) = s
  show (Pair b1 b2) = "("++show b1++","++show b2++")"

conBinding :: Int -> Binding -> [String]
conBinding 1 b = [show b]
conBinding n Wild = replicate n "_"
conBinding n (Pair b1 b2) = show b1 : conBinding (n-1) b2


-- The int is the number of variables in scope.
type Shower x = Int -> x -> ([String],[Binding])

indent ss = map ("  " ++) ss



showFunction :: Show b => (a :-> b) -> String
showFunction = renderTop . showUncurry showBase 0

showBase :: Show a => Shower a
showBase _ x = (lines (show x),[])



showUncurry :: Shower b -> Shower (a :-> b)
showUncurry sh s (Constant f)  = fmap (Wild:) $ sh s f
showUncurry sh s (Uncurry f)   = fmap uncurried $ showUncurry (showUncurry sh) s f
showUncurry sh s (Case ps)     = (ss,Var (var s) : bs) where
  (sss,bs) = showMatches sh (s+1) ps
  ss = ("case " ++ var s ++ " of"): sss


renderTop :: ([String],[Binding]) -> String
renderTop (ss,b) = "\\" ++ show (singbind b) ++ " -> " ++ init (unlines ss)


showMatches :: Shower b -> Shower [Pattern a b]
showMatches sh n ps = (indent $ concat sss, unif bs) where
  (sss,bs,ls) = unzip3 (map (showMatch sh n) ps)
  
  showMatch sh n (NilPat s _ f) = ((pad s ++ " -> " ++ x):xs,bs,length s) where
    ((x:xs),bs) = sh n f
  showMatch sh n (Pattern (s,k) _ f) = (ss,bs,length spat) where
    ss = (pad spat ++ " -> " ++ sf) : ssf
    bs = drop 1 bsf
    pat = head (bsf ++ [Wild])
    spat = s ++ " " ++ unwords (conBinding k pat)
    (sf:ssf,bsf) = showUncurry sh n f

  pad s = take ml $ s ++ repeat ' '
  ml = maximum ls


-- | Function application
($$) :: (a :-> b) -> a -> b
Constant a $$ b     = a
Case ps $$ b        = maybe (error "Incomplete pattern") id $ msum [match p b | p <- ps]
Uncurry f $$ (a,b)  = f $$ a $$ b

match :: (Pattern a b) -> a -> Maybe b
match (Pattern _ p f) = fmap (f $$) . p
match (NilPat _ p f)  = \a -> if p a then Just f else Nothing

instance (Enumerable b, Parameter a) => Enumerable (a :-> b) where
  enumerate = functions


-- | A class of types that can be used as parameters to enumerable functions
class Typeable a => Parameter a where
  functions :: (Enumerable b, Typeable f, Sized f) => Shared f (a :-> b)


signature :: (Typeable a, Enumerable x, Enumerable b, Typeable f, Sized f) 
               => (x -> [Pattern a b]) -> Shared f (a :-> b)
signature f = share (fmap Constant access <|> pay ps) 
  where ps = fmap (Case . f) access -- :: f [Pattern a b]


showPat :: (Eq a, Show a) => a -> b -> Pattern a b
showPat a b = NilPat (show a) (==a) b

instance Parameter () where
  functions = share $ fmap Constant access -- Avoid pattern matching on ()

instance (Parameter a, Parameter b) => Parameter (a,b) where
  functions = share $ fmap Uncurry access

instance (Parameter a, Parameter b, Parameter c) => Parameter (a,b,c) where
  functions = signature $ \f -> [p3 "(,,)" Just f]

instance (Parameter a, Parameter b, Parameter c, Parameter d) => Parameter (a,b,c,d) where
  functions = signature $ \f -> [p4 "(,,,)" Just f]

instance (Parameter a, Parameter b, Parameter c, Parameter d, Parameter e) => Parameter (a,b,c,d,e) where
  functions = signature $ \f -> [p5 "(,,,,)" Just f]



instance (Parameter a, Parameter b) => Parameter (Either a b) where
  functions = signature go
    where go (f,g) = [p1 "Left" extL f, p1 "Right" extR g]
          extL = either Just (const Nothing)
          extR = either (const Nothing) Just


instance Parameter Bool where
  functions = signature go
    where go (f,g) = [showPat False f, showPat True g]

instance Parameter a => Parameter [a] where
  functions = signature go
    where go (f,g) = [p0 "[]" null f, p2 "(:)" extCons g]
          extCons xs = do h:t <- return xs ; return (h,t) -- Nicer way to write patterns? LC + listToMaybe?
                     -- listToMaybe [(h,t)|let h:t = xs]

instance Parameter a => Parameter (Maybe a) where
  functions = signature go 
    where go (f,g) = [p0 "Nothing" (maybe True (const False)) f, p1 "Just" id g]


data Three = Three Bool (Either Bool Bool) [Bool]

instance Parameter Three where
  functions = signature go where
    go f = [p3 "Three" extr f]
    extr (Three a b c) = Just (a,b,c)

type TestType = (Bool,Bool,Either () ()) :-> ()
tst1 n = values n :: [TestType]



p0 :: String -> (a -> Bool) -> b -> Pattern a b
p0 s f b = NilPat s f b

p1 :: String -> (a -> Maybe x1) -> (x1 :-> b) -> Pattern a b
p1 s = Pattern (s,1)

p2 :: String -> (a -> Maybe (x1,x2)) -> ((x1,x2) :-> b) -> Pattern a b
p2 s = Pattern (s,2)

p3 :: String -> (a -> Maybe (x1,x2,x3)) -> ((x1,(x2,x3)) :-> b) -> Pattern a b
p3 s e = Pattern (s,3) (fmap (fmap flop) e) where
  flop (a,b,c)      = (a,(b,c))

p4 :: String -> (a -> Maybe (x1,x2,x3,x4)) -> ((x1,(x2,(x3,x4))) :-> b) -> Pattern a b
p4 s e = Pattern (s,4) (fmap (fmap flop) e) where
  flop (a,b,c,d)      = (a,(b,(c,d)))

p5 :: String -> (a -> Maybe (x1,x2,x3,x4,x5)) -> ((x1,(x2,(x3,(x4,x5)))) :-> b) -> Pattern a b
p5 s e = Pattern (s,5) (fmap (fmap flop) e) where
  flop (a,b,c,d,e)      = (a,(b,(c,(d,e))))

-- ...









