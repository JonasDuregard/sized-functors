{-#LANGUAGE GADTs, TypeOperators, DeriveDataTypeable, DeriveFunctor, StandaloneDeriving #-}

module Control.Enumerable.Functions
 ( (:->)
 , ($$)
 -- * Type class
 , Parameter(..)
 , signature, p0, p1, p2, p3, p4, p5
 , lets
 -- * Various
 , Function
 , isMinimal
 , rhss
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
  Let         :: (String -> String) -> (a -> x) -> (x :-> b) -> (a :-> b)
  Case        :: [Pattern a b] -> (a :-> b)
  Uncurry     :: (a :-> (b :-> c)) -> ((a,b) :-> c)
  deriving (Typeable)

infixr 5 :->

instance Show b => Show (a :-> b) where
  show = showFun

-- | Case alternatives matching on values of type a, mapping to values of type b
data Pattern a b where
  Pattern :: ([PatPrint] -> PatPrint,Int) -> (a -> Maybe x) -> (x :-> b) -> Pattern a b
  NilPat  :: String -> (a -> Bool) -> b -> Pattern a b
  deriving (Typeable)

deriving instance Functor ((:->) a)
deriving instance Functor (Pattern a)

-- | Returns all constants assigned to right hand side values of pattern matches
rhss :: (a :-> b) -> [b]
rhss (Constant x) = [x]
rhss (Case ps)    = concatMap rhsPattern ps
rhss (Let _ _ f)  = rhss f
rhss (Uncurry f)  = concatMap rhss (rhss f)

rhsPattern :: Pattern a b -> [b]
rhsPattern (Pattern _ _ f)  = rhss f
rhsPattern (NilPat _ _ x)   = [x]

-- | Is this function minimal in the sense that it has no "false" case distinctions (yielding the same value for all cases)
isMinimal :: Eq b => Function a b -> Bool
isMinimal = either (const True) id . miniF

miniF :: Eq b => (a :-> b) -> Either b Bool
miniF (Constant x) = Left x
miniF (Case ps)    = miniP ps
miniF (Let _ _ f)  = miniF f
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

infixl 4 $$

-- | Function application (converts a :-> function to a -> function)
($$) :: (a :-> b) -> a -> b
Constant a $$ b     = a
Case ps $$ b        = maybe (error "Incomplete pattern") id $ msum [match p b | p <- ps]
Let _ f g $$ b      = g $$ (f b)
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


lets :: (Parameter x, Enumerable b, Sized f, Typeable f, Typeable a) =>
           String -> (a -> x) -> Shared f (a :-> b)
lets s = lets' ((s ++ " ")++)

lets' :: (Parameter x, Enumerable b, Sized f, Typeable f, Typeable a) =>
           (String -> String) -> (a -> x) -> Shared f (a :-> b)
lets' s f = share $ fmap (Let s f) access

showPat :: (Eq a, Show a) => a -> b -> Pattern a b
showPat a b = NilPat (show a) (==a) b

instance Parameter () where
  functions = share $ fmap Constant access -- Avoid pattern matching on ()

instance (Parameter a, Parameter b) => Parameter (a,b) where
  functions = share $ fmap Uncurry access

instance (Parameter a, Parameter b, Parameter c) => Parameter (a,b,c) where
  functions = signature $ \f -> [pTuple 3 Just f]

instance (Parameter a, Parameter b, Parameter c, Parameter d) => Parameter (a,b,c,d) where
  functions = signature $ \f -> [pTuple 4 Just f]

instance (Parameter a, Parameter b, Parameter c, Parameter d, Parameter e) => Parameter (a,b,c,d,e) where
  functions = signature $ \f -> [pTuple 5 Just f]

pTuple k = Pattern (\ss _ -> "("++intercalate "," (map ($ False) ss) ++ ")", k)


instance (Parameter a, Parameter b) => Parameter (Either a b) where
  functions = signature go
    where go (f,g) = [p1 "Left" extL f, p1 "Right" extR g]
          extL = either Just (const Nothing)
          extR = either (const Nothing) Just


instance Parameter Bool where
  functions = signature go
    where go (f,g) = [showPat False f, showPat True g]

instance Parameter Ordering where
  functions = signature $ \(f,g,h) -> [showPat LT f, showPat EQ g, showPat GT h]

instance Parameter a => Parameter [a] where
  functions = signature go
    where go (f,g) = [p0 "[]" null f, Pattern (\[s1,s2] -> parenPrt $ s1 True ++ ":" ++ s2 True,2) extCons g]
          extCons xs = do h:t <- return xs ; return (h,t) -- Nicer way to write patterns? LC + listToMaybe?
                     -- listToMaybe [(h,t)|let h:t = xs]

instance Parameter a => Parameter (Maybe a) where
  functions = signature go
    where go (f,g) = [p0 "Nothing" (maybe True (const False)) f, p1 "Just" id g]


instance Parameter Int where
  functions = lets' str fun where
    str i = "replicate (2*abs "++i++" - fromEnum ("++i++" > 0)) ()"
    fun i = replicate (2*abs i - fromEnum (i > 0)) ()




dPattern :: (String,Int) -> (a -> Maybe x1) -> (x1 :-> b) -> Pattern a b
dPattern (s,k) = Pattern (\bss -> parenPrt $ s ++ " " ++ unwords (map ($ True) bss),k)



p0 :: String -> (a -> Bool) -> b -> Pattern a b
p0 s f b = NilPat s f b

p1 :: String -> (a -> Maybe x1) -> (x1 :-> b) -> Pattern a b
p1 s = dPattern (s,1)

p2 :: String -> (a -> Maybe (x1,x2)) -> ((x1,x2) :-> b) -> Pattern a b
p2 s = dPattern (s,2)

p3 :: String -> (a -> Maybe (x1,x2,x3)) -> ((x1,(x2,x3)) :-> b) -> Pattern a b
p3 s e = dPattern (s,3) (fmap (fmap flop) e) where
  flop (a,b,c)      = (a,(b,c))

p4 :: String -> (a -> Maybe (x1,x2,x3,x4)) -> ((x1,(x2,(x3,x4))) :-> b) -> Pattern a b
p4 s e = dPattern (s,4) (fmap (fmap flop) e) where
  flop (a,b,c,d)      = (a,(b,(c,d)))

p5 :: String -> (a -> Maybe (x1,x2,x3,x4,x5)) -> ((x1,(x2,(x3,(x4,x5)))) :-> b) -> Pattern a b
p5 s e = dPattern (s,5) (fmap (fmap flop) e) where
  flop (a,b,c,d,e)      = (a,(b,(c,(d,e))))

-- ...




-- Showing functions

showFun :: Show b => (a :-> b) -> String
showFun = showLam . assign . toExpr


data Binding v = Constr ([PatPrint] -> PatPrint) [Binding v]
               | Pair (Binding v) (Binding v)
               | Var v
               | Wild

instance Show v => Show (Binding v) where
  show b = noP $ bindPrt b

bindPrt :: Show v => Binding v -> PatPrint
bindPrt Wild           = enumPrt "_"
bindPrt (Var v)        = enumPrt (var v)
bindPrt (Pair b1 b2)   = enumPrt $ "(" ++ noP (bindPrt b1) ++ ", " ++  noP (bindPrt b2) ++ ")"
bindPrt (Constr p bs)  = p (map bindPrt bs)


unif :: Eq v => [Binding v] -> Binding v
unif = foldr greatest Wild where

  greatest :: Eq v => Binding v -> Binding v -> Binding v
  greatest Wild x                       = x
  greatest y Wild                       = y
  greatest (Var a) (Var b) | a == b     = Var a
  greatest (Pair x y) (Pair a b)        = Pair (greatest x a) (greatest y b)
  greatest (Constr f xs) (Constr _ ys)  = Constr f $ unifs [xs,ys]

  unifs :: Eq v => [[Binding v]] -> [Binding v]
  unifs bs = map unif (transpose bs)


data Expr v b  = CaseE v [(Binding v, Expr v b)]
               | ResE b
               | LetE (Binding v) (String -> String) v (Expr v b)


showLam :: (Show v, Show b) => (Binding v, Expr v b) -> String
showLam (b,e) = "\\" ++ yesP (bindPrt b) ++ " -> " ++ unlines (showExpr e)

showExpr :: (Show v, Show b) => Expr v b -> [String]
showExpr (ResE x)        = lines (show x)
showExpr (LetE b p v e)  = ("let "++show b++" = " ++ p (var v) ++ " in ")
                           : indent (showExpr e)
showExpr (CaseE k ps)    = ("case "++var k++" of") : indent sps where
  (bs,es) = unzip ps
  sbs = pad (map show bs)
  ses = map (\e -> " -> " .++ showExpr e) es
  sps = concat $ zipWith (.++) sbs ses

  -- showMatcher (b,e)  = (show b ++ " -> ") .++ showExpr e
  s .++ []           = s:[] -- for show instances returning empty strings
  s .++ (s2:ss)      = (s++s2) : ss

  pad ss = let m = maximum (map length ss) in map (take m . (++ repeat ' ')) ss

toExpr :: (a :-> b) -> (Binding (), Expr () b)
toExpr (Case ps)     = (Var (), CaseE () $ map toExprP ps)
toExpr (Let p _ f)   = case toExpr f of
  (Wild, e) -> (Wild, e)
  (b,e)     -> (Var (), LetE b p () e)
toExpr (Constant x)  = (Wild, ResE x)
toExpr (Uncurry f)   = (Pair b1 b2, e2) where
  (b1,e1) = toExpr f
  (b2,e2) = flat e1

flat :: Expr () (a :-> b) -> (Binding (), Expr () b)
flat (ResE f)        = toExpr f
flat (LetE b p v e)  = fmap (LetE b p v) (flat e)
flat (CaseE v ms)    = (unif bs, CaseE v ms') where
  (bs,ms') = unzip [(b1,(b2,e')) | (b2,e) <- ms, let (b1,e') = flat e]

toExprP :: Pattern a b -> (Binding (), Expr () b)
toExprP (NilPat s _ b) = (Constr (\_ -> enumPrt s) [], ResE b)
toExprP (Pattern (s,n) _ f) = (Constr s (extr n b), e) where
  (b,e) = toExpr f
  extr n Wild          = replicate n Wild
  extr 1 b             = [b]
  extr n (Pair b1 b2)  = b1 : extr (n-1) b2




type PatPrint = Bool -> String

enumPrt :: String -> PatPrint
enumPrt s = \_ -> s

parenPrt :: String -> PatPrint
parenPrt s b = if b then "(" ++ s ++ ")" else s

var n = 'x':show n
indent ss = map ("  " ++) ss

noP :: PatPrint -> String
noP f = f False

yesP :: PatPrint -> String
yesP f = f True




newtype M a = M {runM :: [Int] -> [Int] -> ([Int],a)} deriving Functor
instance Applicative M where
  pure a     = M (\_ ss -> (ss,a))
  fs <*> xs  = fs >>= (`fmap` xs)

instance Monad M where
  return   = pure
  M m >>= f  = M g where
    g a ss = h a ss' where
      (ss',x) = m a ss
      (M h) = f x

produce :: [Int] -> M a -> M a
produce vs (M a) = M $ \xs ss -> a (vs ++ xs) ss

consume :: (Int -> M a) -> M a
consume f = M $ \(x:xs) ss -> runM (f x) xs ss

pop :: M Int
pop = M (\_ (s:ss) -> (ss,s))


assign :: (Binding (), Expr () b) -> (Binding Int, Expr Int b)
assign e = snd $ runM (assignm e) [] [1..]

assignM :: Expr () b -> M (Expr Int b)
assignM (ResE x)       = pure $ ResE x
assignM (LetE b p _ e) = consume $ \v -> do
  (b',vs) <- bind b
  e' <- produce vs $ assignM e
  return (LetE b' p v e')
assignM (CaseE _ ms)   = consume $ \v -> do
  ms' <- mapM assignm ms
  return $ CaseE v ms'

assignm :: (Binding (), Expr () b) -> M (Binding Int, Expr Int b)
assignm (b,e) = do
      (b',vs) <- bind b
      e' <- produce vs $ assignM e
      return (b',e')

bind :: Binding () -> M (Binding Int, [Int])
bind Wild            = return (Wild,[])
bind (Var _)         = pop >>= \s -> return (Var s,[s])
bind (Pair b1 b2)    = do
  (b1',r1) <- bind b1
  (b2',r2) <- bind b2
  return $ (Pair b1' b2',r1++r2)
bind (Constr pp bs)   = do
  (bs', vss) <- fmap unzip (mapM bind bs)
  return (Constr pp bs', concat vss)
