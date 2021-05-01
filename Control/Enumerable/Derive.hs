{-#Language CPP#-}
{-#Language TemplateHaskell#-}
module Control.Enumerable.Derive (instanceFor, module Language.Haskell.TH) where
import Language.Haskell.TH

-- General combinator for class derivation
instanceFor :: Name -> [[(Name,[Type])] -> Q Dec] -> Name -> Q Dec
instanceFor clname confs dtname = do
  (cxt,dtvs,cons) <- extractData dtname
  cd              <- mapM conData cons
  let
#if MIN_VERSION_template_haskell(2,10,0)
    mkCxt = fmap (cxt++) $ mapM (appT (conT clname) . varT) dtvs
#else
    mkCxt = fmap (cxt++) $ mapM (classP clname . return . varT) dtvs
#endif
    mkTyp = mkInstanceType clname dtname dtvs
    mkDecs conf = conf cd

  instanceD mkCxt mkTyp (map mkDecs confs)


mkInstanceType :: Name -> Name -> [Name] -> Q Type
mkInstanceType cn dn vns = appT (conT cn) (foldl (appT) (conT dn) (map varT vns))

extractData :: Name -> Q (Cxt, [Name], [Con])
extractData n = reify n >>= \i -> return $ case i of
#if MIN_VERSION_template_haskell(2,11,0)
  TyConI (DataD cxt _ tvbs _ cons _)   -> (cxt, map tvbName tvbs, cons)
  TyConI (NewtypeD cxt _ tvbs _ con _) -> (cxt, map tvbName tvbs, [con])
#else
  TyConI (DataD cxt _ tvbs cons _)   -> (cxt, map tvbName tvbs, cons)
  TyConI (NewtypeD cxt _ tvbs con _) -> (cxt, map tvbName tvbs, [con])
#endif

tvbName :: TyVarBndr flag -> Name
tvbName (PlainTV n _)  = n
tvbName (KindedTV n _ _) = n


conData :: Con -> Q (Name,[Type])
conData c = case c of
  NormalC n sts    -> return (n,map snd sts)
  RecC n vsts      -> return (n,map (\(_,s,t) -> t) vsts)
  InfixC st1 n st2 -> return (n,[snd st1,snd st2])
  ForallC _ _ c'   -> conData c'


x :: IO Type
x = runQ $ (toType ''(,))


toType n = case lookup n tups of
  Nothing -> conT n
  Just q  -> q

tups = (''(), [t|()|]):map (\(n,i) -> (n, tupleT i)) (zip [''(,), ''(,,)] [2..])
