{-| 

  Imitates from Value to CSyntax.CExpr. Should only be used for
  printing since it might be unsafe. 

  
-}
module Imitate(expand,imitateConstraint) where
import AgdaScans(mapMAccumL)
import ISyntax
import CSyntax
import SimpleICTranslate(translate,translateProp,translateOpen)
import MetaVarState
import Monads -- (internalError,traceM,(=<<),liftM,liftM2,join)
import ProofMonad
import Eval
import PPrint(ppDebug,ppReadable,PDetail(..),pIText)
import ValueToExp(freshVars)
import UnfoldL

expand :: (Maybe [UId]) -> [Id] -> Exp -> PCM CExpr
expand = imitate False

type ExpFlag = (Bool,Bool,Bool,Bool,Bool,Maybe [UId]) -- relic.
imitateConstraint :: ExpFlag -> Constraint -> PCM CConstraint
imitateConstraint (_,keepEnvMeta,_,_,_,mxs) (Constraint v1 v2) =
  liftM2 CEq (imi v1) (imi v2) where imi = imitate keepEnvMeta mxs []

imitate :: Bool        -- keepEnvMeta?
        -> Maybe [UId] -- local constants not to unfold.
        -> [Id]        -- names taken
        -> Value
        -> PCM CExpr
imitate keepEnvMeta mxs = val where
  val :: [Id]               -> Value      -> PCM CExpr
  semi:: [Id]               -> SemiFolded -> PCM CExpr
  exp :: ([Id],Environment) -> Exp        -> PCM CExpr
  exp (tkn,env) e = val  tkn =<< eval env e
  val tkn     v = semi tkn =<< unfoldL' v
  unfoldL'      =  maybe return unfoldL mxs
  semi tkn1 s1 = case s1 of
      (EVar   x _         )-> return (cvar x)
      (EConst c _         )-> return (cvar c)
      (ESort  pos (Sort s))-> return (CStar pos s s)
      (EApp   s   vs      )-> liftM2  cApply           (val1 s) (valBs [ arg | arg@(hidden,v) <- vs,not hidden])
      (ECon   n   vs      )-> fmap   (cApply (CConS n))          (valBs vs)
      (EConF  n t vs      )-> liftM2 (cApply .CCon  n) (val1  t) (valBs vs)
      (EProj  s n         )-> fmap   (`CSelect` n)     (val1 s)
      (EStop  m s         )-> do isUninst <- isUninstantiated m
                                 if isUninst then semi tkn1  s
                                    else val1  s 
      (EArrow h v1 v2     )-> liftM2 (CArrow h) (val1 v1) (val1 v2)
      (EClos  r e1)-> let
          tr2       = (tkn1 , r)
          abs f b e = do (tr3,cb) <- bind tr2 b; f cb `fmap` exp tr3 e
          letdef (DSimple d ) = fmap CSimple (     ldef d )
          letdef (DMutual ds) = fmap CMutual (mapM ldef ds)
          ldef (Def blocked _ ps c xs tel a d) = do
            (tr3, bs)     <- mapMAccumL bind (toIds xs, r) tel
            let (exp3, ct) = (exp tr3, map carg bs)
                (ps' , c') = (map translateProp ps, toId c)
            a3            <- exp3 a
            case d of{(DExp e)-> fmap   (CDef ps' .(CValueT c' ct a3)) (exp3 e)
                     ; _      -> return (CDef ps' (CAxiom  c' ct a3))}
          ldef (UnTypedDef _ _ ps c xs (DExp e)) =
               (CDef (map translateProp ps) . CValue (toId c)) `fmap`
                exp (toIds xs, r) e
          ldef (DOpen e (OpenArgs os xs)) =
               (CDef[] . (`COpen` (COpenArgs (map (translateOpen True) os)))) `fmap`
                exp (toIds xs, r) e
        in case e1 of
        (EProd   b e )-> abs (cprod.cargB)          b e
        (EAbs    b e )-> abs (Clam . cargB) b e
        (EConstV c xs)-> return (cvar c)
        (EMetaV  m _ xs _)-> do  -- add information so that automatic is correct??
           isUninst <- isUninstantiated m
           if isUninst then do 
                mI <- getMetaInfo m
                let cm = CMeta (pos mI) (parseInfo mI) (visibility mI) m
                    go x cont = lookupEnv x (environment mI) >>= ifxWas where
                        x' = toId x             -- v shouldn't this be y ?
                        ifxWas(EVar y _) = lookupEnv x r >>= val1 >>= ifxIs where
                                ifxIs(CVar z )|x' == z = cont
                                ifxIs v               = fmap ((x',v):) cont
                        ifxWas _       = cont
                if keepEnvMeta then (`CClos` cm) `fmap` foldr go (return []) xs
                   else return cm
              else do
                e <- getMetaSubst m
                exp (tkn1,r) e
        (EStruct pos ds _ _ _)-> CRecord[] pos `fmap` mapM letdef ds
        (ECase e cbs)-> do
            v <- eval r e
            let mx = case v of (EVar x _)-> Just x; _ -> Nothing
                branch (CBConM c pas _, bre) = do
                  ((tkn3,r3),pas') <- mapMAccumL pat tr2 pas
                  let vx = ECon c . (map ((,) False)). mkVars . map getUIdPatt $ pas
                  r3' <- maybe (return r3)
                        (compEnv r3 . (updateEnv emptyEnv `flip` vx)) mx
                  (,)(CPCon c pas') `fmap` exp (tkn3,r3') bre 
            liftM2 Ccase (val1 v) (mapM branch cbs)
        e -> translate True `fmap` eval r e
      s -> return (translate True s)
    where
    semi1 = semi tkn1; val1 = val tkn1
    valBs es = do let (hs,es2) = unzip es
                  es' <- mapM val1 es2
                  return $ zip hs es'


  bind tr (xs,a) = do 
        let (hs,xs') = unzip xs
        a' <- exp tr a
        (tr',ys) <- fresh tr xs'
        return (tr',(zip hs ys,a'))
  pat tr(PArgT x a) = do a'<-exp tr a; (tr',y:_)<- fresh tr [x]
                         return (tr', CPVar(CPatT  y a'))
  pat tr(PArg  x  ) = do (tr',y:_)<- fresh tr [x];return (tr',CPVar(CPatId y))
fresh (tkn,r) xs = do (tkn',ys) <- freshVars False tkn xs
                      return ((tkn', addIdEnv r xs ys), toIds ys)
cprod b = cUniv1 b

cargB(xs,a) = (CArg xs a)
carg(xs,a) = CArg xs a
cvar  = CVar . toId
toIds = map toId
