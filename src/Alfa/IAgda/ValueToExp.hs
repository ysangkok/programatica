{-|
  Imitates rom 'Value' to 'ISynType.Exp'.  

  A very careful translation to expressions, in contrast to 'imitate',
  which should be used only for printing.


-}

module ValueToExp(valueToExp,freshVars)  -- > Solve
where
import AgdaScans
import ISyntax
import MetaVarState
import Monads -- (internalError,traceM,(=<<),liftM,liftM2,join)
import ProofMonad
import Eval
import PPrint(ppDebug,ppReadable,PDetail(..),pIText)
import UnfoldL(unfoldL,SemiFolded)
import Error 
import CITrans(CITrans,cstScope,varScope,inScopeVar,inScopeCst)

-- * a substitute for
--   Imitate2.imitate (True,False,False,False,replaceinternalflag,Just cs)
-- * FreshVars give real UIds now (needed for solve with eta).
--   (ISyntax.update must not ignore internals now.)
-- * unfoldL descends only for certain subexpressions.  When in doubt,
--   go for goE emptyEnv.

valueToExp :: 
            Bool  -- replace internals
         -> Bool  -- restricted imitations (result in scope and no meta-variables)
         -> MetaVar -- the meta variable that might be substituted for the reulting expression
         -> Value -> PCM Exp

valueToExp  rplflg restricted m v = do
   mI <- getMetaInfo m
   let tc = transInfo mI
       cit = fst tc 
       (lcs,tkn0) = (cstScope cit , varScope cit)
   valueToExp' mI tc cit lcs tkn0
  where valueToExp' mI tc cit lcs tkn0 = goV (map toId tkn0) v where
          rplint x = rplflg && isInternalUId x
          metaVar = EMeta m (pos mI) (parseInfo mI) (transInfo mI) (prec mI) (visibility mI)
          --goS ::  [Id]              -> SemiFolded -> PCM Exp
          --goV ::  [Id]              -> Value      -> PCM Exp
          --goE :: ([Id],Environment) -> Exp        -> PCM Exp
          goV  tkn       v = goS tkn =<< if restricted then unfold v else unfoldL lcs v
          goE (tkn, env) e = goV tkn =<< eval env e
          goS tkn1 s1 = case s1 of
              (EVar  x  _ )| restricted -> if inScopeVar cit x then return (EVar x (Just tc)) else passError EImitate
              (EVar  x _)       -> if rplint x then 
                                       return metaVar
                                      else return (EVar x (Just tc))
              (EApp  s   ss)-> do let (hs,ss') = unzip ss
                                  es <- mapM goV1 ss'
                                  e <- goS1 s
                                  return $ eApp' e (zip hs es)
              (ECon  n   vs)-> do let (hs,vs') = unzip vs
                                  es <- mapM goV1 vs'
                                  return $ ECon n (zip hs es)
              (EConF n t vs)-> do let (hs,vs') = unzip vs
                                  es <- mapM goV1 vs'
                                  -- e <- goS1 t
                                  return $ EConF n t (zip hs es)
              (EArrow h v1 v2) -> do v1' <- goV1 v1
                                     v2' <- goV1 v2
                                     return$ EArrow h v1' v2'
              (EProj s n   )-> (`EProj` n) `liftM` goS1 s
              (EStop _ _   )-> if not restricted then 
                                   return metaVar
                               else passError EImitate
      
              (EClos env e1) -> case e1 of
                (EAbs      b e          )|not restricted  -> goAbs  env EAbs     b e
                (EProd     b e          )|not restricted  -> goAbs  env EProd    b e
                (EMetaV m pai _ tc      )| not restricted  -> return metaVar
                (EConstV   c _          )| restricted -> do 
                        d <- def c
                        let xs = varScopeDef d
                            xs' = varScope cit
                        -- traceM ("Equal vars "++show xs ++ " "++show xs')
                        if inScopeCst cit c then return (EConst c (Just tc)) else passError EImitate
                (EConstV   c _          )                  -> return (EConst c (Just tc))
                (EStruct  pos ds xs cs as)| not restricted  -> goLets env EStruct  pos ds xs cs as
                (Epackage pos ds xs cs as)| not restricted  -> goLets env Epackage pos ds xs cs as
                ( _ )| not restricted  -> return s1 -- hmm
                _                                          -> passError EImitate 
              ( _ ) -> do -- traceM (ppDebug s1)
                  return s1 
             where 
                (goS1, goV1) = (goS tkn1, goV tkn1)

                goAbs env binder bd e = do
                    (nenv2,bd2) <- goBd rplflg (map toId tkn0++tkn1,env) bd
                    binder bd2 `liftM` goE nenv2 e
 
                goLets env wrap pos ds xs cs as = do ds' <- mapM goLet ds
                                                     return$ wrap pos ds' xs cs as
                 where 
                 goLet (DSimple d  ) = DSimple `liftM`      goDef d
                 goLet (DMutual ds') = DMutual `liftM` mapM goDef ds'
                 -- obs: only ys names are considered taken,ignoring tkn1,xs,domEnv env.
                 yenv ys = (map toId ys, env)
                 goEy    = goE . yenv
                 goDef (Def blocked rec ps c ys tel a rhs) = do 
                     (nenv2,tel2) <- mapMAccumL (goBd False) (yenv ys) tel
                     let goE2 = goE nenv2         --  ^ hmm
                     a2      <- goE2 a
                     let wrap = Def blocked rec ps c ys tel2 a2
                     case rhs of (DExp e)-> (wrap . DExp) `liftM` goE2 e
                                 ( _    )-> return (wrap PN)
                 goDef   (UnTypedDef blocked rec ps c ys  (DExp e)) =
                         (UnTypedDef blocked rec ps c ys . DExp) `liftM` goEy ys e
                 goDef d@(UnTypedDef _ _ _ _ _ _ ) = return d
                 goDef   (DOpen    e   oas@(OpenArgs _ ys)) =
                    DOpen `flip` oas `liftM` goEy ys e

                goBd rplflg' nenv@(ns,env) (xs,a) = do
                    a2        <- goE nenv a
                    let (hs,xs') = unzip xs
                    (ns2,xs2) <- freshVars rplflg' ns xs'
                    return ((ns2, addIdEnv env xs' xs2), (zip hs xs2, a2)) 
