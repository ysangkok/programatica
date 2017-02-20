module Solve(Tactic(..),solveC,suggestions,solveFirstOrder ) where 

import ISyntax
import Eval
import Utilities
import MetaVarState(Constraint(..),transInfo,environment,context,parseInfo,metaJudg,MetaVarV,visibility,pos)
import Monads
import PPrint 
import CSyntax
import CITranslate
import CITrans
import Position
import Error
import Maybe(catMaybes)
import SimpleICTranslate
import Id
import Util(joinByFst,findDup,apSnd)
import List(intersect,nub)
import ValueToExp(valueToExp, freshVars)
import Equal(simplifyCs)
import AgdaTrace
import Typechecking


data Tactic = Tactic Int 
          deriving (Show,Eq)


splitApp :: Exp -> PCM (Exp,[(Bool,Exp)])
splitApp (EStop _ (EApp f as)) = return (f,as)
splitApp (EApp f as)           = return (f,as)
splitApp _                     = passError ENoSolve


getFunAppSolve  :: Constraint -> PCM [Constraint]
getFunAppSolve  (Constraint v1 v2) =
   do (f,as) <- splitApp v1
      (g,bs) <- splitApp v2
      f' <- getConstant f
      g' <- getConstant g
      if f' == g'
            then return $ zipWith Constraint (map snd as) (map snd bs)
            else passError ENoSolve

  where getConstant (EConstV c _) = return c
        getConstant (EClos _ e)   = getConstant e
        getConstant (EConst c _)  = return c
        getConstant _             = passError ENoSolve



solveFunApp1 ::  Constraint -> PCM [CMetaSubst]
solveFunApp1 c = do 
    cs <- getFunAppSolve c `handle_` (return [])
    rs <- mapM (mkMaybeError.solve1) cs
    ss <- mapM solveFunApp2 cs
    return $  (catMaybes rs)  ++ concat ss
                 




solveFunApp2 :: Constraint -> PCM [CMetaSubst]
solveFunApp2 c = 
                 do cs <- getFunAppSolve c `handle_` (return [])
                    rs <- mapM (mkMaybeError.solve2) cs
                    ss <- mapM solveFunApp2 cs
                    return $  (catMaybes rs)  ++ concat ss
                 



getVarAppSolve :: Constraint -> PCM Constraint
getVarAppSolve (Constraint v1 v2) = 
     do (f,as) <- splitApp v1
        (g,bs) <- splitApp v2
        let (as',bs') = remVars (reverse as) (reverse bs)
        return $ Constraint (eApp' f (reverse as')) (eApp' g (reverse bs'))
      where remVars  [] [] = ([],[])
            remVars as'@((_,EVar x _) : as) bs'@((_,EVar y _) : bs)
                            | x == y = remVars as bs
                            | otherwise = (as',bs')
            remVars as bs = (as,bs)

splitProj :: Value -> PCM (Value,Id)
splitProj (EProj v c) = return (v,c)
splitProj (EStop _ e) = splitProj e
splitProj _           = passError ENoSolve


getProjSolve :: Constraint -> PCM Constraint
getProjSolve (Constraint v1 v2) = 
              do (v1',c1) <- splitProj v1
                 (v2',c2) <- splitProj v2
                 if c1 == c2 
                    then return $ (Constraint v1' v2')
                    else passError ENoSolve


solveFunApp3 ::  Constraint -> PCM CMetaSubst
solveFunApp3 c = do 
    c' <- getVarAppSolve c
    solve1 c'

solveFunApp4 ::  Constraint -> PCM CMetaSubst
solveFunApp4 c = do 
    c' <- getVarAppSolve c
    solve2 c'




solveProj1 ::  Constraint -> PCM CMetaSubst
solveProj1 c = do 
    c' <- getProjSolve c
    solve1 c'
   

solveProj2 ::  Constraint -> PCM CMetaSubst
solveProj2 c = do 
    c' <- getProjSolve c
    solve1 c'


solveC::  Constraint -> Tactic -> PCM [CMetaSubst]
solveC c (Tactic 1) = do --traceM (ppDebug c)
                         (aut,m,e) <- solve1 c  
                         return [(aut,m,e)]
solveC c (Tactic 2) = do 
    m <- solve2 c 
    return [m]
solveC c (Tactic 3) = do 
                         r <- solveFunApp1 c 
                         case r of
                             [] -> do {m <- solveProj1 c;return [m]}
                             _ -> return r

solveC c (Tactic 4) = do 
    r <- solveFunApp2 c 
    case r of
        [] -> do {m <- solveProj2 c;return [m]}
        _ -> return r


solveC c (Tactic 5) = do 
    m <- solveFunApp3 c 
    return [m]

solveC c (Tactic 6) = do 
    m <- solveFunApp4 c 
    return [m]

solveC c (Tactic n) = raise $ eMsg noPosition (EUnknownTactic (show (n -1)))


trySolveC ::  Constraint -> Tactic -> PCM [(MetaVar,CExpr)]
trySolveC c t = do 
    bmes <- solveC c t `handle_` return []
    return $ [(m,e) | (aut,m,e) <- bmes]



suggest1 ::  Constraint -> PCM [(MetaVar,CExpr)]                
suggest1 c = 
   tempM (do let c'= c -- c' <- getVarAppSolve c `handle_` return c  010313
             s1 <- trySolveC c' (Tactic 1) 
             s2 <- trySolveC c' (Tactic 2) 
             s3 <- trySolveC c' (Tactic 3) 
             s4 <- trySolveC c' (Tactic 4) 
             return $ s1++s2++s3++s4)

suggestions :: [Constraint] -> PCM [(MetaVar,CExpr)]
-- Städa bort dubletter
suggestions cs = 
    do ss <- mapM suggest1 cs
       return $ concat (nub ss)

{- --- -}                    

type MClos = (MetaVar, Environment) -- (head) occurrence in constraints.
type MCApp = (MClos,   [(Bool,Value)])
type MCSol = (MClos,    Value )

getSolve :: Constraint -> PCM MCSol
getSolve (Constraint v1 v2)= plain    v1 v2 $ \ w1 ->
                             plain    v2 v1 $ \ w2 ->
                             etasolve w1 v2 $
                             etasolve w2 v1 $ passError ENoSolve
  where
  plain :: Value -> Value -> (Maybe MCApp -> PCM MCSol) -> PCM MCSol
  plain lv rv cont = do lv' <- unfold lv 
                        --traceM ("Try "++ show lv ++ " got " ++ ppDebug lv')
                        split (appnormalized lv') where
    split (EClos env (EMetaV m _ _ _),vs)| null vs   = return      ((m,env),rv)
                                     | otherwise = cont $ Just ((m,env),vs)
    split  _                                     = cont $ Nothing


--MT: What the ...  ?
--    Value --(imitate2              )-->
--    Exp   --(translate / foldVars  )--> 
--    CExpr --(toIExp                )-->
--    Exp   --(give m, then translate)-->
--    CExpr
solve1, solve2 ::  Constraint -> PCM CMetaSubst
solve_ :: Int ->  Constraint -> PCM CMetaSubst
(solve1, solve2) = (solve_ 1,  solve_ 2)
solve_ tacnum c = do
    traceM ("Solve "++ppDebug c)
    ((m, envc), vc)        <- getSolve c
    -- (_,pai,ct,_,_,_,envm,_) <- getMetaInfo m
    mI <- getMetaInfo m
    let   ct = transInfo mI
          cit = fst ct 
          (xs,cs) = (varScope cit, cstScope cit)
          -- buildMap      :: PCM [(UId,[UId])] 
          buildMap = joinByFst `liftM` foldr bM (return []) xs
          bM x r             = lookupEnv x  (environment mI) >>= ifxWas where
            ifxWas (EVar x0 _) = lookupEnv x0 envc >>= ifxIs  where
              ifxIs (EVar  y _) = ((y,x0):) `liftM` r
              ifxIs _         = noSolve
            ifxWas _         = r
          -- toIExp'       ::                  CExpr -> PCM Exp
          -- solveV        ::                  CExpr -> PCM CExpr
          -- tac_          :: (Exp -> CExpr) -> Bool -> PCM CExpr
          -- tac1, tac2    ::                           PCM CExpr
          toIExp' ce = (toIExp ct (parseInfo mI) noPar ce) `handle_` noSolve
          solveV (CMeta _ _ _ _) = noSolve
          solveV  ce           = do e <- toIExp' ce
                                    traceM ("translated "++ ppReadable e)
                                    j <- getJudg (metaJudg mI)
                                    e' <- (context mI,environment mI) |- (mapJudg (\_ -> e) j)
                                    updateMetaSubst True m e'
                                    traceM$ "Solve Meta variable :" ++ show m ++ " was replaced by "++ ppReadable e'
                                    return (translate False e')
                                    -- traceM$ "Meta variable :" ++ show m ++ " was replaced by "++ ppReadable e
          tac_ i f b   = do 
                          traceM ("Tactic "++ show i)
                          --v <- unfold vc
                          e <- valueToExp b False m vc
                          traceM ("Solve0 " ++ ppReadable vc ++ "\n Solve1 next " ++ ppReadable e)
                          traceM ("Solve1 " ++ ppReadable (f e))
                          solveV $ f e
                             
          tac1 = do yxs <- buildMap            --- v hmm
                    let safe = all(\ (y,xs)-> (isInternalUId y) || all(y==) xs) yxs
                        tac2' = if safe then tac2 else noSolve --traceM ("maybe tac 2") >> 
                    -- traceM ("Safe " ++ show safe ++ show yxs)
                    tac_ 1 (foldVars mI yxs) replInternalOff `handle_` tac2'
          tac2 =    tac_ 2 (translate False)     replInternalOn
    e <- if tacnum == 1 then tac1 else tac2
    return (False,m,e)
  where noSolve = passError ENoSolve

foldVars :: MetaVarV -> [(UId,[UId])] -> Exp -> CExpr
foldVars mI yxs v = go v where
  go (EVar y _)              = CVar |@| y
  go (EBinOp e1 (EVar y _) e2) = CBinOp (go e1) `flip` (go e2) |@| y
  go (EApp  e   es)          = e |>= (|*| es)
  go (EProj e   n )          = e |>= (`CSelect` n)
  go (ECon  i   es)          = CConS i        |*| es
  go (EConF i e es)          = CCon  i (go e) |*| es
  --go (EProd (xs,a) b)        = cUniv1 (CArg [(h,toId x) | (h,x) <- xs] preMeta) preMeta
  --go (EAbs (xs,a) b)         =  Clam (CArg [(h,toId x) | (h,x) <- xs] preMeta) preMeta
  go (EConst c _)           = if renameMap yxs then CVar (toId c) else preMeta
  go e                       = preMeta 

  iop |@| y  = maybe preMeta iop (lookup y safeyxs)
  e   |>= op = case go e of (CMeta _ _ _ _ )-> preMeta ; (ce)-> op ce
  h   |*| es = h `cApply` map(\(h,e) -> (h,go e)) es
  safeyxs    = [(y, toId x) | (y,[x]) <- yxs]
  renameMap yxs = all (\(y,xs) -> length xs == 1)  yxs
  preMeta    =  CMeta (pos mI) (parseInfo mI) (visibility mI) preMetaVar
  -- this is slightly better for simple values
  -- iop|@|y = case lookup y yxs of (Just [x])-> iop (toId x); (_ )-> preMeta

replInternalOn  = True
replInternalOff = False

etasolve :: Maybe MCApp -> Value -> PCM MCSol -> PCM MCSol
-- This function breaks the invariant of being a value since it returns a lambda
-- and not a closure and a lambda. Should be fixed.
etasolve Nothing              _ cont = cont
etasolve (Just ((m,env),vs)) rhs cont = do
    traceM ("In eta solve "++ show rhs)
    xs <- getMetaScope m
    --traceM (ppReadable xs)
    ys <- do vs' <- mapM evalMeta (map snd vs); return [ y | EVar y _ <- vs']
    if ok xs ys then go (map toId xs) ys   `handle_`  cont else  cont
  where
  ok xs ys = length ys == length vs && null (findDup ys) -- && null (intersect xs ys)
  hus0@(h0,us0) = apSnd reverse (appnormalized rhs)
  go ns ys = do (_:zss) <- mapM deps (map snd us0++[h0])
                out `liftM` foldM etalam (hus0,(zss, ns)) (reverse ys)
          where out = (,)(m, shrinkEnv env ys) . eAppr . fst
  eAppr (h,us) = eApp' h (reverse us)          --  ^ EClos needed ?

  etalam (hus@(h,us),(zss, ns)) y = if null us then mklam h else tryeta
    where   --OBS: needs updateEnv that does not ignore internals.
    mklam v = do (ns2,[y2]) <- freshVars replInternalOn ns [y]
                 let envy = updateEnv emptyEnv y (EVar y2 Nothing)
                 v2 <- EAbs ([(False,y2)], PreMeta) `liftM` eval envy v
                 return ((v2,[]),([],ns2))
    tryeta  = evalMeta u >>= try where
      ((_,u):us2, _:zss2) = (us, zss)
      try (EVar z _)| y == z && all(z `notElem`) zss = return ((h,us2),(zss2,ns))
      try ( _    )                                 = mklam  (eAppr hus)

  -- make it a straight func when `whatelse' are known for sure, and put it
  -- in an appropriate place.
  deps v0 = case v0 of
    (EVar  x    _)-> return [x]
    (EApp  h   vs)-> mapdeps (h:map snd vs)
    (EClos env e )-> mapdeps (rangeEnv (env `reducedfor` e))
    (EStop _ e   )-> deps e
    (ECon  _   vs)-> mapdeps (map snd vs)
    (EConF _ a vs)-> mapdeps (a:map snd vs)
    (EProj v _   )-> deps v
    (v)-> -- traceM ("deps:\n"++ppDebug v) >>
          passError ENoSolve  -- temporarily..
  mapdeps = liftM concat . mapM deps
  -- not correct with idata
  env `reducedfor` e = case e of
      --(EStruct   _ _ xs    )-> remwith xs
      (EStruct  _ _ xs _ _)-> remwith xs
      (EConstV   _   xs    )-> remwith xs
      --(Epackage  _ _ xs    )-> remwith xs
      (Epackage _ _ xs _ _)-> remwith xs
      (EMeta  _ _ _ cit _ _  )-> remwith (varScope$ fst cit)
      (EMetaV  _  _   xs _   )-> remwith xs
      ( _ )-> env
    where remwith xs = retrieveE env xs


solveFirstOrder :: MetaVar -> Constraint -> PCM [CMetaSubst]
 -- We can use this when we know that m represents a first 
 -- order term
solveFirstOrder m c@(Constraint v1 v2) = do
     v1 <- unfold v1
     v2 <- unfold v2
     --traceM ("Try find solution " ++ show m ++ " " ++ ppDebug v1 ++ " = " ++ ppDebug v2)
     (v1',v2') <- getSol v1 v2 `handle_` getSol v2 v1
     --traceM ("got Solution " ++ ppDebug v1' ++ " and " ++ ppDebug v2')
     (c@(Constraint v3 v4)) <- getAppSolve v1' v2'
     --traceM ("Constr " ++ ppDebug c)
     subst <- if (isMeta v3) && isConstOrVar v4 then solve1 c else passError ENoSolve
     return [subst]
  where 
        getSol (EStop m' v) v' | m == m' = return (v,v') 
        getSol _ _ = passError ENoSolve
        isMeta (EClos _ (EMetaV m' _ _ _)) = m == m'
        isMeta _ = False
        isConstOrVar (EClos _ (EConstV _ _)) = True
        isConstOrVar (EVar _ _) = True
        isConstOrVar _ = False
        getAppSolve v v' = do 
            (h,vs) <- splitApp v
            (h',vs') <- splitApp v'
            return (Constraint h h') -- do we need to test that vs is equal to vs'?
