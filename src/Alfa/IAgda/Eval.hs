module Eval(module Eval,module ProofMonad) where

import AgdaTrace
import PPrint(ppDebug,ppReadable)
import Position(Position,noPosition)
import FString 
import PreStrings (fsUnderscore)
import Literal
import Monads
import Error
import CITrans(varScope,freeVarScope)
--import CITranslate(makeUId,idIndex)
import ISyntax
import ProofMonad
import MetaVarState(transInfo)
--import Data.FiniteMap
import MiscId(nilId,consId,trueId,falseId)
import List (nub)
import Native 
import ExpToExp(expToExp)

whnf :: Exp -> Environment -> PCM Value
whnf e env = do v <- eval env e
                unfold v


genVal  :: UId -> PCM  MValue
genVals :: [UId]  -> PCM ([MValue])

genVal x = do i <- idIndex 
              return$ mkInternalVar x i

genVals = mapM genVal
        
genValN :: Int -> PCM [Value]
genValN n = mapM (\_ -> genValDummy) [1..n]

genValDummy = do 
        i <- idIndex
        st <- getStrTable
        let (st',fs) = hmkFString st ("_"++show i)
        putStrTable st'
        let x = internalUId noPosition fs i
        return (EVar x Nothing)


mkInternalVar :: UId -> Int -> Exp
mkInternalVar x i = EVar (internalUId  (getUIdPosition x) (getUIdFString x) i) Nothing



{-| 'evalMeta': Reevaluates a term, removing new beta-redexes which
stem from the instantiation of meta-variables.  This is to preserve
the invariant that internally, terms should not contain
beta-redexes. -}

evalMeta :: Value -> PCM MValue
evalMeta asis = case asis of
  (EStop m v)-> ifM (isUninstantiated m) (return asis) (evalMeta v)
  (EClos env (EMetaV m _ _ _))-> ifM (isUninstantiated m) (vStop m asis)
                                   (eval env =<< getMetaSubst m)
  (EApp h vs)-> do 
        vh <- evalMeta h
        vApp vh vs --(zip bs vs2)
  (EProj h n)-> (`vProj` n) =<< evalMeta h
  (EIf b v1 v2) -> do b' <- evalMeta b 
                      vIf b' v1 v2
 
  ( _       )-> return asis 

type Folded = MValue  -- meaning, UnFolded
-- still to go: see ~/IAgda_notcommitted/Eval.hs 

unfold:: Value -> PCM Folded


unfold v = do
              n <- getTermCounter
              --traceM'$"unfold:in v="++ppDebug v
              out <- unfoldCount n v `handle` termError v
              --traceM'$"unfold:out (v,out)"++ppDebug (v,out)
              return out
          where termError v e =
                  case e of
                      (_,EPass (ETermProblem t)) -> 
                             raise $ eMsg (getExpPos v) (ETermination (ppReadable v) t)
                      _ -> raise e

unfoldCount :: Int -> Value -> PCM Folded
unfoldCount 0 v = do 
        e <- expToExp v
        passError $ (ETermProblem (ppReadable e))
unfoldCount n v = 
           do v' <- evalMeta v
              -- traceM ("evalMeta " ++ ppDebug v ++ " got " ++ ppDebug v')
              v2 <- unfold' n v' []
              return v2

unfold' :: Int -> MValue -> [(Bool,Value)] -> PCM Folded   
{-
unfold' n v vs = 
            do
              --traceM'$"unfold'{:(n,v,vs)=\n"++ppDebug (n,v,vs)
              v' <- unfOld' n v vs
              --traceM'$"unfold'}:(n,v')=\n"++ppDebug (n,v')
              return v'
-}


unfold' 0 v vs = passError $ (ETermProblem (ppReadable (eApp' v vs)))
unfold' n (EApp v vs) vs' = do 
        unfold' n v (vs++vs')
unfold' n (ECon i vs) vs' = do --vs2 <- mapM (\v -> unfold' n v []) vs
                               return $ ECon i (vs++vs')
unfold' n (EConF i v vs) vs' = do --vs2 <- mapM (\v -> unfold' n v []) vs
                                  return $ ECon i (vs++vs')

unfold' n v@(EClos env (EConstV c xs)) vs = 
          do d <- def c
             --traceM ("unfold "++show c ++ " " ++ show xs ++ ppReadable d)
             let rhs = rhsOfDef d
                 abs = (isAbstract d && c `notElem` (accessible env)) 
                 v4 = eApp' v vs
             case rhs of
                DExp e -> 
                  if abs 
                     then return v4
                     else do
                             v' <- eval' env e vs
                             v2 <- unfold' (n-1) v' []
                             --return $ appChoice v2 v4
                             case v2 of
                                 EStop m v3 ->  vStop m (appChoice v3 v4)
                                 _ -> return $ appChoice v2 v4 
                PN -> return v4
                Native -> if abs 
                             then return v4
                             else do let (hs,vs') = unzip vs
                                     vs2 <- mapM (\v -> unfold' n v []) vs'
                                     --traceM (ppReadable vs)
                                     v <-evalNative c (zip hs vs2)
                                     --traceM (ppDebug v)
                                     return v

unfold' n (EProj v l) vs = do v' <- unfold' n v []
                              v2 <- vProj' n v' l
                              vApp' n v2 vs

unfold' n (EClos env (ECase e cbe)) vs = 
          do --traceM ("Case on " ++ ppDebug e)
             v <- eval env e
             --traceM ("i.e. case on "++ppDebug v)
             v' <- unfold' n v []
             --traceM ("i.e. case on unfolded "++ppDebug v')
             if isStopped v'
                   then do (m,_) <- splitStop v'
                           vStop m (eApp' (EClos env (ECase e cbe)) vs)
                   else case matchAll env v' cbe of
                               Just (env',e') -> do v2 <- eval' env' e' vs
                                                    unfold' n v2 []
                               Nothing -> return (eApp' (EClos env (ECase e cbe)) vs)

unfold' n  v@(EIf bv v1 v2) vs = do 
         bv' <- unfold' n bv []
         v' <- vIf' n bv' v1 v2
         vApp' n  v' vs         

                                       
unfold' _ v vs = return (eApp' v vs)


vProj' ::  Int -> Folded -> Id -> PCM Folded
vProj' k (EStop m v) n = 
          vStop m (EProj v n)
vProj' k v@(EClos env e@(EStruct _ _ xs cs _)) n =
     do c <- lookupProj n cs
        v <- vConst env c xs
        unfold' k v []

vProj' k v@(EClos env (Epackage _ _ xs cs _)) n = 
     do c <- lookupProj n cs
        v' <- vConst env c xs
        unfold' k v' []
vProj' _ v n = return $ EProj v n

vApp' :: Int -> Folded -> [(Bool,Value)] -> PCM Folded
vApp' k (EClos env e) vs = 
  do v <- eval' env e vs
     unfold' k v []

vApp' k (EApp h vs') vs = vApp' k h (vs'++vs)

vApp' _ v'@(EStop m v) vs = vStop m (eApp' v' vs)

vApp' _ v@(EVar _ _) vs = return (eApp' v vs)
vApp' _ v@(EProj _ _) vs = return (eApp' v vs)
vApp' _ v@(ECon _ _) vs = return (eApp' v vs)
vApp' _ v@(EConF _ _ _) vs = return (eApp' v vs)
vApp' _ v@(EIf _ _ _) vs = return (eApp' v vs)
vApp' _ v [] = return v
vApp' _ v vs = internalError ("vApp' "++ ppDebug v)


vIf' n  bv v1 v2 = let bv' = bv in --trace (ppDebug bv) bv in
         case bv' of 
             (ECon i [])  -> if i == trueId then unfold' n v1 [] else unfold' n v2 []
             (EConF i _ [])  -> if i == trueId then unfold' n v1 [] else unfold' n v2 []
             (ELiteral _ (LBool b)) -> if b then unfold' n v1 [] else unfold' n v2 []
             (EStop m v) -> vStop m (EIf bv v1 v2)
             _ -> return $ EIf bv v1 v2                    



isVCase :: Value -> Bool
isVCase (EClos _ (ECase _ _)) = True
isVCase (EApp (EClos _ (ECase _ _)) _) = True
isVCase _ = False

isVData :: Value -> Bool
isVData (EClos _ (EData _)) = True
isVData _ = False

isVIndData :: Value -> Bool
isVIndData (EApp (EClos _ (EIndData  _ _)) _) = True
isVIndData _ = False

isVDataOrInd :: Value -> Bool
isVDataOrInd v = isVData v || isVIndData v

isVSig :: Value -> Bool
isVSig (EClos _ (ESig _ _)) = True
isVSig _ = False

isVStruct :: Value -> Bool
isVStruct (EClos _ (EStruct _ _ _ _ _)) = True
isVStruct _ = False

isVPackage :: Value -> Bool
isVPackage EPackageType = True
isVPackage _ = False


isVpackage :: Value -> Bool
isVpackage (EClos _ (Epackage _ _ _ _ _)) = True
isVpackage _ = False

isVProd :: Value -> Bool
isVProd (EClos _ (EProd _ _)) = True
isVProd (EArrow _ _ _) = True
isVProd  _ = False


isMeta :: Exp -> Bool
isMeta (EClos _ (EMetaV _ _ _ _)) = True
isMeta (EApp (EClos _ (EMetaV _ _ _ _)) []) = True
isMeta (EStop _ e) = isMeta e
isMeta _ = False


vSort :: Position -> Sort -> PCM Value
vSort p s = return (ESort p s)


vVar :: UId -> PCM Value
vVar x = return $ EVar x Nothing

vConst :: Environment -> UId -> FCVars ->  PCM Value
vConst env x xs = return (EClos env (EConstV x xs))

vFun :: Environment -> Bind -> Exp  -> PCM Value
vFun rho b e = return (EClos rho (EAbs b e))


vData :: Environment -> [ConBind] -> PCM Value
vData env cb = return (EClos env (EData cb))

-- Until I remove EIndData from isProbLam, everything should work normally.
-- should I retrieveEnv?  To avoid repeated retrieveEnv, do I need EIndDataV?
vIndData :: Environment -> Tel -> [IndConBind] -> PCM Value
vIndData env  tel cb = return (EClos env (EIndData tel cb))

vStruct :: Environment -> Position -> [LetDef] ->  FCVars -> [(Id,UId)] -> [UId] -> PCM Value
vStruct env pos ds xs cs xs' = return (EClos env (EStruct pos ds xs cs xs'))

vPackage :: Environment -> Position -> [LetDef] ->  FCVars -> [(Id,UId)] -> [UId] -> PCM Value
vPackage env pos ds xs cs xs' = return (EClos env (Epackage pos ds xs cs xs'))



vMeta ::  Environment -> MetaVar  -> PCM Value
vMeta env m = do 
        xs <- getMetaFreeVars m
        pai <- isProblematic m
        mI <- getMetaInfo m
        let cit = transInfo mI
        return (EStop m (EClos env (EMetaV m pai xs cit)))

vStop :: MetaVar -> Value -> PCM Value
vStop m v@(EStop _ _ )  = return v
vStop m v = return (EStop m v)


lookupEnv :: UId -> Environment -> PCM Value
lookupEnv x env1@(E (env,sigma)) = lookupEnv' x env where
     lookupEnv' x env  =  maybe dflt_id evalMeta $ lookupE env x
       where dflt_id = vVar x

 

    {- Since we do retrieveEnv, which identify a const val at an
       env and the weakening of the const val at the def point,
       this defaulting to EVar x _is_ the right thing to do, it seems.
    This defaulting to (EVar x) had been used intentionally in at least
    1. in checkBranches, branch env are made by doing
          <new> <- compEnv <old> $ updateEnv emptyEnv x <cosntr val> 
       This can be avoided by doing
          <new> <- compEnv <old> $ updateEnv <old> x <cosntr val> 
       but less efficient.  (Or, use separate substEnv in place of compEnv.)
    2. in imitate,  v' <- eval emptyEnv v.  ??
    3. getContext:mkArgsBindVar does expand .. (EClos emptyEnv a)
    -}

                -- the order of eval as in the orig, but would it matter?
                -- yes, since we might have duplication, the update operation
                -- simply adds a new value
compEnv :: Environment -> Environment -> PCM Environment
compEnv r1@(E (env1,sigma)) r2@(E(env2,_)) = do
        env <- foldM comp env1 xs
        return (E(env, sigma))
  where xs = domEnv r1
        comp env x  = do 
          vx <- vVar x
          let v = maybe vx id (lookupE env1 x)
          v <- eval r2 v
          return$ update env x v 


updateEnvPatt :: Environment -> [PatArg] -> [Value] -> Environment
updateEnvPatt env [] [] =  env
updateEnvPatt env (p:pas) (v:vs) = let env' = updateEnvPatt env pas vs
                                       x = getUIdPatt p
                                   in updateEnv env' x v
--updateEnvTel env _ _ = raise "Wrong number of arguments in pattern" ??

retrieveEnv :: Environment -> FCVars -> PCM Environment
retrieveEnv env@(E(r,sigma)) = foldr f (return (E([],sigma))) where
    f x = liftM2 (flip(flip updateEnv x)) (lookupEnv x env)

--MT: for a sanity check, check length of pas and vs.
--    defunct ELiteral and CBLit are ignored.
matchAll :: Environment-> Value-> [(CaseBranch,Exp)]-> Maybe(Environment,Exp)
matchAll env v brs = case v of (ECon  c   vs)-> go c (map snd vs)
                               (EConF c _ vs)-> go c (map snd vs)
                               (ELiteral pos (LString l)) -> 
                                        matchAll env (forceListConstr pos l) brs 
                               (ELiteral pos (LBool l)) -> 
                                        matchAll env (forceBoolConstr pos l) brs 
                               
                               ( _          )-> Nothing
  where 
  go c vs = msum (map match brs) where
    match (CBConM c' pas _, bre)| c == c' = Just (brenv, bre) where
      brenv = updateEnvMany env (zipWith ((,).getUIdPatt) pas vs)
    match _                               = Nothing
  forceListConstr pos [] = ECon nilId []
  forceListConstr pos (c:cs) = ECon consId [(False,ELiteral pos (LChar c)), (False,ELiteral pos (LString cs))] 
  forceBoolConstr pos True = ECon trueId []
  forceBoolConstr pos False = ECon falseId []

isProbLam :: Value -> Bool
isProbLam e0 = case e0 of EStop    _ e     -> isProbLam e
                          EClos    _ e     -> isProbLam e
                          EDef     _ e     -> isProbLam e
                          EOpen  _ _ e     -> isProbLam e
                          EAbs     _ e     -> isProbLam e
                          EApp     h _     -> isProbLam h
                          ECase    _ _     -> True
                          EData    _       -> True
                          EIndData  _ _    -> True
                          ESig     _ _     -> True
                          EMeta  _ _ pai _ _ _ -> pai
                          EMetaV   _ pai _ _    -> pai
                          _                -> False


--isProbLam (EApp (EClos _ (ECase _ _)) _) = True
--isProbLam (EIndData _ _ _) = True
--isProbLam (EpackageV _ _ _ _ _) = True
--isProbLam (Epackage _ _ _) = True
--isProbLam (EStruct _ _ _) = True
--isProbLam (EStructV _ _ _ _ _ ) = True

appChoice :: Value -> Value -> Value
appChoice v1 v2 =  if isProbLam v1 then v2 else v1


--  || isStopped v1 isVCase v1 || isVData v1 || isVSig v1 ||

{- 'vApp v vs' computes the normal form of 'v vs'.  The principle case
is that 'v' is a closure, then it is reevaluated using.  The other
cases are neutral forms of 'v' and 'vApp' simply appends the arguments
'vs'. -}

vApp :: MValue -> [(Bool,MValue)] -> PCM MValue
vApp (EClos env e) vs = eval' env e vs
vApp (EApp h vs') vs = return (eApp' h (vs'++vs))
vApp v'@(EStop m v) vs = vStop m (eApp' v' vs)
vApp v@(EVar _ _) vs = return (eApp' v vs)
vApp v@(EProj _ _) vs = return (eApp' v vs)
vApp v@(ECon _ _) vs = return (eApp' v vs)
vApp v@(EConF _ _ _) vs = return (eApp' v vs)
vApp v@(EIf _ _ _) vs = return (eApp' v vs)
vApp v [] = return v
vApp v vs = internalError ("vApp "++ ppDebug v)

vProj :: MValue -> Id -> PCM MValue
vProj (EStop m v) n = vStop m (EProj v n)
vProj v@(EClos env (EStruct _ _ xs cs xs')) n =
                  do c <- lookupProj n cs
                     vConst env c xs
vProj v@(EClos env (Epackage _ _ xs cs xs')) n = 
                  do c <- lookupProj n cs
                     vConst env c xs

vProj  v n = return $ EProj v n


vIf bv v1 v2 = 
         case bv of 
             (ECon i [])  -> if (i == trueId) then return v1 else return v2
             (EConF i _ [])  -> if (i == trueId) then return v1 else return v2
             (ELiteral _ (LBool b)) -> if b then return v1 else return v2
             (EStop m v) -> vStop m (EIf bv v1 v2)
             _ -> return $ EIf bv v1 v2                    
                   


{-
vProjT :: Value -> Id -> PCM Value
vProjT (EStop m v) n = do ifM (isUninstantiated  m)
                              (vStop m (EProjT v n))
                              (do v2 <- evalMeta v
                                  vProjT v2 n)
vProjT (EClos env (EStruct _ xs cs)) n = do c <- lookupProj n cs
                                             a <- def c
                                             eval env (typeOfDef a)
vProjT v n = return (EProjT v n)
-}

lookupProj :: Id -> [(Id,UId)] -> PCM UId
lookupProj n st = maybe (passError ELookupProj) return (lookup n st) 






      


eval:: Environment -> Exp -> PCM Value
eval env e = eval' env e []
            {-
            do
                --traceM'$"eval{:(env,e)="++ppDebug (env,e)
                v <- eval' env e []
                --traceM'$"eval}:v="++ppDebug v
                return v
            -}
    
evalArgs env (h,e) = eval env e >>= \v -> return (h,v)

eval' env (EVar x _) vs          = do v <- lookupEnv x env
                                      vApp v vs


{- whether you treat case-bound vars as vars or consts,
doing retrieveEnv means that you confuses a const val at env
and the weaking of the corresponding const val at the definition point.
Hence, you have to say the correct thing for lookupEnv to do is
to default to identity when lookup fails.-}

eval' env (EConst c mcit) vs = do  d <- def c
                                   let --rhs = rhsOfDef d
                                       xs = varScopeDef d  --maybe [] varScope mcit --  -- tag bort ?
                                   env' <- retrieveEnv env xs
                                   v <- vConst env' c xs

                                   --v <- vConst env c xs

                                   --traceM (ppReadable (EApp (EConst c mcit) vs))
                                   --traceM (ppReadable xs)
                                   return (eApp' v vs)



eval' env ec@(EConstV c xs) vs     =  do v <- vConst env c xs
                                         return (eApp' v vs)

eval' _ (ESort p k)    _               = vSort p k

eval' env e@(EAbs _ _ )  []         = return (EClos env e)

eval' env e'@(EAbs (xs,a) e)  vs        = 
         do let (env',xs',vs') = updatesEnv env xs vs
            v <- eval' env' (eAbs [(xs',a)] e) vs'
            return v
               

eval' env e@(EProd _ _ )  _       = return ( EClos env e)

eval' env (EArrow  h e1 e2)  _       = do v1 <- eval env e1     
                                          v2 <- eval env e2
                                          return$ EArrow h v1 v2

eval' env (EApp h es) vs           = do 
                                        vs' <- mapM (evalArgs env) es
                                        eval' env h (vs'++vs)

eval' env ed@(EDef d e) vs            = eval' env e vs
 {-
 do tmp<- eval' env e vs
    return tmp
 -}

eval' env (ECon i es) vs           = do 
                                        vs' <- mapM (evalArgs env) es
                                        vApp (ECon i  vs') vs


eval' env (EConF i e es) vs        = do 
                                        vs' <- mapM (evalArgs env) es
                                        v <- eval env e
                                        vApp (EConF i v vs') vs





eval' env (EData cb) _             = vData env cb

eval' env (EIndData tel cb) vs  = do 
        v <- vIndData env tel cb
        return $ eApp' v vs




eval' env (ECase e cbe) vs 
          = do  v <- eval env e
                case v of
                   EStop m _ ->  vStop m (eApp' (EClos env (ECase e cbe)) vs)
                   _ -> do v <- unfold v
                           case matchAll env v cbe of
                               Just (env',e') -> do v2 <- eval' env' e' vs
                                                    return v2
                               Nothing -> return (eApp' (EClos env (ECase e cbe)) vs)

                                                       


eval' env (EProj e n) vs = do v <- eval env e
                              v' <- vProj v n
                              vApp v' vs

{-
eval' env (EProjT e n) _ = do v <- eval env e
                              vProjT v n
-}
eval' env e@(ESig _ _) _ = return (EClos env e)

eval' env EPackageType _ = return EPackageType   ---isn't this wrong (CC) 

eval' env v@(EStruct _ _ _ _ _) _ = return (EClos env v)
eval' env v@(Epackage _ _ _ _ _) _ = return (EClos env v)



eval' env (EOpen _ _ e) vs = eval' env e vs

eval' env (EBinOp e1 op e2) vs = eval' env (eApp' op [(False,e1),(False,e2)]) vs

eval' env e@(EIf b e1 e2) vs = do 
         bv <- eval env b
         v1 <- eval' env e1 vs
         v2 <- eval' env e2 vs
         --traceM $ "In if " ++ ppReadable e ++ " " ++ ppReadable bv ++ " " ++ ppReadable v1  ++ " " ++ ppReadable v2 ++ " \n"
         v <- vIf bv v1 v2
         --traceM ("got " ++ ppReadable v)
         vApp v vs

eval' env (EMeta m _ pai (cit,clenv)  _ _) vs  = ifM (isUninstantiated m) 
                                   (do vm <- vMeta env m
                                       vStop m (eApp' vm vs))
                                   (do
                                       ---traceM ("kom hit")
                                       e <- getMetaSubst m
                                       
                                       {- revived.  When trying the
                                       const way of case-bound, xs
                                       would be only reall free var. -}
                                       --let xs = varScope cit
                                       --traceM'$" eval' start ret:m="++show m++" xs="++ppDebug xs
                                       let xs =  varScope cit
                                       env' <- retrieveEnv env xs
                                       --traceM'$" eval' finish ret:m="++show m
                                       eval' env' e vs)
  

eval' env e@(EStop m v) vs            = 
                             do ifM (isUninstantiated m) 
                                    (vStop m =<< eval' env v vs)
                                    (do v' <- evalMeta e
                                        eval' env v' vs)

eval' env (EClos env' e) vs = do env2 <- compEnv env' env
                                 v <- eval' env2 e vs
                                 return v

eval' _ e@(ELiteral _ _) [] = return e                             
                                 
eval' env e vs = return (eApp' (EClos env e) vs)  -- take away, but check first for missing cases



splitMeta v = do v' <- unfold v
                 splitMeta' v'
              where splitMeta' (EStop _ e) = splitMeta' e
                    splitMeta' (EClos env (EMetaV m _ _ _)) = return (m,env)
                    splitMeta' (EApp (EClos env (EMetaV m _ _ _)) []) = return (m,env)
                    splitMeta' _ = passError ESplitMeta

{-
splitMeta2 v = do v' <- unfold v
                  splitMeta' v'
              where splitMeta' (EStop _ e) = splitMeta' e
                    splitMeta' (EClos env (EMetaV m _ xs)) = return (m,env,xs)
                    splitMeta' (EApp (EClos env (EMetaV m _ xs) []) = return (m,env,xs)
                    splitMeta' _ = passError ESplitMeta

-}


arityV :: Value ->  PCM Int
arityV v@(EClos env (EProd (xs,_) e)) = 
                                   do 
                                      vs <- genVals (map snd xs)
                                      let (env',ar) = arity' env xs vs
                                      v2 <- eval env' e
                                      ar' <- arityV v2
                                      return (ar+ar')
                where arity' env ((h,x):b) (v:vs) = 
                        let env' = updateEnv env x v
                            (env2,ar) = arity' env' b vs
                        in (env2,if h then ar else ar + 1)
                      arity' env _ _ = (env,0)
arityV (EArrow h v1 v2) = arityV v2 >>= \a -> (return$ if h then a else 1 + a)
arityV (EStop _ (EClos _ (EMetaV _ _ _ _))) = return 0 --passError ENoArity
arityV e = return 0


force :: Folded -> PCM Exp
force v@(EApp (EClos env (EConstV c xs)) vs) = 
                       do   d <- def c
                            let rhs = rhsOfDef d
                                abs = isAbstract d && c `notElem` (accessible env) 
                            case rhs of
                              DExp e -> if abs 
                                           then return v
                                           else do v' <- eval' env e vs
                                                   --traceM (ppDebug v')
                                                   return v'
                              _ -> return v
force v@(EClos env (EConstV c xs)) = 
                       do   d <- def c
                            let rhs = rhsOfDef d
                                abs = isAbstract d && c `notElem` (accessible env) 
                            case rhs of
                              DExp e -> if abs 
                                           then return v
                                           else do v' <- eval env e 
                                                   --traceM (ppDebug v')
                                                   return v'
                              _ -> return v

                                       
force v = return v



splitFun v = split =<< unfold v
  where split (EClos env (EProd d e))           = return (env,d,e)
        split (EArrow h v1 v2)  = eval emptyEnv (eArrow h v1 v2) >>= splitFun 
        split (EApp (EClos env (EProd d e)) []) = return (env,d,e)
        split _                                 = passError ESplitFun


splitStruct v = do v' <- unfold v
                   splitStruct' v'
             where splitStruct' (EClos env (EStruct pos _ xs cs cs')) = return (pos,env,xs,cs,cs')
                   splitStruct' _ = passError ESplitStruct


splitPackage v = do v' <- unfold v
                    v2 <- force v'
                    splitPackage' v2
             where splitPackage' (EClos env (Epackage pos _ xs cs cs')) = return (pos,env,xs,cs,cs')
                   splitPackage' _ = passError ESplitPackage

splitSig v = do v' <- unfold v
                v2 <- force v'
                splitSig' v2
             where splitSig' (EClos env (ESig pos tel)) = return (pos,env,tel)
                   splitSig' _ = passError ESplitSig


splitData:: Value -> PCM (Environment,[ConBind])
splitData v = do v' <- unfold v
                 v2 <- force v'
                 --traceM' ("splitData:v2="++ppDebug v2)
                 splitData'  v2
              where splitData' (EClos env (EData cbs))  = return (env,cbs)
                    splitData' _             = passError ESplitData


type IndSplit = ((Environment,  Tel, [IndConBind]), {- head -}
                 [(Bool,Value)]) {- args -}

splitDataOrInd :: Value
               -> PCM (Either (Environment,[ConBind]) IndSplit)

splitDataOrInd v = do
   v1 <- unfold v
   v2 <- force v1
   case v2 of
     (EApp (EClos env (EIndData tel cbs)) vs) ->
           return $ Right ((env,tel,cbs),vs)
     (EClos env (EIndData tel cbs))           ->
           return $ Right ((env,tel,cbs),[])
     (EClos env (EData cbs)) -> return $ Left (env, cbs)
     _                      -> passError ESplitData
        
{- 1. think of ways not to case whether #args = 0 or not. (not just here.)
   2. would look less silly when Data gets CtxInfo.
-}
indSplitToConBinds::IndSplit -> [ConBind]
indSplitToConBinds ((_,_,cbs),_) = map fst cbs

indSplitToIndHd::IndSplit -> Value
indSplitToIndHd ((tr,tel,cbs),_) = EClos tr (EIndData tel cbs)


splitCon v = do v' <- unfold v
                splitCon' v'
              where splitCon' (ECon c vs)  = return (c,vs)
                    splitCon' _             = passError ESplitCon

splitConF v = do v' <- unfold v
                 splitCon'  v'
              where splitCon' (EConF c v vs)  = return (c,v,vs)
                    splitCon' _             = passError ESplitCon


splitStop v = do v' <- evalMeta v
                 splitStop' v'
              where splitStop' (EStop m v) = return (m,v)
                    splitStop' _ = passError ESplitStop


getSort v = do v' <- unfold v
               getSort' v'
            where getSort' (ESort _ k) = return k
                  getSort' _ = passError EGetSort


getJudg :: Judgement a -> PCM (Judgement a)
getJudg (x :! v) = (x:!) `fmap` evalMeta v
getJudg (IsType x) = return (IsType x)



unfoldJudg :: Judgement a -> PCM (Judgement a)
unfoldJudg (x :! v) = do v' <- unfold v  
                         return (x :! v')
unfoldJudg (IsType x) = return (IsType x)




evalNative :: UId -> [(Bool,Value)] -> PCM Value
evalNative c vs = do head <- vConst emptyEnv c []
                     -- v <- vApp head vs
                     --traceM ("kom hit" ++ ppReadable c)
                     v' <- evalNativeLit head s vs
                     return v'
    where s = getUIdString c
          evalNativeLit:: Value -> String -> [(Bool,Value)] -> PCM Value  
          --evalNativeLit head "divSafe" [_,_,(_,(EStop m _))]  = vApp head vs >>= vStop m 
          --evalNativeLit head "divSafe" l  =  evalNativeLit head "div" (init l)
          evalNativeLit head s vs  = do v' <- vApp head vs 
                                        maybe (evalStops v' s vs) (evalLits v' s) (mapM expToLiteral values)
          evalLits :: Value -> String -> [Literal] -> PCM Value
          evalLits v s lits = return $ maybe v eLiteral (evalNativeOp s lits)
          evalStops :: Value -> String -> [(Bool,Value)] -> PCM Value
          evalStops v s vs = maybe (return v) (\m -> vStop m v) (stoppedBy values)
          values = map snd vs

