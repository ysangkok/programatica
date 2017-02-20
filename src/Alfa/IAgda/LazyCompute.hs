module LazyCompute (whnfL) where
import ISyntax
import ProofState
import ProofMonad
--import CITranslate(makeUId)
import Monads
import PPrint(ppDebug,ppReadable)
import Position(Position,noPosition)
import CITrans(varScope,freeVarScope)
import Literal
import FString 
import Error
import qualified AltIntMap as I
import AgdaTrace
import qualified Eval
import Utilities (mapPair)
import FiniteMap
import MiscId
import Native

type Folded = Value


whnfL :: Exp -> Environment -> PCM Value
whnfL e env = do v <- Eval.eval env e
                 traceM ("eval " ++ ppReadable e ++ " got " ++ show v)
                 unfold v

unfold :: Value -> PCM Folded
unfold v = do ds <- getDefState 
              return $ unfold' ds v []


unfold' :: DefState -> MValue -> [(Bool,Value)] -> Folded   -- tag bort listan av args
-- unfold' ds 0 v vs = passError $ (ETermProblem (ppReadable (eApp' v vs)))
unfold' ds (ECon i vs) vs' =  let vs2 = map (mapPair id  (\v -> unfold' ds v [])) vs
                              in ECon i (vs2++vs')
unfold' ds (EConF i v vs) vs' =  let vs2 = map (mapPair id  (\v -> unfold' ds v [])) vs
                                 in EConF i v (vs2++vs')
unfold' ds (EApp v vs) vs' = 
               let vs2 = map (mapPair id  (\v -> unfold' ds v [])) vs
               in unfold' ds v (vs2++vs')

unfold' ds v@(EClos env (EConstV c _)) vs = 
      case I.ilookup (getUIdNo c) ds of
           Just d -> 
             case rhsOfDef d of
                 DExp e -> let v' = eval' env e vs

                           
                           in trace("unfold " ++ show e ++ show vs ++ " got " ++ show v' ++ "\n") unfold' ds v' []
                 PN -> eApp' v vs
                 Native -> evalNative c vs
unfold' ds (EProj v l) vs = 
                           let v' = unfold' ds v []
                               v2 = vProj' ds v' l
                           in vApp' ds v2 vs

unfold' ds (EClos env (ECase e cbe)) vs = 
          let v = eval env e
              v' = unfold' ds v []
          in case matchAll env v' cbe of
                   Just (env',e') -> let v2 = eval' env' e' vs
                                     in unfold' ds v2 []
                   Nothing -> vApp' ds (EClos env (ECase e cbe)) vs

unfold' ds (EIf b e1 e2) vs = let 
         bv' =  unfold' ds b []
         v1 = unfold' ds e1 []
         v2 = unfold' ds e2 []
         v = vIf bv' v1 v2
       in vApp' ds v vs         

                                       
unfold' ds v vs = eApp' v vs


vProj' ::  DefState -> Folded -> Id -> Folded
vProj' ds (EStop m v) n = 
          EStop m (EProj v n)
vProj' ds v@(EClos env (EStruct _ _ xs cs xs')) n =
     let c = lookupProj n cs
         v = EClos env (EConstV c xs)
     in unfold' ds v []

vProj' ds v@(EClos env (Epackage _ _ xs cs xs')) n = 
     let c = lookupProj n cs
         v' = EClos env (EConstV c xs)
     in unfold' ds v' []
vProj' ds v n = EProj v n

vApp' :: DefState -> Folded -> [(Bool,MValue)] -> Folded
vApp' ds (EClos env e) vs = 
  let v = eval' env e vs
  in unfold' ds v []

vApp' ds (EApp h vs') vs = vApp' ds h (vs'++vs)

vApp' ds v'@(EStop m v) vs = EStop m (eApp' v' vs)

vApp' ds v@(EVar _ _) vs =  (eApp' v vs)
vApp' ds v@(EProj _ _) vs =  (eApp' v vs)
vApp' ds v@(ECon _ _) vs =  (eApp' v vs)
vApp' ds v@(EConF _ _ _) vs =  (eApp' v vs)
vApp' ds v [] =  v
vApp' ds v@(EIf _ _ _) vs =  (eApp' v vs)
vApp' ds v vs = undefined



lookupEnv :: UId -> Environment -> Value
lookupEnv x env1@(E (env,sigma)) = lookupEnv' x env where
     lookupEnv' x env  =  maybe (EVar x Nothing) id $ lookupE env x
                -- simply adds a new value

compEnv :: Environment -> Environment -> Environment
compEnv r1@(E (env1,sigma)) r2@(E(env2,_)) = 
        let env = foldl comp env1 xs
        in E(env, sigma)
  where xs = domEnv r1
        comp env x  = 
          let v = maybe (EVar x Nothing) id (lookupE env1 x)
              v' = eval r2 v
          in update env x v'



updateEnvPatt :: Environment -> [PatArg] -> [Value] -> Environment
updateEnvPatt env [] [] =  env
updateEnvPatt env (p:pas) (v:vs) = let env' = updateEnvPatt env pas vs
                                       x = getUIdPatt p
                                   in updateEnv env' x v
--updateEnvTel env _ _ = raise "Wrong number of arguments in pattern" ??



matchAll :: Environment -> Value -> [(CaseBranch,Exp)] -> Maybe (Environment,Exp)
matchAll env v [] = Nothing
matchAll env v ((cb,e):cbs) = case match env v cb of 
                                    Just env'  -> Just (env',e)
                                    Nothing -> matchAll env v cbs



   
match :: Environment -> Value -> CaseBranch -> Maybe Environment
--match env (ECon c vs) (CBCon c' pas)
match env (ECon c vs) (CBConM c' pas _)
       |  c == c' = Just (updateEnvPatt env pas (map snd vs))
       |  otherwise  = Nothing
--match env (EConF c _ vs) (CBCon c' pas)
match env (EConF c _ vs) (CBConM c' pas _)
       |  c == c' = Just (updateEnvPatt env pas (map snd vs))
       |  otherwise  = Nothing
match env (ELiteral pos (LString l) ) br = 
       match env (forceListConstr pos l) br 
match env (ELiteral pos (LBool l)) br =
       match env (forceBoolConstr pos l) br 

match env (ELiteral _ l) (CBLit _ l')  
       | l == l' = Just env
       | otherwise = Nothing
match _ e _ = Nothing

forceListConstr pos [] = ECon nilId []
forceListConstr pos (c:cs) = ECon consId [(False,ELiteral pos (LChar c)), (False,ELiteral pos (LString cs))] 
forceBoolConstr pos True = ECon trueId []
forceBoolConstr pos False = ECon falseId []



vApp :: MValue -> [(Bool,MValue)] -> MValue
vApp (EClos env e) vs = eval' env e vs
vApp (EApp h vs') vs =  (eApp' h (vs'++vs))   -- borde vara eApp
vApp v'@(EStop m v) vs = EStop m (eApp' v' vs)
vApp v@(EVar _ _) vs =  (eApp' v vs)
vApp v@(EProj _ _) vs =  (eApp' v vs)
vApp v@(ECon _ _) vs =  (eApp' v vs)
vApp v@(EConF _ _ _) vs =  (eApp' v vs)
vApp v@(EIf _ _ _) vs =  (eApp' v vs)
vApp v [] =  v
vApp v vs = undefined

vProj :: MValue -> Id -> MValue
vProj (EStop m v) n = EStop m (EProj v n)
vProj v@(EClos env (EStruct _ _ xs cs xs')) n =
                  let c = lookupProj n cs
                  in  EClos env (EConstV c xs)
vProj v@(EClos env (Epackage _ _ xs cs xs')) n = 
                  let c = lookupProj n cs
                  in EClos env (EConstV c xs)

vProj  v n =  EProj v n


vIf bv v1 v2 = 
         case bv of 
             (ECon i [])  -> if (i == trueId) then  v1 else  v2
             (EConF i _ [])  -> if (i == trueId) then  v1 else  v2
             (ELiteral _ (LBool b)) -> if b then  v1 else  v2
             (EStop m v) -> EStop m (EIf bv v1 v2)
             _ ->  EIf bv v1 v2                    
                   


lookupProj :: Id -> [(Id,UId)] -> UId
lookupProj n st = maybe undefined id (lookup n st) 


      



eval env e =  eval' env e []

eval' :: Environment -> Exp -> [(Bool,Value)] -> Exp

eval' env (EVar x _) vs          = let  v = lookupEnv x env
                                   in  vApp v vs
                                       

eval' env (EConst c _) vs  =  let v = EClos env (EConstV c [])
                                   --traceM (ppReadable (EApp (EConst c) vs))
                                   --traceM (ppReadable v)
                               in eApp' v vs


eval' env ec@(EConstV c xs) vs     =  let v = EClos env (EConstV c xs)
                                      in (eApp' v vs)

eval' _ (ESort p k)    _               = ESort p k

eval' env e@(EAbs _ _ )  []         =  (EClos env e)

eval' env e'@(EAbs (xs,a) e)  vs        = 
         let (env',xs',vs') = updatesEnv env xs vs
         in eval' env' (eAbs [(xs',a)] e) vs'
             
               


eval' env e@(EProd _ _ )  _       =  ( EClos env e)

eval' env e@(EArrow h a b)  _       =  EArrow h (eval env a) (eval env b)


eval' env e@(EApp h es) vs           = let (hs,es') = unzip vs
                                           vs' = map (eval env) es'
                                       in trace("In eval " ++ show h ++ show ((zip hs vs')++vs)) (eval' env h ((zip hs vs')++vs))


eval' env (EDef d e) vs            = eval' env e vs

eval' env (ECon i es) vs           = let -- e' = eval env e
                                         --traceM "kom hit"
                                         --traceM (ppReadable e)
                                         --traceM (ppReadable e')
                                         (hs,es') = unzip es
                                         vs' = map (eval env) es'
                                     in vApp (ECon i (zip hs vs')) vs

eval' env (EConF i e es) vs         = let (hs,es') = unzip es
                                          e' = eval env e
                                          vs' = map (eval env) es'
                                       in vApp (EConF i e' (zip hs vs')) vs



eval' env (EData cb) _             = EClos env (EData cb)

eval' env (EIndData tel cb) vs  =
   eApp' (EClos env (EIndData tel cb)) vs

eval' env (ECase e cbe) vs = 
            case eval env e of
                   EStop m _ ->  EStop m (eApp' (EClos env (ECase e cbe)) vs)
                   v -> case matchAll env v cbe of
                               Just (env',e') -> eval' env' e' vs

                               Nothing -> eApp' (EClos env (ECase e cbe)) vs

                                                       


eval' env (EProj e n) vs = let v = eval env e
                               v' = vProj v n
                           in vApp v' vs


eval' env e@(ESig _ _) _ =  (EClos env e)

eval' env EPackageType _ =  EPackageType

eval' env v@(EStruct _ _ _ _ _) _ =  (EClos env v)
eval' env v@(Epackage _ _ _ _ _) _ =  (EClos env v)



eval' env (EOpen _ _ e) vs = eval' env e vs

eval' env (EBinOp e1 op e2) vs = eval' env op ([(False,e1),(False,e2)] ++ vs)

eval' env (EMeta m _ pai cit _ _) vs  = 
                let vm = EStop m (EClos env (EMetaV m pai [] cit))
                in EStop m (eApp' vm vs)
      -- This doesn't work if the meta-variable has been filled in later
      

 
eval' env e@(EStop m v) vs            = vApp e vs

eval' env (EIf b e1 e2) vs = let 
                              vb = eval' env b []
                              v1 = eval' env e1 []
                              v2 = eval' env e2 []
                              v = vIf vb v1 v2
                           in vApp v vs


eval' env (EClos env' e) vs = let env2 = compEnv env' env
                              in eval' env2 e vs
                       
--eval' env (EUndef e) vs = eval' env e vs                                 
--eval' env (EDefin e) vs = eval' env e vs                                
eval'  _ e _ =  e


evalNative :: UId -> [(Bool,Value)] -> Value
evalNative c vs = 
     maybe v (\ls -> maybe v id (evalNative' ls)) (allLiterals (map snd vs))
     where allLiterals :: [Value] -> Maybe [Literal]
           allLiterals vs = mapM expToLiteral vs
           evalNative' :: [Literal] -> Maybe Value
           evalNative' ls = do l <- evalNativeOp sc ls
                               return (eLiteral l) 
           sc :: String
           sc = getUIdString c
           v :: Value
           v = let head =  EClos emptyEnv (EConstV c [])
               in vApp head vs
