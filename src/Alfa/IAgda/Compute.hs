{-| 
  Compute a weak head normal form 'whnfS'.
  Builds on Eval.
-}
module Compute(whnfS,unfold)  where

import ISyntax
import ProofMonad
--import CITranslate(makeUId)
import Monads
import PPrint(ppDebug,ppReadable)
import Position(Position,noPosition)
import CITrans(varScope,freeVarScope)
import Literal
import FString 
import Error
import Eval (Folded,eval,eval',evalMeta,matchAll,vStop,vConst,lookupProj,evalNative,vIf)
import Utilities(mapPairM)
import Native

whnfS :: Exp -> Environment -> PCM Value
whnfS e env = do v <- eval env e
                 unfold v

unfold :: Value -> PCM Folded
unfold v = do n <- getTermCounter
              unfoldCount n v `handle` termError v
          where termError v e = 
                  case e of
                      (_,EPass (ETermProblem t)) -> 
                             raise $ eMsg (getExpPos v) (ETermination (ppReadable v) t)
                      _ -> raise e

unfoldCount :: Int -> Value -> PCM Folded
unfoldCount 0 v = passError $ (ETermProblem (ppReadable v))
unfoldCount n v = 
           do --v' <- evalMeta v
              v2 <- unfold' n v []
              return v2

unfold' :: Int -> MValue -> [(Bool,Value)] -> PCM Folded  
unfold' 0 v vs = passError $ (ETermProblem (ppReadable (eApp' v vs)))
unfold' n (EApp v vs) vs' = 
                            do vs2 <- mapM (mapPairM return (\v -> unfold' n v [])) vs
                               unfold' n v (vs2++vs')
unfold' n v@(EClos env (EConstV c _)) vs = 
          do d <- def c
             let rhs = rhsOfDef d
             case rhs of
                 DExp e -> do v' <- eval' env e vs
                              unfold' (n-1) v' []
                 PN -> return (eApp' v vs)
                 Native -> evalNative c vs


unfold' n (EProj v l) vs = 
                           do v' <- unfold' n v []
                              v2 <- vProj' n v' l
                              vApp' n v2 vs

unfold' n (EClos env (ECase e cbe)) vs = 
          do v <- eval env e
             v' <- unfold' n v []
             case matchAll env v' cbe of
                               Just (env',e') -> do v2 <- eval' env' e' vs
                                                    unfold' n v2 []
                               Nothing -> return (eApp' (EClos env (ECase e cbe)) vs)
unfold' n (EIf be e1 e2) vs = do bv <- unfold' n be []
                                 v1 <- unfold' n e1 []
                                 v2 <- unfold' n e2 []
                                 v <- vIf bv v1 v2
                                 vApp' n v vs

unfold' n (ECon c es) vs = do vs2 <- mapM (mapPairM return (\v -> unfold' n v [])) es
                              return $ ECon c (vs2++vs)
unfold' n (EConF c e es) vs = do vs2 <- mapM (mapPairM return (\v -> unfold' n v [])) es
                                 return $ EConF c e (vs2++vs)


unfold' n v vs = return (eApp' v vs)


vProj' ::  Int -> Folded -> Id -> PCM Folded
vProj' k (EStop m v) n = 
          vStop m (EProj v n)
vProj' k v@(EClos env (EStruct _ _ xs cs xs')) n =
     do c <- lookupProj n cs
        v <- vConst env c xs
        unfold' k v []

vProj' k v@(EClos env (Epackage _ _ xs cs xs')) n = 
     do c <- lookupProj n cs
        v' <- vConst env c xs
        unfold' k v' []
vProj' k v n = return $ EProj v n

vApp' :: Int -> Folded -> [(Bool,MValue)] -> PCM Folded
vApp' n (EClos env e) vs = 
  do v <- eval' env e vs
     unfold' n v []

vApp' n (EApp h vs') vs = vApp' n h (vs'++vs)

vApp' n v'@(EStop m v) vs = vStop m (eApp' v' vs)

vApp' n v@(EVar _ _) vs = return (eApp' v vs)
vApp' n v@(EProj _ _) vs = return (eApp' v vs)
vApp' n v@(ECon _ _) vs = return (eApp' v vs)
vApp' n v@(EConF _ _ _) vs = return (eApp' v vs)
vApp' n v@(EIf _ _ _) vs = return (eApp' v vs)
vApp' n v [] = return v
vApp' n v vs = internalError ("vApp' "++ ppDebug v)
