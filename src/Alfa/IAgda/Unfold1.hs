{-| For stepping the computation, does not reduce an expression that is
    on head-normal form. 
-} 

module Unfold1(unfold1) where
import Eval
import Monads
import ISyntax

unfold1 :: Value -> PCM Value
unfold1 v = do v' <- evalMeta v
               v2 <- unfold1' v' []
               --traceM ("Unfold "++ ppReadable v)
               --traceM("Via "++ppReadable v')
               --traceM ("Getting"++ppReadable v2)
               return v2

unfold1' :: Value -> [(Bool,Value)] -> PCM Folded
unfold1' (EApp v vs) vs' = unfold1' v (vs++vs')
unfold1' v@(EClos env (EConstV c xs)) vs = 
          do d <- def c
             let rhs = rhsOfDef d
             case rhs of
                 DExp e -> eval' env e vs
                              --traceM ("Unfolding "++ppDebug v')
                              --traceM ("Getting "++ppDebug v2)
                 PN -> return (eApp' v vs)
                 Native -> evalNative c vs 
unfold1' (EClos env (ECase e cbe)) vs = 
          do v <- eval env e
             v' <- unfold1' v []
             if isStopped v'
                   then do (m,_) <- splitStop v'
                           vStop m (eApp' (EClos env (ECase e cbe)) vs)
                   else case matchAll env v' cbe of
                               Just (env',e') -> eval' env' e' vs
                               Nothing -> return (eApp' (EClos env (ECase v' cbe)) vs)

                                       

unfold1' (EProj v n) vs = do v' <- unfold1' v []
                             v2 <- vProj v' n
                             vApp1' v2 vs 
unfold1' (EIf b v1 v2) vs = do bv <- unfold1' b []
                               v <- vIf bv v1 v2
                               vApp1' v vs

unfold1' v vs = return (eApp' v vs)





vApp1' :: Folded -> [(Bool,Value)] -> PCM Folded
vApp1' (EClos env e) vs = 
  eval' env e vs

vApp1' (EApp h vs') vs = vApp1' h (vs'++vs)
vApp1' v'@(EStop m v) vs = vStop m (eApp' v' vs)
vApp1' v@(EVar _ _) vs = return (eApp' v vs)
vApp1' v@(EProj _ _) vs = return (eApp' v vs)
vApp1' v@(EIf _ _ _) vs = return (eApp' v vs)
vApp1' v@(ECon _ _) vs = return (eApp' v vs)
vApp1' v@(EConF _ _ _) vs = return (eApp' v vs)
vApp1' v [] = return v
vApp1' v vs = internalError ("vApp1' ")


