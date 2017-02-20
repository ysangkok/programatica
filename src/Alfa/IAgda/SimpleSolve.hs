{- |
Implements a solve mechanism for constraints that doesn't 
need typechecking.

-}


module SimpleSolve(solveSafe,solveAllCsSafeRep,trySolveSafe) where 

import ISynType(Environment,Value,appnormalized,Exp(..))
import MetaVarState(Constraint(..),MetaSubst,transInfo)
import Monads(raise,handle,handle_,passError,traceM)
import CSyntax(CExpr)
import CITrans
import Position (noPosition)
import Error
import SimpleICTranslate
import Id(isDummyUId)
import Eval(PCM, isUninstantiated, getMetaInfo, getConstraints,
            updateMetaSubst,lookupEnv,unfold)
import ValueToExp(valueToExp)
import MetaVars
import PPrint(ppReadable)


type Solution a = [(MetaVar,a)] 

getSolve :: Constraint -> PCM ((MetaVar,Environment),Value)
getSolve (Constraint (EStop m (EClos env (EMetaV _ _ _))) v) = return ((m,env),v)
getSolve (Constraint v (EStop m (EClos env (EMetaV _ _ _))) ) = return ((m,env),v)
getSolve _ =  passError ENoSolve





{- Adds metas to then end of the expression, e, if the expected type is not a 
   product. This could maybe be improved by adding n meta-variables, where n
   is the the difference in leading hidden bindings in the types.  
-}


solveSafe :: Constraint -> PCM MetaSubst
solveSafe c = do
    ((m, envc), vc)        <- getSolve c
    --traceM $ "Try solve " ++ show m ++ " = " ++ ppReadable vc
    v <- getMetaInfo m
    let   cit = transInfo v
          xs = freeVarScope $ fst cit
          -- buildMap      :: PCM [(UId,UId)] 
          buildMap = mapM bM  xs
          bM x              = lookupEnv x  envc >>= ifxIs where
              ifxIs (EVar  y _) = return (x,y)
              ifxIs _         = noSolve
          solveV'   =  do e <- valueToExp False True cit vc
                          updateMetaSubst m e
                          --traceM ("SSolve "++ppReadable (m,e))
                          return (True,m,e) 
    yxs <- buildMap            --- v hmm
    let safe = all(\ (y,x)-> isDummyUId x || x == y) yxs
    if safe then solveV' else noSolve
  where noSolve = passError ENoSolve



trySolveSafe :: (Value,Value) -> PCM (Maybe MetaSubst)
trySolveSafe (v1,v2) = (solveSafe (Constraint v1 v2)>>= return.Just) `handle_` (return Nothing)

trySolveSafeCs :: PCM (Maybe (Bool,MetaVar,Exp))
trySolveSafeCs = do cs <- getConstraints  
                    mes <- foldr try (return Nothing) cs
                    return mes
                where try :: Constraint -> PCM (Maybe (Bool,MetaVar,Exp)) -> PCM (Maybe (Bool,MetaVar,Exp))
                      try c rest = (solveSafe c >>= return.Just) `handle_` rest
                        

solveAllCsSafeRep :: Int -> PCM [(Bool,MetaVar,Exp)]
solveAllCsSafeRep i = solveAllCsSafeRep' [] 0
         where solveAllCsSafeRep' sls c
                   | c > i = raise $ eMsg noPosition (ESolveNonTerm i)
                   | otherwise = 
                      do r <- trySolveSafeCs
                         case r of
                            Nothing -> return sls
                            Just (aut,m,e) -> solveAllCsSafeRep' ((aut,m,e):sls) (c+1)
