module BasicEngineOps(update,checkAndUpdate,solveCs) where 
import ISyntax     (MetaVar, Exp, mapJudg)
import Eval        (PCM, isUninstantiated, getMetaInfo, getJudg,
                    updateMetaSubst, delAllConstraints)
--import SimpleSolve (solveAllCsSafe)
import MetaVarState(MetaSubst)
import Position    (noPosition)
import Monads      (raise,traceM,done,handle_)
import Monad(unless,when)
import Error       (ErrMsg(EUninstantiated,ESolveNonTerm), eMsg )
import Utilities   (unlessM)
import MetaVarState(metaJudg,context,environment)
--import Equal((=?))
import MetaVarState(metaJudg,context,environment,pos)
import PPrint (ppReadable)
import ISynEnv (TCEnv,Judgement)
import SuggestSol(suggestSafeSols)
import Equal (simplifyCs,simplifyC)
import ProofMonad(getConstraints,getTermCounter)


give :: Bool -> (TCEnv -> (Judgement Exp) ->  PCM Exp) -> Bool -> MetaVar -> Exp -> PCM Exp
give  toTC typeCheck aut m e = do 
	mI <- getMetaInfo m
	unlessM (isUninstantiated m) (raise $ eMsg (pos mI) (EUninstantiated $ show m))

	e' <- if toTC then getJudg (metaJudg mI) >>= \j ->  (typeCheck (context mI,environment mI) (mapJudg (\_ -> e) j))     
                      else return e
        --traceM (ppReadable $ mapJudg (\_ -> e) j)
	updateMetaSubst aut m e'
        simplifyCs
	return e'


checkAndUpdate :: (TCEnv -> (Judgement Exp) ->  PCM Exp) -> Bool -> MetaVar -> Exp -> PCM Exp
checkAndUpdate typeCheck automatic m e = do 
       e' <- give True typeCheck automatic m e
       solveCs typeCheck
       return e'

update :: (TCEnv -> (Judgement Exp) ->  PCM Exp) -> Bool -> MetaVar -> Exp -> PCM ()
-- | Pre: the expression is type correct and has no meta variables in it,
-- which also means no hidden args.
update typeCheck automatic m e = do
    give False typeCheck automatic m e 
    solveCs typeCheck

solveCsRepLimited :: (TCEnv -> (Judgement Exp) ->  PCM Exp) -> Int -> PCM ()
-- Pre: all constraints are simplified 
-- Post:
solveCsRepLimited typeCheck limit  = solveCsLimit 0
   where solveCsLimit counter | counter == limit =  raise $ eMsg noPosition $ ESolveNonTerm limit
         solveCsLimit counter = do
             cs <- delAllConstraints
             sols <- suggestSafeSols cs
             rs <- mapM (either solveDirSol solveTCSol) sols
             --traceM ("solveCsRep "++ ppReadable sols)
             mapM simplifyC cs
             if (or rs) then solveCsLimit $ counter + 1
                        else done
         solveDirSol :: MetaSubst -> PCM Bool
         solveDirSol  (_,m,e) = do 
              isUninst <- isUninstantiated m
              if not isUninst then return False
                 else (give False typeCheck True m e 
                       >> return True) `handle_` return False

         solveTCSol (_,m,e) = do 
              isUninst <- isUninstantiated m
              if not isUninst then return False
                 else (give True typeCheck True m e >> return True) `handle_` return False


solveCs :: (TCEnv -> (Judgement Exp) ->  PCM Exp) ->  PCM ()
solveCs typeCheck = do
    limit <- getTermCounter
    solveCsRepLimited typeCheck limit