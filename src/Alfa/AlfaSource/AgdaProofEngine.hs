module AgdaProofEngine(module AgdaProofEngine,Agda.MetaVar) where
import qualified Agda
import qualified ConvToAgda as CVA
import qualified ConvFromAgda as U
--import qualified ForgetMetas(exp) -- unnecessary?
import SubstMeta(recordSubstMeta)
import AgdaPreserve

import UAbstract(ExpAnnot,Var(..),Sort(..),Exp(EMeta,ESort))
import EditMonad

import Fud(argReadKey)

--import Monad(ap)
import ListUtil(mapSnd)
import Maybe(mapMaybe)
import Utils2(apBoth)

#ifdef __HASKELL98__
#define map fmap
#endif

-- debug:
--import Debug2(trace)
tr x = --trace x $
       return ()


-- Proof Engine State:
type State =  (Agda.State,Options)

data Options = Opt { autoSolve :: Bool }
options0 = Opt True

-- Proof Engine Monad
type PE a = EdM State String a

liftPCM :: Agda.PCM a -> PE a
liftPCM (Agda.STM g) = liftStateEd fst upd (edM g')
  where
    g' s = Agda.convError (g s)
    upd s (_,opt) = (s,opt)

initState = (initState',options0) :: State
  where initState' = Agda.writeTermCounter termcounter Agda.initState

changeOption = liftStateEd snd upd . updateEd
  where
    upd opt (s,_) = (s,opt)

getOptions = map snd loadEd 

setAutoSolve on = changeOption $ \ opt -> opt { autoSolve=on }
getAutoSolve = map autoSolve getOptions

give g e =
  do 
     e' <- liftPCM $ CVA.exp e >>= giveme' g
     optSolveAllSafe
     --return [(g,U.exp e')] --!!
     metaSubst
  where
    giveme' g e =
      do tr ("giveme' "++show g++" "++Agda.ppDebug e++"...")
	 Agda.giveme g e
	 tr "done"

goal i g =
  do tr "goal"
     ctx0 <- liftPCM (Agda.metaContext g)
     t0 <- liftPCM (Agda.metaType i g) `handleEd` const (return dummyType)
     let ctx = [(Var s,U.exp e)|(s,Just e)<-ctx0]
         t = conv' t0
     -- Is this really necessary?
     ms <- metaSubst
     tr "goal returns"
     return (recordSubstMeta ms (ctx,t))
  where
    --conv (s,Nothing) = (Var s,ESort (Sort "Package"))
    --conv (s,Just e) = (Var s,U.exp e)
    conv' Nothing = ESort (Sort "Kind") -- hmm
    conv' (Just e) = U.exp e

    dummyType = Just (Agda.CMeta Agda.noPosition False (Just False) Agda.preMetaVar)

--compute Nothing  _ = errorEd "Can't compute in top level env in this version."
compute optg e =
 do e' <- liftPCM $ CVA.exp e >>= Agda.compute optg
    ms <- metaSubst
    return $ recordSubstMeta ms $ U.exp e'
    


--typeOf Nothing _ =  errorEd "Can't type check in top level env in this version."
typeOf optg e = map U.exp (liftPCM $ CVA.exp e >>= Agda.typeOf optg)

appendDecls uds =
    do ads1 <- liftPCM $ CVA.decls uds
       --tr ("addDefs:\n"++Agda.ppDebug ds1)
       ads' <- liftPCM $ Agda.addDefs' ads1
       optSolveAllSafe
       let ads'' = preserve ads1 ads'
       ms <- metaSubst
       return (U.decls ads'',ms) -- apply ms to ds' before returning?!


checkTermination uds =
    do ads <- liftPCM $ CVA.decls' uds
       t <- liftPCM $ Agda.checkCorrDefs ads
       case Agda.nontermError t of
	 Nothing -> return ()
	 Just e -> errorEd e

--genMetas n = return (replicate n Agda.preMetaVar) --!!!
--genMeta = return Agda.preMetaVar --!!!

constraints = map (apBoth U.exp . ceq) `map` liftPCM Agda.constraints
  where ceq (Agda.CEq e1 e2) = (e1,e2)

metaSubst = map (mapSnd U.exp) (loadEd >>= liftPCM . Agda.extractMetaSubst . fst)
metaVars = map (Agda.extractMetaVars . fst) loadEd

intro g =
  do --s<-loadEd
     es<-liftPCM (Agda.intros g)
     --storeEd s
     return (map (U.exp . Agda.forgetMetaVars) es)

refine      = refine' Agda.refine
refineExact = refine' Agda.refineExact

refine' agdaRefine g re =
  do s       <- loadEd
     re'     <- liftPCM (CVA.exp re)
     tr "before Agda.refine"
     ({-_ms,-}e) <- liftPCM (agdaRefine g re')
     tr "after Agda.refine"
     --ms <- metaSubst
     storeEd s
     return ({-recordSubstMeta ms-} (U.exp e))
  
case' g e = preview (CVA.exp e >>= Agda.makeCase g)
open  g e = preview (CVA.exp e >>= Agda.makeOpen g)
openTLhack e = previewDecl (CVA.exp e >>= Agda.makeOpenTL)

{-
solveConstraint tactic cnr =
  do ms <- liftPCM $ Agda.solveCN tactic cnr
     ms2 <- liftPCM $ solveAllSafe
     return (mapSnd U.exp (ms2++ms)) -- appending substitutions?!
-}

suggestSolution =
  do ms <- liftPCM Agda.suggestSolution
     return (mapSnd U.exp ms)

preview = preview' U.exp
previewDecl = preview' U.decl

preview' fromAgda opPCM =
  do s <- loadEd
     ({-_ms,-}e) <- liftPCM opPCM
     storeEd s
     return (fromAgda (Agda.forgetMetaVars e))

optSolveAllSafe =
  do on <- getAutoSolve
     if on then solveAllSafe else return ()

solveAllSafe = liftPCM $ Agda.solveAllCsSafeRep solvecnt

setTerminationCounter = liftPCM . Agda.setTerminationCounter

--unfoldN n Nothing e = errorEd "Can't unfold in top level env in this version."
unfoldN n optm e = U.exp `map` liftPCM (CVA.exp e >>= Agda.unfoldN n optm)

contUnfold1 = U.exp `map` liftPCM Agda.contUnfold1

--
solvecnt = argReadKey "solvecnt" 30 :: Int
termcounter = argReadKey "termcounter" Agda.defaultCounter
