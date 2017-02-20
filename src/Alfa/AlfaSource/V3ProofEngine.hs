module V3ProofEngine where
import qualified V3
import qualified ConvToV3 as CV3
import qualified ConvFromV3 as U
import qualified ForgetMetas(exp)
import SubstMeta(expSubstMeta)

import UAbstract(ExpAnnot,Exp(EMeta))
import EditMonad

import ListUtil(mapSnd)
--import Utils(apBoth)

-- Proof Engine State:
type State =  (V3.CheckState (),V3.ProofState ())

-- Proof Engine Monad
type PE a = EdM State String a

liftG :: V3.G () a -> PE a
liftG g = liftStateEd snd (\ ps (cs,_) -> (cs,ps)) (edM g)
                          -- flip ((,) . fst)

initState = (V3.nilCheck,V3.nilProof) :: State

give g e = liftG (CV3.exp e `V3.bindG` V3.giveG g) >> metaSubst

appendDecls ds =
    do ds' <- liftG (CV3.decls ds)
       addDecls ds'
       ms <- metaSubst
       return (U.decls ds',ms)
  where addDecls ds = do s <- loadEd
                         s' <- liftEitherEd (steps ds s)
			 storeEd s'

genMetas n = do es <- liftG (V3.genMetasG n); return [g | V3.EMeta g<-es]
genMeta = do ~(V3.EMeta g) <-liftG V3.genMetaG; return g

--topLevelContext = return [] -- !!

constraints = 
  do cs0 <- liftG V3.getProofConstraintsG
     ((_,subst,_),_) <- loadEd
     liftG (V3.listG (convConstr subst) cs0)
  where
    convConstr subst (_,e1,e2) =
        (,) `CV3.mapG` expWalG e1 `CV3.apG` expWalG e2
      where expWalG = CV3.mapG U.exp . V3.expValG subst . V3.valWal

metaSubst = mapSnd U.exp `map` liftG V3.getProofMetaSubstG

intro g =
  do (cs,tval) <- getGoal g
     twhnf <- liftG (V3.whnf1G cs tval)
     s <- loadEd
     case twhnf of
       V3.VClos _ (V3.ESum constrs) ->
	   return [ e | Right (e,_)<-map tryCon constrs]
         where tryCon = flip runEd s . refine g . U.name . fst
       _ -> caseEd (preview g (V3.introG g))
	           (return . (:[]))
		   (const (return []))

refine g n = preview g (V3.refineG g (CV3.name n))

case' g e = preview g (CV3.exp e `V3.bindG` V3.tacCaseG g)

preview g opG =
  do s <- loadEd
     liftG opG
     ms <- liftG V3.getProofMetaSubstG
     e <- liftG (V3.lookupG ms g)
     storeEd s
     return (ForgetMetas.exp (U.exp e))

goal g =
  do ((env0,subst,_),val) <- getGoal g
     env1 <- liftG (listEnvG env0)
     ms <- metaSubst
     env <- mapM (convBindG ms subst) env1
     t <- liftG $ V3.expValG subst val
     return (env,convExp ms t)
  where
    convExp ms = expSubstMeta ms . U.exp
    convBindG ms subst (x,val) =
      do e <- liftG $ V3.expValG subst val
         return (U.name x,convExp ms e)

compute optMeta e =
  do cs <- checkstate optMeta
     v <- liftG (CV3.exp e `V3.bindG` V3.hnfG cs)
     return (U.exp v)

typeOf optMeta e =
  do cs <- checkstate optMeta
     t <- liftG $ CV3.exp e `V3.bindG` typeofG cs
     return (U.exp t)

-----

--listEnvG is a combination of V3/Abstract.namesEnv and V3/Env.lookupEnvTypeG
--listEnvG :: V3.Env a -> G [(V3.Name,V3.Val a)]

checkstate optMeta =
  case optMeta of
  Just g -> map fst (getGoal g)
  Nothing -> map fst loadEd

--getGoal :: MetaVar -> PE (CheckState, Val)
getGoal g = liftG (V3.getProofGoalsG `V3.bindG` flip V3.lookupG g)

listEnvG env =
  case env of
    V3.NilEnv -> V3.unitG []
    V3.Update env1 _ _ -> listEnvG env1
    V3.UpdateCase env1 _ -> listEnvG env1
    V3.UpdateType env1 x _ v -> CV3.mapG ((x,v):) (listEnvG env1)
    V3.UpdateRec env1 defs  ->
      CV3.mapG ([(x,V3.VClos env (V3.piExp ctx t)) |
                   (x,(ctx,_,t))<-V3.defsDecl defs]++)
               (listEnvG env1)
    V3.Call env1 _ -> listEnvG env1

typeofG cs e =
 V3.inferExpG cs e `V3.bindG` \ v ->
 V3.expValG (V3.substCheck cs) v


--steps :: [Decl] -> State -> E State
steps [] cp = V3.unitE cp
steps (d:ds) cp =
    step d cp `V3.bindE` \ (c1,p1) -> 
    V3.unifG p1 `V3.bindE` \ (_,p2) -> 
    steps ds (c1,p2)
  where
    --step :: Decl -> State -> E State
    step d (c,p) =
      V3.checkDeclG c d p `V3.bindE` \ (_,p1) ->
      V3.unitE (V3.addDecl c d,p1)
