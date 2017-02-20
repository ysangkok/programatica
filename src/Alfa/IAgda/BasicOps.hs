{-| 
  Interface to the whole functionality of Agda.  

  Both emacsagda and Alfa operate through this interface.

  Defines abbreviated names for useful combinations of functions
  from many other modules.

TODO: 
* Document invariants, precoditions etc. for these operations
* Example: check that all meta variables are unique
* See BasicOps_Properties.hs for a start of this work
-}

module BasicOps ( auto, giveme, refineExact, refine, refinePrj, abstract,
  let_def, intro, intros, makeCase, makeOpen, makeOpenTL, addDefs,latestMetaSubsts,
  importDefs, checkCorrDefs, compute, computeWHNF, computeWHNFS,
  computeWith, getTypeExp, constraints, getTypeOfMeta, getPosMeta, getMetaInfo_pi,getVisibilityMeta, putPosition,
  getContext,  solveCN,
  solveAllCsSafeRep, suggestSolution, unfoldCN, unfoldN, contUnfoldN,
  contUnfold1, nfC, terminationCounter, setTerminationCounter,includePrelude,
  ArgsBind(..)) 
    where
import Maybe(fromJust,catMaybes)
import List(sortBy)
import Util(thd,findDup)
import AgdaScans(mapMAccumR)
import PPrint(PPrint(..),ppReadable,(~.),separate,nest,ppDebug)
import Position(Position,noPosition)
import Error(ErrMsg(..),PassMsg(..),eMsg,prEMsg)
import Monads(ErrorMonad(..), internalError, traceM, liftM2, liftM3, foldM,liftMaybeSTM)
import Utilities(phandle_,t,pp)
import MetaVarState(Constraint(..),MetaVarV(..))
import MetaVars(MetaVar)
import CSyntax
import ISyntax
import CITrans(CITrans,varScope,cstScope,dummyCCPV,getCstSymTab,initCIT,initCIT_CST)
import CITranslate(toIExp,toILetDefs)
import SimpleICTranslate(translate,translateLetDef)
import Imitate(expand,imitateConstraint)
--import Imitate3(freshVars)
import ProofMonad
import Compute(whnfS)
import qualified Compute
import LazyCompute(whnfL)
import Eval(eval,unfold,evalMeta,arityV,splitDataOrInd,indSplitToConBinds
        ,unfoldJudg,splitMeta,force,isVProd,isVSig,isVDataOrInd,splitSig
        ,splitFun,splitPackage,lookupEnv)
-- import Solve(Tactic(..),solveC,suggestions,solveFirstOrder)
import SuggestSol(Tactic(..),solveC,suggestions)
import qualified Solve(solveFirstOrder)
import Termination(checkTermLetDefs,checkPosLetDefs)
import Typechecking(inferType,checkLetDefs)
import Import(importLetDefs)
import BasicEngineOps(checkAndUpdate,solveCs)
import Monad (unless,liftM)
import Unfold1(unfold1)
import PreludeDefs(preludeDefs)
import MetaVars(preMetaVar)
import ExpToExp(letsToLets)
import NoRec(noRec)
import ValueToExp(valueToExp)
import ClassEnv(splitInstance,lookupInstance,getClassInst,isClass)
import AgdaTrace(trace)
import ExpToExp (letToLet)
import Equal (simplifyCs)
import Typechecking((|-))
--import BasicOps_Properties(assertIfDebugging, unique_metas)
-- begin local abbrevs



f <$> op = f `fmap` op; 
infixl 4 <$>

ppR :: PPrint a => a -> String
ppR    x   = ppReadable x
raiseN msg = raise $ eMsg noPosition msg
eMsgI      = eMsg  . getIdPosition
raiseI i   = raise . eMsgI i
raiseE e   = raise . eMsg (getExpPos e)
raiseCom s = raiseN$ ECommand s

appMetas :: Visibility -> TransClass -> Exp -> Int -> PCM Exp
-- | appMetas cit e n returns e ? ? .. ? (n times)

-- ** Tried to insert assertion during Agda code sprint (2004), but
-- ** underlying code changed since then. The following commented code
-- ** could be used as inspiration for inserting new assertions of the
-- ** same sort.
--     result <- eApp' e `fmap` sequence (replicate n oneFreshMetaVar)
--     assertIfDebugging ("unique_metas: " ++ ppR result) (unique_metas result)
--     return result
--   where pos = getExpPosLast e
--         oneFreshMetaVar = do 
--            i <- metaVarIndex
--	    return (False, EMeta i pos False cit 10 (Just False))

appMetas vis cit e n = do 
        foldM (\e' -> \_ -> appMeta' e') e [1..n]

   where appMeta' e = do 
             i <- metaVarIndex
	     let m = EMeta i (getExpPosLast e) False cit 10 vis 
             return $ eApp' e [(False,m)]

splitCBs :: Value -> PCM [ConBind]
splitCBs t         = either snd indSplitToConBinds <$> splitDataOrInd t

cMeta pos pai hidden     = CMeta pos pai (Just hidden) preMetaVar  
cMetaF pos = cMeta pos False False
cMetaT pos = cMeta pos True False

unfoldedTypeOfJudg :: Judgement a -> PCM Value
unfoldedTypeOfJudg = fmap typeOfJudg . unfoldJudg

toUId0 :: Id -> UId
toUId0 x         = toUId x 0

freshen  :: [Id] -> [UId] -> PCM ([Id], [UId])
freshen          = freshVars True {- replaceInternalsFlag -}
freshen' :: [Id] -> [UId] -> PCM [Id]
freshen' tkn xs  = map toId . snd <$> freshen tkn xs

varIds :: TransClass -> [Id]
varIds        = map toId . varScope . fst


varIdsMeta :: MetaVar -> PCM [Id]
varIdsMeta m  = (varIds.transInfo) <$> getMetaInfo m


clam :: [(Bool,Id)] -> CExpr -> CExpr
clam xs a = cLam [CArg xs (cMeta (getCExprPos a) False False )] a

cuniv :: [(Bool,Id)] -> CExpr -> CExpr
cuniv xs a = cUniv [CArg xs (cMeta (getCExprPos a) False False )] a



-- iniTCEnv      = (emptyC,emptyEnv)

-- If given a meta-variable it returns the translation- and
-- type-checking environment of the meta.variable,
-- otherwise the top-level translation environmnet and the empty 
-- type-checking environmnet
environmentInfo :: Maybe MetaVar -> PCM (TransClass,TCEnv)
environmentInfo = maybe (fmap (\     st -> ((initCIT_CST st,initClassEnv),emptyTCEnv)) getSymTab)
                     (fmap (\ mI ->(transInfo mI,(context mI,environment mI))).getMetaInfo)



expand' :: TransClass -> [Id] -> Exp -> PCM CExpr
expand' cit   = expand (Just(cstScope $ fst cit))

-- | onUninst is never used in this module and not exported!
onUninst :: MetaVar -> PCM a -> PCM a
onUninst m op = isUninstantiated m >>= \yes-> if yes then op
                else raiseN$ EUninstantiated$ show m

getUninstMetaInfo :: MetaVar -> PCM MetaVarV
-- | lookups the info for an uninstantiated meta-variable
getUninstMetaInfo m = do isUninst <- isUninstantiated m
                         unless isUninst  (raiseN$ EUninstantiated$ show m)
                         getMetaInfo m 

translateMetaSubsts :: [MetaSubst] -> [(Bool,MetaVar,CExpr)]
translateMetaSubsts mes = map (\(aut,m,e) -> (aut,m,translate True e)) mes

-----------------------------------------------------
-- Operations that will update the proof state -------
-----------------------------------------------------

opMME :: ((TransClass,TCEnv) -> Exp -> PCM b) -> Maybe MetaVar -> CExpr -> PCM b

opMME  op mm ce = do 
        ei@(cit,_) <- environmentInfo mm 
        e <- toIExp cit True noPar ce 
        op ei e

givtran :: Bool -> MetaVar -> Exp -> PCM (CExpr)
givtran    remHidden    m  e = do 
        e' <- checkAndUpdate (|-) False m e 
        --traceM ("give " ++ ppReadable mes)
        return (translate remHidden e')


givcic ::  TransClass -> Bool -> MetaVar -> CExpr -> PCM (CExpr)
givcic cit pai m ce = toIExp cit pai noPar ce >>= givtran True m 
-- end abbrevs

-----------------------------------------------------
-- Operations that will update the proof state -------
-----------------------------------------------------
-- | Agsy
auto :: MetaVar -> PCM CExpr
auto m = internalError "Auto not implemented yet"


-- | Set metavariable to an expression.
giveme :: MetaVar -> CExpr -> PCM (CExpr)
giveme m ce = do 
        mI <- getUninstMetaInfo m
        e  <- toIExp (transInfo mI) (parseInfo mI) (prec mI) ce
        givtran False m e


-- | refineExact m h returns h ?1 ... ?n where
--  n is the estimated arity of h
refineExact ::MetaVar-> CExpr-> PCM (CExpr)
refineExact m ce = do
        --traceM ("exact "++ppReadable ce) 
        mI <- getUninstMetaInfo m
        e <- toIExp (transInfo mI) (parseInfo mI) (prec mI) ce 
        e' <- refineExactExp m e
        return (translate True e')

-- | refine m h tries to find an expression, h ? ... ?, such that
--  the expression type-checks. Maximum number of meta-variables that it 
--  tries is 15, minimum is 0.
refine ::MetaVar-> CExpr-> PCM (CExpr)
refine m ce = do 
        mI <- getUninstMetaInfo m
        e <- toIExp (transInfo mI) (parseInfo mI) (prec mI) ce 
        --traceM $"1 " ++ ppReadable e
        e' <- refineExp m e
        return (translate True e')

 
refineExp ::  MetaVar -> Exp -> PCM (Exp)
refineExp m e@(ECon _ _) = refineExactExp m e
refineExp m e =  do mI <- getUninstMetaInfo m
                    try (transInfo mI) 15 (pos mI, ERefine 15) e where
   try cit 0 _       _  = refineExactExp m e
   try cit i lasterr e' = checkAndUpdate (|-) False m e'   `handle` go
            where go (_,ETooManyArgs _) = raise lasterr
                  go err                = try cit (i-1) err =<< appMetas (Just False) cit e' 1
 

refineExactExp :: MetaVar -> Exp -> PCM (Exp)
refineExactExp m e = do 
        e' <- saturate_ (Just False) m e 
        checkAndUpdate (|-) False m e'       

saturate_ :: Visibility -> MetaVar ->  Exp -> PCM Exp
saturate_ vis m e = do 
    mI <- getUninstMetaInfo m
    let gr = (context mI,environment mI)
        t = typeOfJudg (metaJudg mI)
    n <- arity gr t e
    appMetas (Just False) (transInfo mI) e n
 where
  arity _ t (ECon c es) = maybe (notElem t) length' . lookupConstr c
                   =<< (splitCBs t `handle_` notSum t) where
    length' tel = return(sum(map(length.fst)tel)- length es) -- Change for hidden args
    notSum  t = raiseI c $ ENotSum     (ppR t)
    notElem t = raiseI c $ ENotSumElem (ppR t) (ppR c)
  arity gr _ e = do (t',_) <- inferType gr e; u <- unfold t'
                    arityV u `handle_` raiseN(ERefineArity$ ppR t')


-- | refinePrj h n1 .. nk gives the expression 
--  (...((h ? ..?).n1 ? ..?).n2 ?..?)...).nk ? .. ? 
refinePrj :: MetaVar-> Id -> [Id] -> PCM (CExpr)
refinePrj m h ns = do   
                mI <- getUninstMetaInfo m
                e <- toIExp (transInfo mI) False 9 (CVar h)
                e' <- refinePrjExp m e ns
                return (translate True e')
    

refinePrjExp :: MetaVar -> Exp -> [Id] -> PCM (Exp)
refinePrjExp m e ns  = buildProj (Just False) m e ns >>= refineExp m

buildProj :: Visibility -> MetaVar -> Exp -> [Id] -> PCM Exp
buildProj vis m e [] = saturate_ vis m e
buildProj vis m e (n:ns) = do
    e' <- saturate_ vis m e
    buildProj vis m (EProj e' n) ns

-- | abstract m x1 .. xn will give as result \x1 -> ... \xn -> ? 
-- If the type of m is a meta-variable, then that meta-variable 
-- will be replaced by a product type.

abstract :: MetaVar -> [Id] -> PCM (CExpr, Maybe (MetaVar,CExpr)) 
-- Change so that it takes [(Bool,Id)] instead of [Id]
abstract m0 xs0 = do 

        mI <- getUninstMetaInfo m0
        t <- unfoldedTypeOfJudg (metaJudg mI)
        abstract' (pos mI) (parseInfo mI) (transInfo mI)  m0 [] xs0 t `handle_` raiseCom "abstract"
 where 
        abstract' pos pai cit m zss [] t = out pos pai cit m  Nothing zss --return (CMeta pos pai m0, Nothing)
        abstract' pos pai cit m zss xs1 (EClos r1 (EProd (ys1,a) b)) = f [] xs1 ys1 r1 where
                 f zs (x:xs)((h,y):ys) r = f ((h,x):zs) xs ys (updateEnv r y (EVar(toUId0 x) (Just cit)))
                 f zs   xs    ys r = abstract' pos pai cit m (zs:zss) xs =<< eval r(eProd[(ys,a)]b)
        abstract' pos pai cit m zss (x:xs1) (EArrow h a b) = 
                 abstract' pos pai cit m ([(h,x)]:zss) xs1 b
        abstract' pos pai cit m  zss xs t = do  
                (m',_)<- splitMeta t                      
                ys <- (`freshen'` map toUId0 xs) =<< varIdsMeta m 
                a  <-  giveme m' $ foldr (cuniv.(:[])) (cMetaF pos) (map ((,) False) ys)    
                out pos pai cit m (Just(m',a)) ([[(False,x)]|x<-reverse xs]++zss)
        out pos pai cit m ma ys = do 
                let ce = foldl (flip(clam.reverse)) ((cMeta pos) pai False) ys
                e <- givcic cit pai m ce
                return (e,ma)


-- | let_def m x1 ...xn updates m with the expression 
-- let {x1 :: ? = ?; ... ; xn :: ? = ?} in ?
let_def :: MetaVar -> [Id] -> PCM (CExpr)
let_def m xs = giveme m$ Clet ds (cMetaT noPosition) where
  ds = [CSimple(CDef [] (CValueT x [] (cMetaF noPosition) (cMetaT noPosition)))|x<- xs]

-- | intro m will either give an expression where all leading lambda-abstractions are
-- made or a record, depending on the type.
intro :: MetaVar -> PCM  (CExpr)
intro m  = do 
              mI <- getUninstMetaInfo m
              t <- unfoldedTypeOfJudg (metaJudg mI)
              let p = pos mI
                  cit = transInfo mI
                  pai = parseInfo mI
              e <- if isVProd t then mkLambda p pai (varIds cit) t
                      else mkStruct_ p t `handle_` raiseCom "intro"
              givcic cit pai m e
              
   where 
     mkLambda pos pai tkn (EClos r (EProd (xs,a) b)) = do
                                let (hs,xs') = unzip xs 
                                (tkn',ys) <- freshen tkn xs'
                                b' <- eval (addIdEnv r xs' ys) b
                                ce <- mkLambda pos pai tkn' b'
                                return $ clam (zip hs $ map toId ys) ce
     mkLambda pos pai tkn (EArrow h a b) = do
                                (tkn',[y]) <- freshen tkn [toDummyUId dummyId]
                                ce <- mkLambda pos pai tkn' b
                                return $ clam [(h,toId y)] ce
       
     mkLambda pos pai _ _ = return$ cMeta pos pai False


mkStruct_ :: Position -> Value -> PCM CExpr
mkStruct_ pos t = splitSig t >>= \ (_,_,sigdfs)-> return
   (CRecord [] pos [ CSimple (CDef [] (CValueT (toId (snd x)) [] (cMetaF pos) (cMetaT pos)))
                          | ESigAbs (xs,_) <- sigdfs, x <- xs])




-- | makeCase m e will return the case-expression 
-- case e of {pat1 -> ? ; ... ; patn -> ? } 
-- where pat1, .., patn corresponds to the constructors of the
-- type of the expression e
makeCase :: MetaVar -> CExpr -> PCM (CExpr)
makeCase m ce = do 
        mI <- getUninstMetaInfo m
        let p = pos mI
            cit = transInfo mI
        e <- toIExp cit False noPar ce 
        (t,e') <- inferType (context mI,environment mI) e
        conBinds <- splitCBs t `handle_` notSum p t
        caseBranches <- mapM (arm p cit) conBinds
        let caseExp = Ccase ce caseBranches
        givcic cit (parseInfo mI) m caseExp
    where notSum pos t   = raise . eMsg pos $ ENotSumCase (ppR t) ""    
          arm :: Position -> TransClass -> ConBind -> PCM (CPat,CExpr)
          arm pos cit (c,tel) = do 
                let (hs,xs') = unzip $ concatMap fst tel
                xs <- freshen' (varIds cit) xs'
                let me = cMeta pos True False
                    cpat = CPCon c $ map (CPVar . CPatId) xs
                return (cpat,me)


-- | mkOpen m e returns 
-- open e use x1, ..., xn in ?
-- where x1, ..., xn are the fields of the type of e
makeOpen   :: MetaVar -> CExpr -> PCM (CExpr)
makeOpen m ce = do 
        mI <- getUninstMetaInfo m 
        let cit = transInfo mI
        xs <- mkOpen_  (False,prec mI,(context mI,environment mI),cit) ce
        let cOpenArgs = COpenArgs $ map (COArg [] . toId)  xs
            cOpenExp = Copen ce cOpenArgs (cMeta (pos mI) False False)
        givcic cit False m cOpenExp
        



-- | mkOpenTL  e returns a top-level declaration, 
-- open e use x1, ..., xn 
-- where x1, ..., xn
-- are the fields of the type of e
-- will not change the state

makeOpenTL :: CExpr -> PCM (CLetDef)
makeOpenTL ce = do 
        st <- getSymTab
        let headSol =  case head     ce   of {Nothing -> raiseN ENoOpen;
                Just i -> case lookupST st i of {Nothing -> err i;
                Just c -> do{d<-def c; case getIds d of{[]->err i; xs-> return xs}}}}
        xs <- mkOpen_ (True,noPar,emptyTCEnv,(initCIT_CST st,initClassEnv)) ce `handle_` headSol
        let cOpenArgs = COpenArgs $ map (COArg [] . toId)  xs
            oa = COpen ce cOpenArgs 
        return  (CSimple (CDef [] oa)) 
 where head (CVar   i    ) = Just i
       head (CApply e _  ) = head e
       head (CBinOp _ i _) = Just i
       head _              = Nothing
       getIds (Def _   _ _ _ _ _ _ (DExp e)) = getIdsExp e
       getIds (UnTypedDef _ _ _ _ _ (DExp e)) = getIdsExp e
       getIds _                           = []
       getIdsExp (EAbs     _ e   ) = getIdsExp e
       getIdsExp (EDef     _ e   ) = getIdsExp e
       getIdsExp (EStruct  _ ds _ st _ ) =  map snd  st
       getIdsExp (Epackage _ ds _ st _ ) = map snd st
       getIdsExp (EOpen    _ _  e) = getIdsExp e
       getIdsExp _                 = []
       err i = raiseI i ENoOpen


mkOpen_ :: (Bool, Int, TCEnv,TransClass)
           -> CExpr -> PCM [UId]
mkOpen_ (pai,pi,(g,r),cit) ce = do
  e <- toIExp cit pai pi ce
  (t,e') <- inferType(g,r) e 
  domESigDefs . thd <$> splitSig t
    `handle_` do { v<-eval r e; (\ (_,_,_,st,_)-> map snd st)<$> splitPackage v
    `handle_` raiseE e ENotProductOpen} -- obs grouping



addDefs, importDefs :: [CLetDef] -> PCM ([CLetDef],[LetDef])
addDefs    = chkOrImprt_ (\tchenv -> \c -> \ds -> (checkLetDefs tchenv ds >>= (\ (_,ds') -> return ds')))
importDefs = chkOrImprt_ (\tchenv -> \c -> \ds -> importLetDefs tchenv c ds >>= \_ -> return ds)

chkOrImprt_ :: (TCEnv -> Context -> [LetDef] -> PCM [LetDef])
               -> [CLetDef] -> PCM ([CLetDef], [LetDef])
chkOrImprt_ op cds = do
  st <- getSymTab
  classEnv <- getClassEnv 
  (cit,ds) <- toILetDefs True (initCIT_CST st,classEnv) cds    
  --traceM ("translated ok" ++ ppReadable ds)
  case findDup$ domST st ++ map toId (concatMap idsLetDef ds) of
    (c:c':_)-> raiseI c $ EDuplicate (getIdPosition c') (pprId c)
    _       -> do ds' <- op emptyTCEnv  emptyC ds
                  --traceM (unlines (map ppReadable ds'))
                  putSymTab(getCstSymTab $ fst cit)
                  putClassEnv(snd cit) 
                  -- getConstraints >>= \cs -> traceM (ppReadable cs)
                  --traceM ("Constraints " ++ ppReadable mes)
                  --letsToLets ds' >>= (\ds2 -> traceM $ unlines $ map ppReadable $ map (translateLetDef False) ds2)  -- Prints the definitions with hidden arguemnts expended
                  return (map (translateLetDef False) ds', ds')


-- | Will give the meta variables that has been solved automatically since last 
-- time it was called
latestMetaSubsts :: PCM [CMetaSubst]
latestMetaSubsts = do 
        mes <- flushBoundedMetaVars 
        return (translateMetaSubsts mes)
    
solveCN :: Int -> Int -> PCM () -- tac, constraint num
-- Doesn't do anything anymore, change
-- Check that it is used only on visible constraints
solveCN t n = do c <- raint_ n; 
                 solveC c (Tactic(t+1));return () `handle` err where
  err er@(_,EUnknownTactic _)= raise er
  err _ = raiseN$ ESolveNotApp (show t) (show n)
raint_ n = getConstraints >>= \ cs-> if n < length cs then return(cs!!n)
           else raiseN$ EUnknownConstraint (show n)

solveAllCsSafeRep ::  Int -> PCM ()

solveAllCsSafeRep limit = do 
    solveCs (|-)
    trySolveClasses
    return ()

{-
solveAllCsSafeRep limit = do 
	go limit 
        return ()
 where
  go i| i>0       = do r<- solveCsSafe
                       if null r then return [] else (r++) <$> go(i-1)
      | otherwise = raiseN$ ESolveNonTerm limit
  trySolveCs :: Int -> PCM [CMetaSubst]
  trySolveCs tac = getConstraints >>= foldr try (return []) where
    try c rest = (Solve.solveC c (Solve.Tactic (tac+1)) >> simplifyCs >> return []) `handle_` rest 
  solveCsSafe :: PCM [CMetaSubst]
  solveCsSafe = do mes <- trySolveCs 0
                   mes' <- trySolveClasses 
                   return$ mes ++ mes'


-}


includePrelude :: PCM ()
includePrelude = do (cds,ds) <- addDefs preludeDefs
                    --traceM$ ppReadable cds
                    return ()

-------------------------------------------------------
---- Operations for information and computations ------
-------------------------------------------------------

-- | constraints returns all the constraints in the proof stat
constraints::PCM [CConstraint]
constraints = do 
        constrs <- getConstraints
        -- traceM ("constraints "++ppDebug constrs)
        mapM (imitateConstraint(False,True,False,True,False,Just[])) constrs
              

-- | getTypeOfMeta i m returns the type of the meta-variable m.
-- If i > 0 then the type will be computed

getTypeOfMeta :: Int -> MetaVar -> PCM (CJudgement MetaVar)
getTypeOfMeta i m0 = getMetaInfo m0 >>= \ mI -> 
  let cit = transInfo mI
  in case (metaJudg mI) of
        IsType m -> return$ CIsType m
        m :! v   -> do v' <- evalMeta v
                       if i == 0 then HasType m<$>expand' cit (varIds cit) v'
                                 else HasType m<$> (expand (Just []) (varIds cit) =<< unfold v')


-- | getTypeExp i (Just m) e will give the type of the expression
--  at the place of the meta variable m.
--  getTypeExp i Nothing  e will the type on top-level
--  If i > 0 then the resulting type will be evaluated
getTypeExp  i  maybeMeta ce = do 
        (cit,tchenv@(g,r)) <- environmentInfo maybeMeta
        e <- toIExp cit True noPar ce
        (v,_) <- inferType tchenv e
        v' <- optUnfold i v 
        expand' cit (map (toId.fst) g) v'
 -- opExpand$ ((optUnfold i =<<).).inferType

getPosMeta :: MetaVar -> PCM Position
getPosMeta m = do
        mI <- getMetaInfo m
        return$ pos mI

getVisibilityMeta :: MetaVar -> PCM Visibility
getVisibilityMeta m = do
        mI <- getMetaInfo m
        return$ visibility mI

getMetaInfo_pi :: MetaVar -> PCM Int
getMetaInfo_pi = fmap (prec).getMetaInfo -- not here


-- for printing of contexts
data ArgsBind = AnArg UId CExpr | ABind UId CExpr CExpr | APackage UId
instance PPrint ArgsBind where
  pPrint d p arg = case arg of 
      AnArg x a   -> x.:a                           
      ABind x a e -> x.:a</>nest 2 (t"= "~.pp d e)                          
      APackage  x -> t"package "~.ppUId d x                                 
    where x .:  a = ppUId d x ~.t" ::" </> nest 2 (pp d a)
          p </> q = separate [p,q]

-- | returns the context of the meta-variable
getContext :: MetaVar -> PCM [ArgsBind]
getContext m = getMetaInfo m >>= \ mI -> let
     cit = fst (transInfo mI)
     g = context mI
     (xs, cs) = ([x| x<- map fst g, elem x (varScope cit)], cstScope cit)
     expand_ lcs tkn e = expand(Just lcs) tkn =<< eval emptyEnv e
     var tkn x = typ . fromJust $ lookupC x g where
       typ a = do ca <- expand_ cs tkn a
                  e <- lookupEnv x (environment mI)
                  arg <- case e of
                            EVar y _ | y == x -> return$ AnArg x ca
                            _                 -> ABind x ca <$> expand_ cs tkn e
                  return (toId x:tkn,arg)
     cst lcs c = def c >>= \d-> (,)(c:lcs) <$> case d of
       (Def _ _ _ _ _ _ EPackageType _)-> return(APackage c)
       _ -> AnArg c <$> expand_ lcs (map toId(varScopeDef d))(typeOfDef d)
  in do r <- getAllInScope m
        -- traceM (ppReadable r)
        liftM2((.snd).(++).snd)(mapMAccumR var [] xs)(mapMAccumR cst [] cs)


getAllInScope :: MetaVar -> PCM ([ArgsBind],[CLetDef])
getAllInScope m = getMetaInfo m >>= \ mI -> let
     cit = fst (transInfo mI)
     g = context mI
     (xs, cs) = ([x| x<- map fst g, elem x (varScope cit)], cstScope cit)
     expand_ lcs tkn e = expand (Just lcs) tkn =<< eval emptyEnv e
     var tkn x = typ . fromJust $ lookupC x g where
       typ a = do ca <- expand_ cs tkn a
                  e <- lookupEnv x (environment mI)
                  arg <- case e of
                            EVar y _ | y == x -> return$ AnArg x ca
                            _                 -> ABind x ca <$> expand_ cs tkn e
                  return (toId x:tkn,arg)
     cst c = def c >>= (letToLet.DSimple)  >>= \d-> return$ translateLetDef False d
     
  in do vars <- liftM snd  (mapMAccumR var [] xs)
        csts <- mapM cst cs
        return (vars,csts)


{-
If it should exist it shoud return [CMetaSubst]
getMetaSub ::  PCM [(MetaVar,Exp)]
getMetaSub  = getMetaSubsts
-}

-- | intros m returns a lambda-expression if the type is a, constructors
-- or a record, depending on the type
intros :: MetaVar -> PCM [CExpr]
intros m = do 
        mI <- getUninstMetaInfo m
        vt <- unfoldedTypeOfJudg (metaJudg mI)
        t <- force vt
        makeIntros (pos mI) (parseInfo mI) (transInfo mI) t
  where  makeIntros pos pai cit t
                | isVProd      t =  do 
                        (_,((h,x) :_,_),_) <- splitFun t
                        xs' <- freshen' (varIds cit) [x] 
                        return [clam (zip [h] xs') (cMeta pos pai False)] 
                | isVDataOrInd t = let ccon :: (Id,Tel) -> CExpr
                                       ccon (c,tel) = CApply (CConS c) [(h,cMeta pos False h)|(hxs,_) <-tel, (h,_) <- hxs]

                                   in map ccon <$> splitCBs t 
                                     
                | isVSig       t = (:[])<$>mkStruct_ pos t
                | otherwise    = return []



suggestSolution :: PCM [(MetaVar,CExpr)]
suggestSolution = (mapM (\ (m,e) -> return (m,translate False e))) =<< suggestions =<< getConstraints



-- Everything with computations needs to be cleaned up.
type Computor =        Maybe MetaVar-> CExpr-> PCM CExpr
compute::Computor
computeWHNF, computeWHNFS :: Maybe MetaVar-> CExpr-> PCM CExpr
computeWith::(Exp-> Environment-> PCM Value)-> Computor
getTypeExp ::                           Int -> Computor
compute      = computeWith whnfS
computeWHNFS = compute           
computeWHNF  = computeWith whnfL 

        

computeWith red = opExpand$ \(_,r) -> (`red` r)


optUnfold :: Int -> Value -> PCM Value
optUnfold   i   = if i==0 then return else unfold

opExpand :: (TCEnv -> Exp -> PCM Exp)
            -> Maybe MetaVar -> CExpr -> PCM CExpr
opExpand op =opMME$ \(cit,tchenv@(g,_))e -> op tchenv e>>=expand' cit(map(toId.fst)g)



unfoldCN :: Int -> Int -> PCM () -- raint num, lhs(0) or rhs.
unfoldCN n side = delAllConstraints >>= \ cs -> if n < length cs then let
        (cs0,c:cs1) = splitAt n [(v1,v2)|Constraint v1 v2 <- cs]
      in sequence_ (map add cs0 ++ (add =<< unf c) : map add cs1)
    else raiseN$ EUnknownConstraint (show n)  where
  add        = addConstraint
  unf(v1,v2) = if side == 0 then flip(,)v2<$>unfold v1 else (,)v1<$>unfold v2

unfoldN :: Int -> Maybe MetaVar -> CExpr -> PCM CExpr
unfoldN n = opMME$ \(_,(_,r)) e-> eval r e >>= setValue.Just>> contUnfoldN n

contUnfoldN :: Int -> PCM CExpr; contUnfold1 :: PCM CExpr
contUnfoldN n = cUnf_ $ foldM(\v _ -> unfold1 v) `flip` [1..n]
contUnfold1   = cUnf_ $ unfold1
cUnf_ op = getValue >>= maybe (internalError "No computation going on")
           (\v->do{v'<- op v; setValue(Just v'); expand Nothing [] v'})

nfC :: Int -> MetaVar -> CExpr -> PCM CExpr
nfC  lim = nfC_ lim . Just
nfC_ lim = opMME$ \ (cit,(g,r)) e -> eval r e >>= go lim >>= \ v ->
           setValue(Just v) >> expand' cit (map (toId.fst) g) v where
  go i v = if i < 0 then return v else  unfold v >>= \ u -> case u of
           EApp   h  vs    -> eApp' h <$> mapM go' vs
           EBinOp v1 v2 v3 -> liftM3 EBinOp(go (i-1)  v1)(go (i-1) v2)(go (i-1) v3)
           ECon   c  vs    -> ECon  c <$> mapM go' vs
           EConF  c  v  vs -> liftM2(EConF c) (go (i-1) v) (mapM go' vs)
           EProj  v  n     -> EProj `flip` n <$> go (i-1) v
           EStop  m  v     -> EStop m <$> go (i-1) v
           _               -> return u
   where go' (h,e)  = do v <- go (i-1) e 
                         return (h,v)

-------------------------------------------------------
---- Other operations ---------------------------------
-------------------------------------------------------

checkCorrDefs :: [CLetDef] -> PCM ([UId],[([UId],UId)])
checkCorrDefs cds = do
        (_,ds) <- addDefs cds 
        return (checkTermLetDefs ds, checkPosLetDefs ds)


putPosition :: MetaVar -> Position -> PCM ()
putPosition m p = do 
        mI <- getMetaInfo m
        setMetaInfo m (mI {pos = p})


terminationCounter    :: PCM Int
setTerminationCounter :: Int -> PCM ()
terminationCounter = getTermCounter
setTerminationCounter = setTermCounter


{-
====== Experiment with classes =============================
-}

trySolveClasses :: PCM [CMetaSubst]
trySolveClasses = do
        ms <- getUninstantiated
        mmes <- mapM trySolve ms
        return $ concat$ catMaybes mmes
    where trySolve :: MetaVar -> PCM (Maybe [(Bool,MetaVar,CExpr)])
          trySolve m = do
                mI <- getMetaInfo  m
                --traceM ("kom hit "++ppReadable j)
                let tc@(cit,classEnv) = transInfo mI
                case (metaJudg mI) of 
                        (_:! tv) -> do --t <- valueToExp False False m tv  `handle_` return tv
                                       solveWith classEnv m tv
                       
                        _ -> return Nothing
          solveWith :: ClassEnv -> MetaVar -> Value -> PCM (Maybe [CMetaSubst])
          solveWith classEnv m e = do
                --traceM$ "In solveWith" ++ (ppDebug classEnv)
                --traceM (show m)
                --traceM (ppReadable e)
                --traceM "End solveWith"
                mes <- solveInstanceMeta classEnv m e
                return$ Just mes
           `handle_` return Nothing
   

solveInstanceMeta :: ClassEnv -> MetaVar -> Value -> PCM ([CMetaSubst])
solveInstanceMeta  classEnv  m v = do 
        mI <- getUninstMetaInfo m
        -- v <- eval (environment mI) e
        --traceM (ppDebug v)
        v' <- unfold v
        (v',es) <- splitApp v'
        c <- getClass classEnv v'
        v2 <- getArg es 
        -- traceM (ppDebug v2)
        e' <- valueToExp False False m v2
        buildSol mI classEnv c m e' `handle` tryMeta v2 v
   where splitApp (EApp h es) = return (h,es)
         splitApp _ = internalError "not on class form"
         tryMeta (EStop _ ( EClos _ (EMetaV m' _ _  _))) e err  = do
             cs <- getConstraints 
             mes <- foldr (try m') (return []) cs
             if null mes  then raise err else do 
                mes' <- solveInstanceMeta classEnv m e
                return (mes ++ mes')
         tryMeta e _ err = raise err --trace ("in tryMeta 5" ++ show e) raise err
             
         try m c rest = Solve.solveFirstOrder m c `handle_` rest 
         buildSol :: MetaVarV -> ClassEnv -> UId -> MetaVar -> Exp -> PCM ([CMetaSubst])
         buildSol mI classEnv c m e' = do 
                --traceM ("lookup "++ ppDebug c ++ " " ++ ppDebug e')
                (name,prjs)  <- liftMaybeSTM (lookupInstance classEnv e' c) (getUIdPosition c,ENoInstance (ppReadable e') (ppReadable c)) 
                --traceM ("Projections found "++ ppDebug prjs)
                e <- toIExp (transInfo mI) False 9 (CVar (toId name))
                e' <- buildProj (visibility mI) m e (map toId prjs) 
                checkAndUpdate (|-) True m e'
                return$ (True,m,translate True e'):[]  -- Shouldn't be needed
         getClass :: ClassEnv -> Value -> PCM UId
         getClass classEnv (EClos _ (EConstV c _)) = 
                if isClass classEnv c then return c else internalError ("not on class form")
         getClass classEnv v = do
                --traceM ("Not a class " ++ ppDebug v)
                internalError ("not on class form")
         getArg :: [(Bool,Value)] -> PCM Value
         getArg [(_,v)] = unfold v
         getArg _ = internalError "Wrong number of arguments"

{- 
solveConstraintsInstance :: ClassEnv ->  PCM ([CMetaSubst])
solveConstraintsInstance cenv = do
        cs <- constraints
        minsts <- areInstances cenv cs
        mes <- trySolveInstances cenv m e

   where 

-}

