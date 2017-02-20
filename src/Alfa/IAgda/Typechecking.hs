{-|

  The type checker.   
  It  adds hidden arguments to the expressions. It also solves constraints while type checking.
  Operates on ISynType.Exp.

  Three judgements:

  * Gamma |- a is type  (Wellformedness of types.)

  * Gamma |- e :! v     (Type checking.)

  * Gamma |- e -> v     (Type inference.)


-}


module Typechecking ((|-),inferType,checkLetDefs,
            TCEnv,mkLetDefError) where

import System
import List (find,delete,deleteFirstsBy,partition,union,(\\))
import Maybe
import AgdaTrace
import ISyntax
import Equal
import Eval
import Monad
import Monads
import Position
import ProofMonad
import Error
import Id (getUIdPosition,pprId)
import PPrint(PPrint, ppReadable,pIText,PDetail (..),ppDebug,(^.))
import CSyntax()
import Imitate(expand)
import Util (joinByFst,my_assert,rTake,findDup,mDefault,snd3,thd)
import AgdaScans
import Utilities --(foldlM,oM,tM,mapPairM,mapPair',tup_ass)
import CITrans
import ValueToExp (valueToExp)
import ExpToExp (expToExp)
import NoRec(noRec)
import MiscId
import Literal
import BasicEngineOps 
--import SimpleSolve (solveAllCsSafeRep)
--import ExpToExp(expToExp,telToTel)

infix 5 |- 

-- type TCEnv = (Context,Environment)



{- main routines: (|-) and inferType -}

(|-) :: TCEnv -> Judgement Exp -> PCM Exp

tchenv |- IsType e@(ESort  _ _ )      = return e

tchenv |- IsType (EProd bind b) = do 
        (tchenv',bind') <- corrBind tchenv bind 
        b' <- tchenv' |- IsType b
        return (EProd bind' b')

tchenv |- IsType (EArrow h a b) = do
        a' <- tchenv |- IsType a
        b' <- tchenv  |- IsType b
        return (EArrow h a' b')



(g,r) |- IsType (EMeta m pos pai cit pi hidden) = do
        updateMetaInfo m pos pai cit pi hidden g r (IsType m)
        addUnboundMetaVar m
        return (EMeta m pos pai cit pi hidden )


tchenv |- IsType (ESig pos sds)        = do
        (_,sds') <- mapAccumLM corrSigDef tchenv sds 
        return (ESig pos sds')
  where
        corrSigDef :: TCEnv -> ESigDef -> PCM (TCEnv,ESigDef)
        corrSigDef tchenv (ESigAbs b)  = do 
                (tchenv',b') <- corrBind tchenv b
                return (tchenv',ESigAbs b')
        corrSigDef tchenv (ESigDefn d) = do
                (_,d') <- checkDef tchenv emptyC [] d
                return (tchenv,ESigDefn d')



tchenv |- IsType (EDef ds e)        = do 
        (_,ds') <- checkLetDefs tchenv  ds
        e' <- tchenv |- IsType e
        return (EDef ds' e')



tchenv |- IsType (EOpen m as e)     = do 
        (_,m',as') <- checkOpenArgs tchenv [] m as
        e' <- tchenv |- IsType e
        return (EOpen m' as' e')

tchenv@(g,_) |- IsType e                  = do 
        (tv,e') <- inferType tchenv e
        (tv0,e2) <- addHiddenArgs tchenv e' tv Nothing
        getSort tv0 `handle_` mkErr tv0
        return e2
  where mkErr:: Exp -> PCM a
        mkErr t = do --t' <- maybe (return t) (\cit -> expand (Just(cstScope cit)) (map toId (varScope cit)) t) (transInfoExp t)
                     raiseE e $ ENotType (ppR e) (ppR t)


tchenv |- (e@(ESort pos k) :! t)    = do 

        s <- getSort t `handle_` mkErr
        checkSort k s 
        return e
  where
  mkErr = raiseE e
          $ EKindError "" (ppR e) (ppR (ESort pos (addToSort 1 k))) (ppR t) pos
                                      
tchenv |- (e@(EProd bind b) :! t)= do     
        getSort t  `handle_` mkErr
        (tchenv',bind') <- chkBind t tchenv bind
        b' <- tchenv' |- b :! t
        return (EProd bind' b')
  where mkErr = raiseE e $ ENotType (ppR e) (ppR t)



tchenv |- (e@(EArrow h a b) :! t)= do     
        getSort t  `handle_` mkErr
        a' <- tchenv |- a :! t
        b' <- tchenv  |- b :! t
        return (EArrow h a' b')
  where mkErr = raiseE e $ ENotType (ppR e) (ppR t)

tchenv@(_,r) |- (e@(EAbs (xs,a) b) :! t) | not (isStopped t) = do 
        a' <- tchenv |- IsType a
        va  <- eval r a'
        (tchenv',tb') <- checkAbs a' va xs tchenv t 
        b' <- tchenv' |-  b :! tb'
        return (eAbs [(xs,a')] b')
  where
    checkAbs a va [] tchenv t =
           return (tchenv,t)
    checkAbs a va xs tchenv t = do
        (tr, (ys,ta), tb) <- splitFun t `handle_` mkErrA
        vta <- eval tr ta
        va =? vta                       `handle` mkErrB vta
        let n = min (length xs) (length ys)
        let (xs0,xs1) = splitAt n xs
            (ys0,ys1) = splitAt n ys
            tchenv' = addBind tchenv (xs0,a)
        tr' <- buildEnv tr xs0 ys0
        tb' <- eval tr' (eProd [(ys1,ta)] tb)
        checkAbs a va xs1  tchenv' tb' 

    buildEnv tr ((h,x):xs) ((h',y):ys) =  
        if h == h' then 
                if isDummyUId y then buildEnv tr xs ys
                   else buildEnv (updateEnv tr y (EVar x Nothing)) xs ys
           else mkErrC                  

    buildEnv tr _ _ = return tr 
    mkErrA         = raiseE e $ ENotFun (ppR e) (ppR t)
    mkErrB vta err = raiseE e
                     $ EDeclaredNotEqual (ppR xs) (ppR a) (ppR vta)
                     $ prNPEMsg err
    mkErrC = raiseE e EHiddenArgs


tchenv@(_,r) |- (EAbs bind@(xs,a) e :! t) = do 
        (tchenv',bind') <- corrBind tchenv bind
        (t',e')   <- inferType tchenv' e `handle_` mkMetaType tchenv'
        v <- eval r (EProd bind' t')
        v =? t
        return (EAbs bind' e')
   where mkMetaType tchenvE@(_,r')= do 
             i <- metaVarIndex
             (cit,tc) <- maybe mkErrA return (transInfoExp t)
             let (hs,xs') = unzip xs
                 cit' = foldr (uncurry addVar) cit (zip (map toId xs') xs')
                 pos = getExpPos t
                 tb = EMeta i pos False (cit',tc) noPar Nothing
             tb' <- tchenvE |- IsType tb
             tv <- eval r' tb'
             e' <- tchenvE |- e :! tv
             return (tv,e')
         mkErrA         = raiseE e $ ENotFun (ppR e) (ppR t)
         mkErrB vta err = raiseE e
                     $ EDeclaredNotEqual (ppR xs) (ppR a) (ppR vta)
                     $ prNPEMsg err


tchenv |- (EDef ds e :! t)        = do 
        (_,ds') <- checkLetDefs tchenv ds
        e' <- tchenv |- e :! t
        return (EDef ds' e')

tchenv |- (EOpen m as e :! t)     = do 
        (_,m',as') <- checkOpenArgs tchenv [] m as
        --traceM (ppDebug $ EOpen m' as' e)
        e' <- tchenv |- e :! t
        return (EOpen m' as' e')


(g,r) |- (e@(EMeta m pos pai cit pi hidden) :! t) = do
        --traceM (show e)
        updateMetaInfo m pos pai cit pi hidden g r (m :! t)
        addUnboundMetaVar m
        return (EMeta m pos pai cit pi hidden)


tchenv |- (e@(ECon i args) :! t)   = do         
        solveCs (|-)
        mdt <- splitDataOrInd t `handle_` mkErrS
        either chkCon chkIndCon mdt
  where
    chkCon (tr, cbs)          = do 
        ctel <- lookup_i cbs 
        (_,_,args') <- fit_args tr ctel
        return (ECon i args')

    chkIndCon (spl@((tr, ttel, cbs), vs)) = do
        (ctel, es)    <- lookup_i (map tup_ass cbs)
        (tr',r',args')       <- fit_args tr ctel
        let (hs,es1) = unzip es 
        vs1   <- mapM (eval tr') es1
        zipWithM eqSolve vs1 (map snd vs)   `handle` mkErrB spl (zip hs vs1)
        return (ECon i args')
      where eqSolve v1 v2 = do v1 =? v2
    fit_args tr ctel = fitsTel (tr,tchenv,args) ctel
                            `handle` mkErrA (length ctel)


    lookup_i cbs     = lookupI i cbs $ ENotSumElem (ppR t) (ppR i)

    mkErrS                = raiseE e . ENotSum $ ppR t
    mkErrA n (_, EPass _) = raiseE e $ ENrConstrArgs (pprId i) n
    mkErrA _ err          = raise err
    mkErrB spl vs1 err    = do
     let t1 = indSplitToIndHd spl
          -- eApp' (indSplitToIndHd spl) --[(False,v) | v <- vs1] 
          -- Not correct with hidden arguments
     raiseE e $ ETypeError "" (prEMsg err) (ppR e) (ppR t1) (ppR t) noPosition


tchenv |- (ECase ex brs :! t) = do
    (tx,ex')  <- inferType tchenv ex
    --traceM'$"|-ECase: tx="++ppDebug tx
    let it = (tchenv, ex', brs, t, tx)
    solveCs (|-)
    spl <- splitDataOrInd tx `handle_` mkErr tx
    --traceM'$"|-ECase: spl="++ppDebug spl
    brs' <- either (checkBranches it) (chkIndCase it) spl
    return (ECase ex' brs')
  where
    mkErr tx = raiseE ex $ ENotSumCase (ppR tx) ""
                     

tchenv |- (EData cbs :! t)      = do  
        solveCs (|-)
        getSort t `handle_` raiseE t ENotSetData
        cbs'  <- mapM (checkTel tchenv t) (map telCB cbs)
        return (EData (zip (map idCB cbs) cbs'))


tchenv |- (EIndData tel cbs :! t) = do
        (tel',cbs') <- chkIndDef tchenv tel cbs t
        return (EIndData tel' cbs')



tchenv |- (ESig pos sds :! t)   = do
        solveCs (|-)
        getSort t `handle_` raiseP pos ENotTypeSig
        (_,sds') <- mapAccumLM corrSigDef tchenv sds
        return (ESig pos sds')
  where
    
        corrSigDef :: TCEnv -> ESigDef -> PCM (TCEnv,ESigDef)
        corrSigDef tchenv (ESigAbs b)  = do 
                (tchenv',b') <- chkBind t tchenv b
                return (tchenv',ESigAbs b')
        corrSigDef tchenv (ESigDefn d) = do
                (_,d') <- checkDef tchenv emptyC [] d
                return (tchenv,ESigDefn d')

tchenv |- (e@(EStruct pos ds xs vcs acs) :! t) = do
        solveCs (|-)
        si <- splitSig t `handle_` mkSigError
        --traceM (ppReadable e)
        --traceM (show xs)
        ds' <- checkRecSig tchenv ds xs si
        return (EStruct pos ds' xs vcs acs)

  where mkSigError = raiseP pos . ENotSignature $ ppR t



tchenv |- (e@(Epackage pos ds xs vcs acs) :! t)  = do 
        --traceM (ppReadable e)
        --traceM (ppReadable xs)
        --traceM (ppReadable vcs)
        solveCs (|-)
        v <- unfold t
        unless (isVPackage v)  mkErr
        (_,ds') <- checkLetDefs tchenv ds

        return (Epackage pos ds' xs vcs acs)
  where mkErr = internalError "Wrong type of package"

gr@(g,r) |- (EIf b e1 e2 :! t) = do 
                              tb <- mkBool 
                              vtb <- eval r tb
                              b' <- gr |- b :! vtb
                              e1' <- gr |- e1 :! t
                              e2' <- gr |- e2 :! t
                              return (EIf b' e1' e2')

tchenv |- (e :! t)                   = do 
	(tv,e') <- inferType tchenv e
        (tv2,e2) <- addHiddenArgs tchenv e' tv (Just t)
        tv2 =? t `handle` mkInferError e2 tv2 t
        return e2
  where mkInferError e2 infType expType err = do
            st <- getSymTab 
            let cit = maybe (initCIT_CST st,initClassEnv) id (transInfoExp e)
                varIds = map toId $ varScope $ fst cit
            infType' <- expand (Just(cstScope $ fst cit)) varIds infType
            -- infType' <- expToExp infType'
            expType' <- expand (Just(cstScope $ fst cit)) varIds expType
            -- expType' <- expToExp expType'
            raiseE e $
             ETypeError "" (prNPEMsg err) (ppR e) (ppR infType') (ppR expType') noPosition



inferType :: TCEnv  -> Exp -> PCM (Value,Exp)

inferType tchenv@(g,r) e@(EVar x mcit)   = 
      maybe (mkErr x) (\a -> (eval r a >>= \v -> return (v,e)))  $ lookupC x g
    where 
        mkErr x = raiseU x $ EInternal ("Unknown variable " ++ ppDebug x
                                      ++ "\n(g,r)=\n"++ppDebug (g,r))

inferType (_,r) e@(EConst c m) = do 
        v <- eval r . typeOfDef =<< def c
        return (v,e)

inferType tchenv@(_,r) (EConF i t []) = do
        t' <- tchenv |- IsType t
        vt        <- eval r t'
        solveCs (|-)
        (tr, cbs) <- either return mkErrB
                      =<< splitDataOrInd vt      `handle_` mkErrA
        contel    <- lookupI i cbs           $ ENotSum $ ppR t
        v <- eval tr (eProd contel vt)            -- eval must be able to handle vals.
        return (v,EConF i t' [])

  where 
    mkErrA   = raiseI i . ENotSum $ ppR t
    mkErrB _ = internalError "EConF for idata constr not yet."

inferType tchenv (EConF i t es) = inferType tchenv (EApp (EConF i t []) es)



inferType  tchenv (EApp h es) = do 
        (vt,h') <- inferType tchenv h
        (v,e) <- typeOfApp tchenv h' es vt `handle` mkErr
        return (v,e)
  where
    mkErr (_,EPass ESplitFun) = raiseE h $ ETooManyArgs (ppR h)
    mkErr err                 = raise err


inferType tchenv@(g,r) (EProj e x) = do
    (te,e') <- inferType tchenv e
    solveCs (|-)
    v  <- eval r e'
    v' <- unfold v
    if isVPackage te then do 
        (pos,tr,_,cs,_) <- splitPackage v' `handle_` mkSigProjErr te

        tv <- eval tr . typeOfDef =<< def =<< (lookupProj x cs `handle_` mkProjErr v pos)
        return (tv,EProj e' x)

     else do 

        (_,tr,sds) <- splitSig te          `handle_` mkSigProjErr te
        t <- lookupSig tr x v' sds              `handle_` mkNotField te
        return (t,EProj e' x)
  where
    mkProjErr v pos = raiseI x $ ENotInRecord (ppR x) (ppR v) pos
    mkSigProjErr te = raiseI x $ ENotProduct (ppR te) (ppR x)
    mkNotField t    = raiseI x $ ENotProductElem (ppR t) (ppR x)

inferType tchenv (EBinOp e1 op e2) = do
     inferType tchenv (EApp op [(False,e1),(False,e2)]) 


{- Why not? -}


inferType tchenv@(_,r) (EAbs bind e) = do 
        (tchenv',bind') <- corrBind tchenv bind
        (t,e')   <- inferType tchenv' e 
        v <- eval r (EProd bind' t)
        return (v,EAbs bind' e')




inferType gr (EIf b e1 e2) = do tb <- mkBool 
                                vtb <- eval (env gr) tb
                                b' <- gr |- b :! vtb
                                (v1,e1') <- inferType gr e1
                                (v2,e2') <- inferType gr e2
                                v1 =? v2
                                return (v2,EIf b' e1' e2')
             



inferType _ e@(ELiteral pos (LInteger i)) = inferTypeLiteral pos integerId >>= \v -> return (v,e)
inferType _ e@(ELiteral pos (LChar i)) = inferTypeLiteral pos charId >>= \v -> return (v,e)
inferType _ e@(ELiteral pos (LString i)) = inferTypeLiteral pos stringId >>= \v -> return (v,e)
inferType _ e@(ELiteral pos (LRational i)) = inferTypeLiteral pos rationalId >>= \v -> return (v,e)


inferType tchenv e@(ESort pos k) = return (ESort pos (addToSort 1 k),e)



{- right, you need for tchenv |- IsType a to return a sort.
inferType tchenv (EProd (xs,a) e) = do
-}
   


inferType _ e                  = raiseE e $ ECanNotInferType (ppR e)

{- support routines in no particular order. -}

inferTypeLiteral :: Position -> Id -> PCM Value
inferTypeLiteral pos c = do
     st <- getSymTab 
     id <- liftMaybeSTM (lookupST st c) (eMsg pos (ELookupPrelude(ppR c)))
     vConst emptyEnv id []

mkBool :: PCM Exp
-- Move to MiscId?
mkBool = do st <- getSymTab
            maybe (internalError "Couldn't make Bool") (\c -> return$ EConst c Nothing) (lookupST st boolId)

checkSort :: Sort -> Sort -> PCM ()
checkSort k k1|k < k1    = done
              |otherwise = internalError "Sort comparison failed"


typeOfApp              :: TCEnv
                       -> Exp              {- head -}           
                       -> [(Bool,Exp)]     {- args -}
                       -> Value            {- type of head -}
                       -> PCM (Value,Exp) {- type of app, metas solved and resulting expression -}

typeOfApp _  head [] v = return (v,head)
typeOfApp tchenv@(_,r) head es v = do
    solveCs (|-)
    (tr,(xs,a),b) <- splitFun v 
    va <- eval tr a
    ((tr',xs',es'),e) <- typeOfAppOne  head (tr,xs,es) va
    if (null es') then do tv <- eval tr' (eProd [(xs',a)] b)
                          return (tv,e)
                  else do vb <- eval tr' b
                          typeOfApp tchenv e es' vb
  where 
    typeOfAppOne head (tr,(h,x):xs,es'@((h',e):es)) va = 
        if h == h'
           then do 
                  e' <- tchenv |- e :! va
                  v' <- eval r e'
                  typeOfAppOne (eApp' head [(h',e')]) ((updateEnv tr x v'),xs,es) va
           else if h then 
                   case e of  
                      (EMeta _ _ _ _ _ (Just aut)) | aut  ->  typeOfAppOne head (tr,(h,x):xs,(h,e):es) va
                      _  ->  do i <- metaVarIndex
                                --traceM (show e)
                                cit <- maybe (internalError $ "Wrong head: "++ppR head) return (transInfoExp head)
                                let pos = getExpPos head
                                    e' = EMeta i pos False cit 10 Nothing
                                typeOfAppOne head (tr,(h,x):xs,(h,e'):es') va
                      

                else raiseE e EHiddenArgs
    typeOfAppOne head it _  = return (it,head)

addHiddenArgs :: TCEnv -> Exp -> Value -> Maybe Value -> PCM (Value,Exp)
addHiddenArgs tchenv e v (Just t) = do 

        case transInfoExp e of
            Nothing -> return (v,e)
            Just cit -> do
                solveCs (|-) 
                t <- unfold t              
                if isVProd t 
                   then do (_,(xs,_),_) <- splitFun t
                           if fst (head xs) -- first binding is hidden 
                              then return (v,e)
                              else addHiddenArgs' tchenv e v cit
                   else addHiddenArgs' tchenv e v cit
addHiddenArgs tchenv e v Nothing = 
        case transInfoExp e of
            Nothing -> return (v,e)
            Just cit -> addHiddenArgs' tchenv e v cit
        
addHiddenArgs'  tchenv@(g,r) e v  cit = do
     solveCs (|-)
     funT <- splitFun v 
     addHidden' e v funT 
   `handle_` return (v,e)
  
  where addHidden' e v (tr,(xs,a),b) = 
              do va <- eval tr a
                 --traceM $ unwords ["typeOfHidden", ppReadable h, ppReadable es, ppReadable v]
                 (tr',xs',e') <- addMetas tr xs e va 
                 --traceM $ unwords ["typeOfHidden", ppReadable h, ppReadable es, ppReadable e,ppReadable es',ppReadable va]
                 vb <- eval tr' (eProd [(xs',a)] b)
                 if null xs' then addHiddenArgs' tchenv e' vb cit
                    else return (vb,e')
        addMetas tr []  e va = return (tr,[],e)
        addMetas tr xs@((hidden,_):_) e va | not hidden = return (tr,xs,e)
        addMetas tr (x'@(hidden,x):xs) e va  = do
                 i <- metaVarIndex
                 let pos = getExpPos e
                     e' = EMeta i pos False cit 10 Nothing
                 e' <- tchenv |- e' :! va
                 v' <- eval r e'
                 addMetas (updateEnv tr x v') xs (eApp' e [(True,e')]) va

corrBind :: TCEnv -> Bind -> PCM (TCEnv,Bind)
corrBind tchenv (xs,a)   = do 
        a' <- tchenv |- IsType a
        let b = (xs,a')
        return (addBind tchenv b,b)



chkBind :: Value -> TCEnv -> Bind -> PCM (TCEnv,Bind)
chkBind vt tchenv (xs,a) = do 
        a' <- tchenv |- a :! vt  
        return (addBind tchenv (xs,a'),(xs,a'))


correctTel :: TCEnv -> Tel -> PCM (TCEnv,Tel)
correctTel tchenv tel = do 
        (tchenv',tel') <- mapAccumLM correctTel' tchenv tel
        return (tchenv',tel')
    where correctTel' tchenv b = do
                (tchenv',b') <- corrBind tchenv b 
                return (tchenv',b')

checkTel :: TCEnv -> Value -> Tel -> PCM Tel
checkTel tchenv vt tel = do 
        (_,tel') <- mapAccumLM checkTel' tchenv tel
        return tel'
    where checkTel' tchenv b = do
                (tchenv',b') <- chkBind vt tchenv b
                return  (tchenv',b')



lookupSig :: Environment       {- of sig, from splitSig -}
          -> Id                {- label to be looked up -}
          -> Folded            {- sruct val (not unfolded when called from
                                  checkOpenArgs, in the original or now) -}
          -> [ESigDef]         {- components of sig, from splitSig -}
          -> PCM Value         {- the type of the label -}

lookupSig r i v sds = luS [b | ESigAbs b <- sds] r
  where
    luS []          _ = passError ENotInSig
    luS ((hxs,a):bs) r = let xs = map snd hxs
                         in if any ((i ==) . toId) xs then eval r a
                            else luS bs =<< foldM addprj r xs
      where addprj r x = updateEnv r x `fmap` vProj v (toId x)


checkRecSig :: TCEnv            {- of struct -}
            -> [LetDef]         {- str components -}
            -> [UId]            {- free(+case-bound) vars of struct,
                                   used to make const vals out of labels -}
            -> (Position, Environment, [ESigDef])  {- splitSig -}
            -> PCM [LetDef]

checkRecSig tchenv@(g,r) ds xs (pos,tr,sds) = do
        lg <- buildRecTypes $ domVisibleLetDef ds
        (_,ds) <- mapAccumLM (checkLetDef lg tchenv) [] ds 
        return ds

  where
    buildRecTypes :: 
                  [UId]         {- str labels   -}
                  -> PCM Context   {- their typing, with which
                                      let defs in struct must agree.  -}
    buildRecTypes ls = do                           
        (_,(lg,ls')) <- foldM bRTBind (tr,([],ls)) [b | ESigAbs b <- sds]
        unless (null ls') $ mkErrMany ls'
        return lg
      where
        bRTBind :: (Environment,(Context,[UId])) -> Bind -> PCM (Environment,(Context,[UId])) 
        bRTBind it@(tr,_) (ys,a) = do 
                va <- eval tr a
                foldM (bRT  va) it ys 
        bRT va (tr,(lg,ls)) (_,y) = do
          l <- maybe (mkErrMiss y) return $ find (eqAsId y) ls
          let tr' = updateEnv tr y $ EClos r (EConstV l xs)

          return (tr', (addC lg l va, delete l ls))

    mkErrMiss y          = raiseP pos $ EMissingField (ppR y) (gUPos y)
    mkErrMany ls'        = raiseU (head ls') $ ETooManyFields (ppR ls') pos

    {- bRT does:
       for a given sig field y::va, finds the matching str label l in
       the remaining labels ls, makes a const val out of l,
       extends current tr to tr,y=l (as val)
       for the rest of sig, collects typing info l::va in label context
       lg, and removes l from available str labels. -}




checkBranches ::(TCEnv,  
                 Exp,    {- the expression being cased -}
                 [(CaseBranch,Exp)],
                 Value,  {- the desired type of the whole expression -}
                 Value)  {- inferred type of exp being cased -}
              -> (Environment,[ConBind]) {- splitData of tx -}
              -> PCM ([(CaseBranch,Exp)])

checkBranches (tchenv@(_,r), ex, brs, t, tx) (tr,cbs)  = do
    vx  <- eval r ex
    brs' <- mapM (checkBranch vx) brs
    checkCover ex cbs (map fst brs')
    return (brs')
  where
    checkBranch vx (CBConM c pas m, bre) = do
         --let mx = [x|EVar x <- [vx]]
             --((g1,r1,_),tchenv2) = breakTCEnv tchenv mx
         ctel          <- lookupI c cbs $ ENotInType (ppR c) (ppR tx)
         (_,(g2',r2'),pas') <- bindPatArgs (tr,(tchenv,pas)) ctel c

         --traceM $ unlines [show c,"g2' "++show g2',"r2' "++ppDebug r2',"cit' "++show cit'] 
         let v         =  ECon c [(False,e) | e <- mkVars $ map getUIdPatt pas']
         r1' <- compEnv r2' (updateEnvMany emptyEnv [(x,v)|EVar x _ <- [vx]])
         let brr       =  r1' --catEnv r1' r2'

         brt           <- eval brr t
         --traceM (unlines [show c, "g2' "++show (g2'),"brr "++ppDebug brr,"cit' "++show cit',ppDebug bre,ppDebug brt])
         bre' <- (g2',brr) |- bre :! brt -- obs un-re-evaluated g1
         return ((CBConM c pas' m, bre'))

    checkBranch _ _ = internalError ("checkBranch")

bindPatArgs :: (Environment,     {- tr = that of constr tel    -}
                (TCEnv,          {- tchenv = that of pattern       -}
                [PatArg]))
            -> Tel               {- constr tel                 -}
            -> Id                {- constr, only for error msg -}
            -> PCM (Environment,TCEnv,[PatArg]) {-extended tr, tchenv + solutions to meta variables     -}
            
bindPatArgs (tr0,(tchenv0,pas0)) ctel c = do 
        (tr',tchenv',pas',remPats) <- foldM matchCP (tr0,tchenv0,[],pas0) ctel
        unless (null remPats) mkErrNum
        return (tr',tchenv',reverse pas')

  where
        matchCP (tr,tchenv,pas,remPas) (xs,a) = do 
                va <- eval tr a
                matchCPT va tr tchenv pas remPas xs 
        matchCPT va tr tchenv  pas remPas [] = return (tr,tchenv,pas,remPas)
        matchCPT va tr tchenv pas (pa:remPas) (x:xs) = do
                (tr',tchenv',pa') <- bPA va tr tchenv pa x
                matchCPT va tr' tchenv'  (pa':pas) remPas xs
        matchCPT _ _ _ _ _ _ = mkErrNum
        --bPA :: Value -> Envíronment -> TCEnv -> PatArg -> (Bool,UId) 
        --      -> PCM (Envíronment,TCEnv,PatArg)
        bPA va tr tchenv@(_,r) pa (_,x) = case pa of
                PArg y    -> do 
                                return (updateEnv tr x (EVar y Nothing),addBind1 tchenv y va, pa)
                PArgT y b -> do 
                        b' <- tchenv |- IsType b
                        vb <- eval r b'
                        vb =? va       `handle` mkErrTy x va vb
                        return (updateEnv tr x (EVar y Nothing),addBind1 tchenv y va,PArgT y b')

        mkErrTy x va vb err = raiseU x
                         $ EDeclaredNotEqual (ppR x) (ppR va) (ppR vb)
                         $ prNPEMsg err
    

        mkErrNum = raiseI c . EMissingPatArg $ ppR c

    {- bPA does: for one var x::va from ctel, matches it with one var y[::b]
       in pa, extends tr to tr,x=Evar y, tchenv to g,y::(va or b); y= Evar y,
       and consumes pa.  -}

  
checkCover :: Exp  {- the exp being cased, just for error message -}
           -> [ConBind] -> [CaseBranch] -> PCM ()
checkCover ex cbs brs = do
    let is = map fst cbs
        js = map getIdBr brs
        dcs = findDup js
        mcs = is \\ js
    unless (null dcs) $ raiseI (head dcs) EOverlap
    unless (null mcs) $ raiseE ex $ EMissingConstrs $ map ppR mcs

{- a dense version of the original algo .. 
checkCover _ [] [] = done
checkCover _ [] (pa: _)  = raiseI (getIdBr pa) EOverlap
checkCover ex ((c,_):cs) pas = let h = checkCover ex cs in
    mcs <- maybe (return . (c:) =<< h pas) h $ lookup_c pas
    unless (null mcs) $ raiseE ex $ EMissingConstrs $ map ppR mcs
  where lookup_c [] = Nothing
        lookup_c (pa:pas) = 
          (if c == getIdBr pa then Just else liftM (pa:) . lookup_c) pas
-}


checkOpenArgs :: TCEnv
              -> [UId]       {- abs consts so far found -}
              -> Exp         {- exp being opened -}
              -> OpenArgs 
              -> PCM ([UId],Exp,OpenArgs) {- further accum of abs consts -}

checkOpenArgs tchenv@(_,r) acs m (OpenArgs oas xs) = do
        (tm,m') <- inferType tchenv m
        vm          <- eval r m'
        typer       <- mkTyper tm vm
        (acs',oas') <- checkAll tchenv typer m' acs  xs oas
        return (acs',m',OpenArgs oas' xs)
  where
    {- For vm sig or package, returns a function that sends label to
       its expected type. -}
    mkTyper:: Value -> Value -> PCM (Id -> PCM Value)
    mkTyper tm vm = do
        solveCs (|-)
        tm <- unfold tm
        if (isVPackage tm) then do -- reove this test
            (pos,tr,_,cs,_) <- splitPackage vm `handle_` mkErrSpl
            let typer n     = eval tr . typeOfDef =<< def
                            =<< (lookupProj n cs `handle_` mkPrjErr)
                 where mkPrjErr = raiseI n $ ENotInRecord (ppR n) (ppR vm) pos
            return typer
         else do
            (_,tr,sds) <- splitSig tm `handle_` mkErrSpl
            let typer n = lookupSig tr n vm sds `handle_` mkNotField
                 where mkNotField = raiseI n $ ENotProductElem(ppR tm)(ppR n)
            return typer
      where mkErrSpl = raiseE m ENotProductOpen

    checkAll :: TCEnv -> (Id -> PCM Value) -> Exp -> [UId] -> [UId] -> [OpenArg]  -> PCM ([UId],[OpenArg])
    checkAll tchenv typer m acs _ [] = return (acs,[])
    checkAll tchenv typer m acs xs (oa:oas) = do
        (acs',oa') <- mkCheckerOA typer m acs xs oa
        (acs2,oas') <- checkAll tchenv typer m acs' xs oas
        return (acs2,oa':oas')
    mkCheckerOA :: (Id -> PCM Value) -> Exp -> [UId] -> [UId] -> OpenArg  -> PCM ([UId],OpenArg)
    mkCheckerOA typer m'  acs xs (OpenConst ps c) = 

              do let n = toId c 
                 t <- typer n
                 addDef c  $ mkEDef False False ps c xs [] t (EProj m'  n)
                 let acs' = if (Eabstract `elem` ps) then (c:acs) else acs
                 return (acs',OpenConst ps c)
    mkCheckerOA typer m'  acs xs (OpenConstAs ps n c) = 
              do t <- typer n

                 addDef c  $ mkEDef False False ps c xs [] t (EProj m'  n)
                 let acs' = if (Eabstract `elem` ps) then (c:acs) else acs
                 return (acs',OpenConstAs ps n c)
    mkCheckerOA typer m'  acs xs (OpenConstT ps c a) = do
              do let n = toId c
                 t <- typer n
                 a' <-  chkGvnTyp tchenv c t a
                 addDef c  $ mkEDef False False ps c xs [] a' (EProj m'  n)
                 let acs' = if (Eabstract `elem` ps) then (c:acs) else acs
                 return (acs',OpenConstT ps c a')

    mkCheckerOA  typer m'  acs xs (OpenConstAsT ps n c a) = do
             do  t <- typer n
                 a' <-  chkGvnTyp tchenv c t a
                 addDef c  $ mkEDef False False ps c xs [] a' (EProj m'  n)
                 let acs' = if (Eabstract `elem` ps) then (c:acs) else acs
                 return (acs',OpenConstAsT ps n c a')

    chkGvnTyp tchenv@(g,r) c t a    = do 
        a'  <- tchenv |- IsType a
        va <- eval r a'
        va =? t         `handle` mkTypErr c t a
        return a'

    mkTypErr c t a =
            raiseU c . EDeclaredNotEqual (ppR c) (ppR a) (ppR t) . prNPEMsg



{- consider something like
type DefG = ([EProp],UId,(Maybe(Tel,Exp)),Maybe Exp)
-}



checkLhs :: TCEnv
         -> Context     {- expected label typing, when checking struct -}
         -> Def         
         -> PCM (TCEnv,Def)   {- tchenv extended with the tel of def             -}

checkLhs tchenv@(g,r) lg (Def blocked rec ps c xs tel a rhs) =
    bdy `handle` mkLetDefError "type" c
  where
    bdy = do
        (tchenv'@(g',r'),tel')  <- correctTel tchenv tel `handle` mkLetDefError "telescope" c
        a' <- if a == EPackageType then return EPackageType else  tchenv' |- IsType a
        maybe done (chklbltyp tel' a') $ lookupC c lg
        addDef c (mkPN blocked rec ps c xs tel' a')
        return (tchenv',Def blocked rec ps c xs tel' a' rhs)

      where
        chklbltyp :: Tel -> Exp -> Value -> PCM ()
        chklbltyp tel a va' = do va2 <- eval r (eProd tel a) 
                                 va2 =? va' `handle` mkErr va' va2 
    mkErr va' va err  = raiseU c $
                        EDeclaredNotEqual (ppR c) (ppR a) (ppR va')
                        $ prNPEMsg err


checkLhs tchenv lg (UnTypedDef blocked rec ps c xs rhs) = do
        tv <- guessTyp   `handle` mkLetDefError "type" c -- Replace with a meta-variable
        addDef c  (mkPN blocked rec ps c xs [] tv) 
        return (tchenv,UnTypedDef blocked rec ps c xs rhs)

  where
    guessTyp = maybe dflt (\v -> return v) $ lookupC c lg
      where dflt = case rhs of 
                        DExp e -> do (t,e') <- inferType tchenv e
                                     return t
                        _ -> raiseU c $ ECanNotInferType (pprUId c)
                        
           
checkLhs  _ _ (DOpen e _)              = raiseE e ENoOpenInMutual 


checkRhs :: [UId]       {- abs consts so far checked, to be exposed
                           if necessary                             -}
         -> (TCEnv, Def)
         -> PCM ([UId],Def)   {- abs con list extended by those checked now -}

checkRhs acs (tchenv, d@(Def _ _ _ c _ _ _ PN)) = if (isAbstract d)
                                              then return (c:acs,d)
                                              else return (acs,d)



checkRhs acs (tchenv, d@(Def _ _ _ c _ _ _ Native)) = 
                                          updateDef c d >> 
                                          if (isAbstract d)
                                              then return (c:acs,d)
                                              else return (acs,d)

checkRhs acs (tchenv, d@(Def blocked rec ps c xs tel a (DExp e))) =
    bdy `handle` mkLetDefError "definition" c
  where
    bdy = do let (tchenv'@(_,r'),acs') = exposeAbsIf (isAbstract d)
             va <- eval r' a
             e' <- tchenv' |- e :! va
             let d' = Def blocked rec ps c xs tel a (DExp e')
             -- traceM (ppDebug d')
             updateDef c d'
             return (acs',d') 
    exposeAbsIf b = if b then (foldl addAbsConst tchenv acs, c:acs) else (tchenv, acs)

checkRhs acs (tchenv,(UnTypedDef _ _ _ c _ (DExp e))) = do
    d <- def c             {- which is Def ... by checkLhs, so the below -}
                           {- does not loop. ugh. -}
    (acs',d') <- checkRhs acs (tchenv, updateRhsDef d (DExp e))
    return (acs',unTypeDef d')

checkDef:: TCEnv
        -> Context        {- expected label typing, when checking struct -}
        -> [UId]          {- abs consts so far found in enclosing LetDefs -}
        -> Def            
        -> PCM ([UId],Def)      {- abs con list extended by those found now -}

checkDef tchenv _ acs (DOpen e as) = do 
        (acs,e',as') <- checkOpenArgs tchenv acs e as                
        return ( acs,DOpen e' as')


checkDef tchenv lg acs d           = do 
        (tchenv',d') <- checkLhs tchenv lg d
        --traceM (ppDebug d')
        --traceM (show cit2)
        (acs,d2) <- checkRhs acs (tchenv',d')
        return (acs,d2)
        

mkLetDefError :: String -> UId -> EMsg -> PCM a
mkLetDefError s c err@(pos,msg) = case msg of
    (EInLetDef _ _ _) -> raise err
    _                 -> raise (pos, EInLetDef (ppR c) s (prError msg))       


checkLetDef :: 
               Context {- expected label typing and symtab-}
            -> TCEnv
            -> [UId]    {- list of abs con so far found in enclosing LetDefs -}
            -> LetDef
            -> PCM ([UId],LetDef) {- that extended by those found now-}
checkLetDef lg tchenv acs (DSimple d)  = do 
        (acs',d') <- checkDef tchenv lg acs d
        solveCs (|-)
        --traceM ("checkLetDef " ++ ppDebug d ++ " got "++ ppDebug d')
        return (acs',DSimple d')

checkLetDef lg tchenv@(g,r) acs (DMutual ds) = do 
        des <- mapM (checkLhs tchenv lg) ds
        (acs',ds') <- checkMutRhs acs des
        solveCs (|-)
        return (acs',DMutual ds')
  where --checkMutLhs lg tchenv (d:ds) = do
        --        (tchenv',d') <- checkLhs tchenv lg d
        --        rhs' <- checkMutLhs lg tchenv ds
        --        return ((tchenv',d'):rhs')
        --checkMutLhs lg tchenv [] = return []
        checkMutRhs acs [] = return (acs,[])
        checkMutRhs acs0 (envd:des) = do
                (acs1,d') <- checkRhs acs0 envd 
                (acs2,ds') <- checkMutRhs acs1 des
                return (acs2,d':ds')


checkLetDefs :: TCEnv
             -> [LetDef]
             -> PCM ([UId],[LetDef])


checkLetDefs tchenv ds = do 
        (acs,ds) <- mapAccumLM (checkLetDef emptyC tchenv) [] ds 
        return (acs,ds)

{- -------- -}
chkIndDef :: TCEnv
          -> Tel            {- the idata telescope, [] if omitted. -}
          -> [IndConBind]
          -> Value          {- desired type -}
          -> PCM (Tel,[IndConBind])


chkIndDef tchenv@(_,r) [] cbs t = do
        (ttel',sv) <- un_eProd t

        --traceM (ppReadable t ++ "\n" ++ show ttel')
        getSort sv `handle` mkErrC
        --(_,mes,ttel2) <- correctTel tchenv ttel
        --traceM ("Part 2: "++ show ttel ++ "\nGot 2: " ++ show ttel2)
        --eqTel0 (ttel',emptyEnv) (ttel2,r)
        cbs' <- mapM (chkIndCb ttel' sv) cbs
        return ([],cbs')
  where
    chkIndCb ttel' sv ((i,contel),es) = do
        (tchenv',contel') <- mapAccumLM (chkBind' sv) tchenv contel
        (_,_,es') <- fitsTel (r,tchenv',es) ttel' `handle` mkErrB i
        return ((i,contel'),es')
     
    chkBind' sv tchenv icb = do 
        (tchenv',icb') <-  chkBind sv tchenv icb
        return (tchenv',icb')       
            
    mkErrA err   = raiseE t $ ETEMP $
                   "the type does not match the idata telescope.\n"
                   ++prNPEMsg err
    mkErrB i err = raiseI i $ ETEMP $
                   "the indexes for the type of constructor "++ppR i
                   ++" does not fit the idata telescope.\n"
                   ++ if isPassMsg err then "too many / too few exps"
                       else prEMsg err

    mkErrC err   = raiseE t $ ETEMP$
                   ppR t ++ " is not a function type to a sort.\n"
                   ++ prNPEMsg err



{- given (tr,(tchenv@(g,r),es)) tel, 
   checks if es under tchenv fits tel under tr.
   returns extended r and tr.
   ASSUMES THAT THE TEL IS CORRECT -} 

fitsTel :: (Environment,     {- of tel                -}
            TCEnv,          {- of exps               -}
             [(Bool,Exp)])         {- the exps to be fitted -}
        -> Tel               {- the tel               -}
        -> PCM (Environment, {- of tel extended       -}
                Environment, {- of es  extended       -}
                [(Bool,Exp)])

fitsTel it tel = do ((tr',(_,r'),es'),ess) <- mapAccumLM fit it tel
                    unless (null es') $ passError ETooMany
                    return (tr',r',concat ess)
  where
    fit it@(tr,_,_) (xs,a) = do 
        va <- eval tr a
        mapAccumLM (fT va) it xs

        
    fT va (tr,tchenv@(g,r),(h,e):es) (h',x) = 
        if h == h' 
           then do
                e' <- tchenv |- e :! va
                v <- eval r e'
                return ((updateEnv tr x v,(addC g x va, updateEnv r x v),es),(h,e'))

           else  case e of 
                   (EMeta _ _ _ _ _ (Just aut)) | aut ->  fT va (tr,tchenv,(h',e):es) (h',x)
                   _                           -> raiseE e EHiddenArgs

    
    fT _ _ _                       = passError ETooFew




chkIndCase:: (TCEnv,   {- tchenv -}
              Exp,     {- the exp being cased -}
              [(CaseBranch,Exp)],
              Value,   {- the desired type of the case expression -}
              Value)   {- infered type of exp being cased -}
          -> IndSplit  {- its splitIndData -}
          -> PCM ([(CaseBranch,Exp)])

{- tidy up later -}
chkIndCase  (tchenv@(_,r), ex, brs, t, tx) spl@((tr,ttel,cbs),hvs) = do
    --traceM (show ex)
    --traceM.ppDebug =<<  eval r ex 
    ux         <- getCasedVarUId  =<< eval r ex
    let (hs,vs) = unzip hvs
    ---traceM (show vs)
    us         <- getBaseVarUIds vs
    let tchenvs    =  breakTCEnv tchenv (ux:us)
    {-
    --traceM'$ let ((g1,r1),(g2,r2)) = tchenvs
        in        
        "chkIndCase:tchenv="++ppDebug tchenv
        ++"\ntr="++ppDebug tr
        ++"\nvs="++ppDebug vs
        ++"\n(g1,r1)="++ppDebug(g1,r1)
        ++"\n(g2,r2)="++ppDebug(g2,r2)
    -}
    mapM_ (chkNoDep us) (map snd $ listEnv tr)
    let chkIndBranch' = chkIndBranch ux us tchenvs
    brs' <- mapM chkIndBranch' brs
    checkCover ex (map fst cbs) (map fst brs')
    return brs'

  where
    {- to try the const way, put back in the addDef
       and change _ to (Just (x',xs)). -}
    chkIndBranch ux us ((g1,r1),tchenv2) (CBConM c pas m, bre) = do
        (ctel,hes)        <- lookupCon c cbs
        let (hs,es) = unzip hes
        (tr',(g2',r2'),pas')  <- bindPatArgs (tr,(tchenv2,pas)) ctel c `handle` mkExcuse
                                         --  ^^^ wrong for PatArgT, but.
        let ex'          = ECon c (zip hs  (mkVars $ map getUIdPatt pas'))
                           --ECon c . map (EVar . getUIdPatt) $ pas
        --addDef x'        $ mkEDef [Eprivate,Econcrete] x' xs [] tx ex'
        es'              <- mapM (eval tr') es
        r1' <- compEnv r1 $ updateEnvMany emptyEnv $ (zip us es')++[(ux,ex')]
        --traceM'$"chkIndBranch:(r1,r1')=\n"++ppDebug (r1,r1')
        let brr          = catEnv r1' r2'
                            
        brt              <- eval brr t
        bre' <- (g1++g2', brr) |- bre :! brt         -- obs un-re-evaluated g1
        return (CBConM c pas' m,bre')

    chkIndBranch _ _ _ _ = internalError "chkIndBranch: real internal"


    getCasedVarUId vx  = getVarUId vx `handle_` mkErr
       where mkErr     = raiseE ex $ ETEMP $ "case on non-var "++ppR ex
                         
    getBaseVarUIds vs2 = do unless (all isVar vs2) mkErr
                            us <- mapM getVarUId vs2
                            unless (null $ findDup us) mkErr
                            return us
       where mkErr     = raiseE ex $ ETEMP $
                         "cannot case: exps "++ppR vs2
                         ++" must be distinct variables."
 
    {- takeitout takeitout-}
    chkNoDep:: [UId] -> Value -> PCM ()
    chkNoDep ys v = cND v where
        cND (EVar x _)    = when (x `elem` ys) $ mkErrD x
        cND (EApp h vs) = mapM_ cND (h:map snd vs)

        cND (EClos r e) = mapM_ cND (map snd $ listEnv r)
        cND (EStop m e) = cND e
        cND (ECon _ vs) = mapM_ cND (map snd vs)
        cND (EProj e _) = cND e
        cND whatelse    = mkErrD1 whatelse
 
        mkErrD x = raiseE ex $ ETEMP $
                    "cannot case: the inductive family\n"++ppDebug v
                    ++"\ndepends on / may depend on "++ppR x
        mkErrD1 wh = raiseE ex $ ETEMP $
                      "cannot case: could not verify " ++ppR v
                      ++" is independent of "++ppR ys
                      ++"\ncND:whatelse = "++ppDebug wh

    lookupCon c cbs    = lookupI c (map tup_ass cbs) $
                         ENotInType (ppR c) (ppR tx)

    mkExcuse err@(_, EInternal _) =
         internalError $ "typed pattern args not yet:\n"++prEMsg err
    mkExcuse err             = raise err




{-
simplifyTC :: TypingConstraint -> PCM ()
simplifyTC (TypingConstraint m tchenv j) = do
    uninst <- isUninstantiated m
    if uninst then addTypingConstraint m tchenv j 
       else do j' <- getJudg j
               tchenv |- j'
               done

simplifyTCs :: PCM ()
simplifyTCs = do 
    cs <- delAllTypingConstraints
    mapM simplifyTC cs
    done
-}

{- ------- things that shouldn't be here -------------- -}


{- should be in Eval.hs? -}


getVarUId::Value -> PCM(UId)
getVarUId v = case v of
    (EVar x _) -> return x
    _        -> raiseE v $ ETEMP
                $ "getVarUID:" ++ppR v ++"\nshould be a variable."
                                       
{- the result can be used only assuming that eval can handle values. -}
{- THIS RELIES ON THAT WE CONFUSE A VAR AND A GENERIC VALUE,
     i.e., (Pi(x:A)B)[rho] = Pi(x:A[rho])(B[rho,x=x])                -}

un_eProd::Value       {- the type to be unwound -}
        -> PCM(Tel,   {- EProd prefixes, with already evaled types -}
               Value) {- non EProd value type -}
un_eProd t = maybe (return ([],t)) doProd =<< mkMaybeError (splitFun t)
  where
    doProd (tr,(xs,a),b) =  do
        va        <- eval tr a
        (tel',t') <- un_eProd =<< eval (addHIdEnv tr xs xs) b
        return ((xs,va):tel', t')

gUPos = getUIdPosition

ppR::PPrint a => a -> String
ppR = ppReadable

-- should be in ..ProofMonad?
raiseP::Position -> ErrMsg -> PCM a
raiseP pos emsg = raise $ eMsg pos emsg

raiseU::UId -> ErrMsg -> PCM a
raiseU x emsg = raiseP (gUPos x) emsg

raiseE::Exp -> ErrMsg -> PCM a
raiseE e emsg = raise $ eMsg (getExpPos e) emsg

eMsgI::Id -> ErrMsg -> EMsg
eMsgI i emsg = eMsg (getIdPosition i) emsg

raiseI::Id -> ErrMsg -> PCM a
raiseI i emsg = raise $ eMsgI i emsg

prNPEMsg::EMsg -> String
prNPEMsg err = if isPassMsg err then "" else prEMsg err

lookupI i l m = liftMaybeSTM (lookup i l) (eMsg (getIdPosition i) m)




{- -------------------- -}

{- for minor speed up:
   reorganise ISyntax:e.g., OpenArg,Def
   telfoldM: let f take care of updateEnv
   buildRecTypes: find, delete.
-}
