{-|

  A faster type checker which cannot handle metavars.
  Substitute for interface files.

-}
module Import(importLetDefs) where

import AgdaTrace
import ISyntax
import Equal
import Eval
import Monad
import Monads
import Position
import ProofMonad
import Error
import Id (getUIdPosition)
import PPrint(ppReadable,pIText,PDetail (..),ppDebug,(^.))
import List (deleteBy,find)
import Typechecking(inferType,TCEnv,mkLetDefError)
import Utilities (foldlM,tup_ass)
import MiscId (boolId)
import Maybe (fromJust)
import Literal
-- Dec 1999     made compatible with the ghc syntax 


infix 5 |- 


-- type TCEnv = (Context,Environment)



{- main routines: (|-) and inferType -}

(|-) :: TCEnv -> Judgement Exp -> PCM ()

tchenv |- IsType e@(ESort  _ _ )      = return ()


tchenv |- IsType (EProd bind b) = do 
        tchenv' <- corrBind tchenv bind 
        tchenv' |- IsType b

tchenv |- IsType (EArrow h a b) = do
        tchenv |- IsType a
        tchenv  |- IsType b

tchenv |- IsType (EDef ds e)        = do 
	checkLetDefs tchenv  ds
	tchenv |- IsType e
	

tchenv@(g,_) |- IsType e                  = importType tchenv e

tchenv |- (e@(ESort pos k) :! t)    = return () 
                                      
tchenv |- (e@(EProd bind b) :! t)= do     
        tchenv' <- chkBind t tchenv bind
        tchenv' |- b :! t

tchenv |- (e@(EArrow h a b) :! t)= do     
        tchenv |- a :! t
        tchenv  |- b :! t

tchenv@(_,r) |- (e@(EAbs (xs,a) b) :! t) = do
	tchenv |- IsType a
        (tchenv',tb') <- checkAbs a xs tchenv t
    	tchenv' |-  b :! tb'
  where
    checkAbs a [] tchenv t  =
	   return (tchenv,t)
    checkAbs a  xs tchenv t = do
	(tr, (ys,ta), tb) <- splitFun t 
        let n = min (length xs) (length ys)
        let (xs0,xs1) = splitAt n xs
	    (ys0,ys1) = splitAt n ys
	    tchenv' = addBind tchenv (xs0,a)
        tr' <- buildEnv tr xs0 ys0
    	tb' <- eval tr' (eProd [(ys1,ta)] tb)
        checkAbs a xs1  tchenv' tb' 

    buildEnv tr ((h,x):xs) ((h',y):ys) =  
		if isDummyUId y then buildEnv tr xs ys
		   else buildEnv (updateEnv tr y (EVar x Nothing)) xs ys

    buildEnv tr _ _ = return tr 


tchenv |- (EDef ds e :! t)        = do 
	checkLetDefs tchenv ds
	tchenv |- e :! t

tchenv |- (e@(ECon i args) :! t)   = do 
        mdt <- splitDataOrInd t 
        either chkCon chkIndCon mdt
  where
    chkCon (tr, cbs)          = do 
        ctel <- lookup_i cbs 
        fit_args tr ctel
        return ()
    chkIndCon (spl@((tr, ttel, cbs), vs)) = do
        (ctel, es)    <- lookup_i (map tup_ass cbs)
        fit_args tr ctel        
        return ()
    fit_args tr ctel = fitsTel (tr,tchenv,args) ctel
    lookup_i cbs     = lookupI i cbs 

  


tchenv |- (ECase ex brs :! t) = do
    (tx,_,_)  <-  inferType tchenv ex
    --traceM'$"|-ECase: tx="++ppDebug tx
    let it = (tchenv, ex,brs, t, tx)
    spl <- splitData tx 
    --traceM'$"|-ECase: spl="++ppDebug spl
    checkBranches it spl
   
                     

tchenv |- (EData cbs :! t)      = do     
        mapM (checkTel tchenv t) (map telCB cbs)
        done

tchenv |- (e :! t)                   = do 
	importType tchenv e

importType :: TCEnv  -> Exp -> PCM ()

importType tchenv  e@(EVar x mcit)   = done

importType (_,r) e@(EConst c m) = done

importType tchenv@(_,r) (EConF i t []) = do
	tchenv |- IsType t

importType tchenv (EConF i t es) = importType tchenv (EApp (EConF i t []) es)


importType  tchenv (EApp h es) = do 
        (vt,_,_) <- inferType tchenv h
        typeOfApp tchenv h es vt 


{- right, you need for tchenv |- IsType a to return a sort.
importType tchenv (EProd (xs,a) e) = do
-}
   

importType _ e                  = internalError (ppReadable e)

{- support routines in no particular order. -}


typeOfApp              :: TCEnv
                       -> Exp              {- head -}           
                       -> [(Bool,Exp)]     {- args -}
                       -> Value            {- type of head -}
                       -> PCM ()

typeOfApp _  head [] v = done
typeOfApp tchenv@(_,r) head es v = do
    (tr,(xs,a),b) <- splitFun v 
    va <- eval tr a
    ((tr',xs',es'),e) <- typeOfAppOne head (tr,xs,es) va
    if (null es') then done
                  else do vb <- eval tr' b
                          typeOfApp tchenv e es' vb
  where 
    typeOfAppOne mes (tr,(h,x):xs,es'@((h',e):es)) va = do 
                  tchenv |- e :! va
                  v' <- eval r e
                  typeOfAppOne  (eApp' head [(h',e)]) ((updateEnv tr x v'),xs,es) va
    typeOfAppOne head it _  = return (it,head)


corrBind :: TCEnv -> Bind -> PCM TCEnv
corrBind tchenv b@(xs,a)   = do 
	tchenv |- IsType a
        return (addBind tchenv b)

chkBind :: Value -> TCEnv -> Bind -> PCM TCEnv
chkBind vt tchenv b@(xs,a) = do 
	tchenv |- a :! vt
        return (addBind tchenv b)

correctTel :: TCEnv -> Tel -> PCM TCEnv
correctTel tchenv tel = do 
        foldM corrBind tchenv tel

checkTel :: TCEnv -> Value -> Tel -> PCM TCEnv
checkTel tchenv vt tel = do 
        foldM (chkBind vt) tchenv tel
             

checkBranches ::(TCEnv,  
                 Exp,    {- the expression being cased -}
                 [(CaseBranch,Exp)],
                 Value,  {- the desired type of the whole expression -}
                 Value)  {- importred type of exp being cased -}
              -> (Environment,[ConBind]) {- splitData of tx -}
              -> PCM ()

checkBranches (tchenv@(_,r), ex, brs, t, tx) (tr,cbs)  = do
    vx  <- eval r ex
    mapM (checkBranch vx) brs
    done
  where
    checkBranch vx (CBConM c pas m, bre) = do
         ctel          <- lookupI c cbs 
	 (_,(g2',r2')) <- bindPatArgs (tr,(tchenv,pas)) ctel c
         let v         =  ECon c [(False,e) | e <- mkVars $ map getUIdPatt pas]
	 r1' <- compEnv r2' (updateEnvMany emptyEnv [(x,v)|EVar x _ <- [vx]])
	 let brr       =  r1' --catEnv r1' r2'
         brt           <- eval brr t
         (g2',brr) |- bre :! brt -- obs un-re-evaluated g1

    checkBranch _ _ = internalError ("checkBranch")

bindPatArgs :: (Environment,     {- tr = that of constr tel    -}
                (TCEnv,          {- tchenv = that of pattern       -}
                [PatArg]))
            -> Tel               {- constr tel                 -}
            -> Id                {- constr, only for error msg -}
            -> PCM (Environment,TCEnv) {-extended tr, tchenv + solutions to meta variables     -}
            
bindPatArgs (tr0,(tchenv0,pas0)) ctel c = do 
        (tr',tchenv',_) <- foldM matchCP (tr0,tchenv0,pas0) ctel
        return (tr',tchenv')

  where
        matchCP (tr,tchenv,remPas) (xs,a) = do 
                va <- eval tr a
                matchCPT va tr tchenv remPas xs 
        matchCPT va tr tchenv remPas [] = return (tr,tchenv,remPas)
        matchCPT va tr tchenv (pa:remPas) (x:xs) = do
                (tr',tchenv') <- bPA va tr tchenv pa x
                matchCPT va tr' tchenv' remPas xs

    	bPA va tr tchenv@(_,r) pa (_,x) = case pa of
         	PArg y    -> do 
				return (updateEnv tr x (EVar y Nothing),addBind1 tchenv y va)
         	PArgT y b -> do 
			tchenv |- IsType b
			return (updateEnv tr x (EVar y Nothing),addBind1 tchenv y va)



{- consider something like
type DefG = ([EProp],UId,(Maybe(Tel,Exp)),Maybe Exp)
-}


checkLhs :: TCEnv
	 -> Context     {- expected label typing, when checking struct -}
	 -> Def         
	 -> PCM (TCEnv)   {- tchenv extended with the tel of def             -}

checkLhs tchenv@(g,r) lg (Def blocked rec ps c xs tel a rhs) = do
        (tchenv'@(g',r'))  <- correctTel tchenv tel
   	if a == EPackageType then return () else tchenv' |- IsType a
	addDef c (mkPN blocked rec ps c xs tel a)
        return (tchenv')

           

checkRhs :: [UId]       {- abs consts so far checked, to be exposed
                           if necessary                             -}
         -> (TCEnv, Def)
         -> PCM ([UId])   {- abs con list extended by those checked now -}

checkRhs acs (tchenv, d@(Def blocked rec ps c xs tel a (DExp e))) = do 
	     let (tchenv'@(_,r'),acs') = exposeAbsIf (isAbstract d)
	     va <- eval r' a
	     tchenv' |- e :! va
	     -- traceM (ppDebug d')
             updateDef c d
             return (acs')
    where exposeAbsIf b = if b then (foldl addAbsConst tchenv acs, c:acs) else (tchenv, acs)

checkRhs acs (tchenv,(UnTypedDef _ _ _ c _ (DExp e))) = do
    d <- def c             {- which is Def ... by checkLhs, so the below -}
                           {- does not loop. ugh. -}
    (acs') <- checkRhs acs (tchenv, updateRhsDef d (DExp e))
    return (acs')

checkDef:: TCEnv
        -> Context        {- expected label typing, when checking struct -}
	-> [UId]          {- abs consts so far found in enclosing LetDefs -}
	-> Def            
	-> PCM ([UId])      {- abs con list extended by those found now -}



checkDef tchenv lg acs d           = do 
	tchenv' <- checkLhs tchenv lg d
	checkRhs acs (tchenv',d)


checkLetDef :: 
	       Context {- expected label typing and symtab-}
	    -> TCEnv
            -> [UId]    {- list of abs con so far found in enclosing LetDefs -}
	    -> LetDef
	    -> PCM [UId] {- that extended by those found now-}
checkLetDef lg tchenv acs (DSimple d)  = do 
	checkDef tchenv lg acs d

checkLetDef lg tchenv@(g,r) acs (DMutual ds) = do 
 	des <- checkMutLhs lg tchenv ds
        checkMutRhs acs des
  where checkMutLhs lg tchenv (d:ds) = do
	     	tchenv' <- checkLhs tchenv lg d
                rhs' <- checkMutLhs lg tchenv ds
		return ((tchenv',d):rhs')
	checkMutLhs _ _ [] = return []
	checkMutRhs acs [] = return acs
	checkMutRhs acs0 (envd:des) = do
		acs1 <- checkRhs acs0 envd 
		checkMutRhs acs1 des



checkLetDefs :: TCEnv
	     -> [LetDef]
             -> PCM ([UId])


checkLetDefs tchenv ds = do 
     	foldM (checkLetDef emptyC tchenv) [] ds 


{- given (tr,(tchenv@(g,r),es)) tel, 
   checks if es under tchenv fits tel under tr.
   returns extended r and tr.
   ASSUMES THAT THE TEL IS CORRECT -} 

fitsTel :: (Environment,     {- of tel                -}
            TCEnv,          {- of exps               -}
             [(Bool,Exp)])         {- the exps to be fitted -}
        -> Tel               {- the tel               -}
        -> PCM (Environment, {- of tel extended       -}
                Environment {- of es  extended       -}
                )

fitsTel it tel = do (tr',(_,r'),_) <- foldM fit it tel
                    return (tr',r')
  where
    fit it@(tr,_,_) (xs,a) = do 
        va <- eval tr a
        foldM (fT va) it xs

	
    fT va (tr,tchenv@(g,r),(h,e):es) (h',x) = do
        	tchenv |- e :! va
		v <- eval r e
		return (updateEnv tr x v,(addC g x va, updateEnv r x v),es)
    
    fT _ _ _                       = passError ETooFew




{- ------- things that shouldn't be here -------------- -}


{- should be in Eval.hs? -}


getVarUId::Value -> PCM(UId)
getVarUId v = case v of
    (EVar x _) -> return x

                                       
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

lookupI i l = return$ fromJust (lookup i l)




{- -------------------- -}

{- for minor speed up:
   reorganise ISyntax:e.g., OpenArg,Def
   telfoldM: let f take care of updateEnv
   buildRecTypes: find, delete.
-}
           






importLetDefs :: TCEnv ->  Context -> [LetDef] -> PCM ()
importLetDefs ce gamma ds =  do acs <-  foldM (checkLetDef gamma ce) [] ds 
                                return ()
           


