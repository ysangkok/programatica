{-| 

  Translates into internal, more succinct version of abstract syntax
  to simplify type-checking. Makes all identifiers and meta-variables unique.
  Checks that all variables and constants are in scope.

-}

module CITranslate where

import AgdaTrace
import Monad
import Util(findDupWith,findDup)
import BinParse(prec)
import PPrint(ppReadable,ppDebug)
import Position
--import AgdaScans
import ISyntax
import CSyntax
import Monads
--import Gensym
import CITrans(initCIT,CITrans,addCst,addVar,varScope,lookupId,updateCCPV,addCsts,addPV,scope)
import Error
import NoRec(noRec)
import ProofMonad
import Maybe(catMaybes)
import AgdaScans
import ClassEnv
import BinParse(Fixity(..))
--import Id(toTmpUId,getUIdPosition)
--import PreStrings(preStrTable)




toDummyUIdB :: (Bool,Id) -> (Bool,UId)
toDummyUIdB (h,x) = (h,toDummyUId x)



rec :: Bool 
rec = not noRec

-- Kolla dubletter

checkDup :: [UId] -> TM ()
checkDup xs = let ss = findDupWith eqAsId xs
              in if null ss
                    then return ()
                    else let (c1:c2:_) = ss
                         in raise $ eMsg (getUIdPosition c1) (EDuplicate (getUIdPosition c2) (pprUId c1))

makeUId :: Id -> PCM UId
makeUId i = do n <- idIndex
               return (toUId i n)

addConstant :: TransClass -> Id  -> PCM (TransClass,(UId,FCVars))
addConstant (cit,classEnv) i = do 
        c <- makeUId i
        let cit' = addCst i c cit 
            vs = varScope cit
        return ((cit',classEnv),(c,vs))


addConstant' :: (Id,UId) -> TransClass -> TransClass
addConstant' (i,c) (cit,classEnv) = 
        let cit' = addCst i c cit 
        in (cit',classEnv)

{-
addConstants :: TransClass -> [(Id,UId)] -> PCM TransClass
addConstants cit ccs = foldr addConstant' cit ccs 
  -}                                              

addVariable :: Exp -> TransClass -> Id -> PCM (TransClass,UId)
addVariable a (cit,classEnv) i = do 
        x <- makeUId i
        let cit' = addVar i x cit 

        classEnv' <- addInstance classEnv x a
        --traceM ("addVariable "++ ppDebug classEnv ++ " "++ppReadable x ++ " " ++ ppDebug a ++ " got " ++ ppDebug classEnv')
        return ((cit',classEnv'),x)


addVariable' :: TransClass -> Id -> PCM (TransClass,UId)
addVariable' (cit,classEnv) i = do 
        x <- makeUId i
        let cit' = addVar i x cit 
        return ((cit',classEnv),x)

{-                        
addVariable' :: CITrans -> Id -> UId -> PCM CITrans
addVariable' cit i x = do 
                       let cit' = addVar i x cit 
                       return cit'

-}

updateTCDef:: 
               Bool       -- recursive? 
            -> Tel
            -> [(Id,UId)] -- mutuals
            -> TransClass    -- cit before tel
            -> TransClass    -- cit after  tel
            -> TransClass

updateTCDef rec tel ccs@((n,c):_) (cit0,_) (cit1,classEnv) = 
  (updateCCPV c ys $ citx,classEnv) where   -- this is wrong for rec. funct.
    ys    = concatMap getVarsBind tel
    citx  = if rec  then cit0' else cit1 
    cit0' = foldl addVar' (addCsts ccs cit0) ys
    addVar' = flip (\ y -> addVar (toId y) y)


mkMeta ::  Position -> ParseInfo -> TransClass -> Int -> Visibility -> PCM Exp
mkMeta pos pai tc pi aut = do 
        i <- metaVarIndex
        return (EMeta i pos pai tc pi aut)


mkIdExp :: TransClass -> Id ->  PCM Exp
mkIdExp tc@(cit,classenv)  n = do 
        r <- liftESTM (lookupId cit n)
        case r of 
            Left x -> return (EVar (putUIdPosition x (getIdPosition n)) (Just tc))
            Right c -> return (EConst (putUIdPosition c (getIdPosition n)) (Just tc))



toIProg :: TransClass -> CProgram -> PCM (TransClass,Program)
toIProg tc (CProgram ms) =  do 
        (tc' ,ds') <- mapMAccumL toIModule tc ms
        let ds2 = concat ds'
        checkDup $ concatMap idsLetDef ds2
        return (tc',Program ds2)


toIModule :: TransClass ->  CModule -> PCM (TransClass,[LetDef])
toIModule  tc (CModule i as b) = 
       toILetDef  False tc (CSimple (CDef [Cabstract] (CPackage i as b )))

toIProp :: CProp -> EProp
toIProp Cprivate = Eprivate
toIProp Cpublic = Epublic
toIProp Cabstract = Eabstract
toIProp Cconcrete = Econcrete

toIProps :: [CProp] -> [EProp]
toIProps [] = []
toIProps [p] = [toIProp p]
toIProps (p:ps) = let p' = toIProp p
                      ps' = toIProps ps
                  in if elem p' ps'
                        then ps'
                        else case p' of
                                Eabstract | elem Econcrete ps' -> ps'
 --raise (noPosition,EConflictingModifiers (ppReadable p) "concrete")
                                Econcrete | elem Eabstract ps' -> ps'
 --raise (noPosition,EConflictingModifiers (ppReadable p) "abstract")
                                Epublic | elem Eprivate ps' -> ps'
 --raise (noPosition,EConflictingModifiers (ppReadable p) "private")
                                Eprivate |  elem Epublic ps' -> ps' 
 --raise (noPosition,EConflictingModifiers (ppReadable p) "public")

                                _ -> (p':ps')



exportSummand :: Id -> CArgs -> CSummand -> CDefn
exportSummand c as (c',as') = 
     let a = cApply (CVar c) (concatMap mkPar as)
         e = cApply (CCon c' a) (concatMap mkPar as')
         as2 = map mkHiddenArg as
     in CValueT c' (as2++as') a e
     where 

exportElim :: CITrans -> Id -> CArgs -> CSummand -> PCM CDefn
exportElim cit c as (c',[CArg [(h,c2)] a]) = do
     st <- getStrTable
     let (st',x) = freshId' st ((map toId $ scope cit) ++ concatMap boundVars as) "h" 
     putStrTable st
     let e = Ccase (CVar x) [(CPCon c' [(CPVar (CPatId c2))],CVar c2)]
         as2 = [CArg [(False,x)] (cApply (CVar c) (concatMap mkPar as))]
     return $ CValueT c2 ((map mkHiddenArg as)++as2) a e
   



mkPar:: CArg -> [(Bool,CExpr)]
mkPar (CArg his _) = [(b,CVar i) | (b,i) <- his] 

mkHiddenArg :: CArg -> CArg
mkHiddenArg (CArg his a) = CArg [(True,i) | (_,i) <- his ] a

exportClassMember :: Id -> CArgs -> CSign -> [CDefn]
exportClassMember c cas (CSign xs a) = 
        let args1 = map mkHiddenArg cas
            arg2 = CArg [(True,internalHypId)] (cApply (CVar c) (concatMap mkPar cas))
        in map (\x -> CValueT x (args1++[arg2]) a (CSelect (CVar internalHypId) x)) xs
exportClassMember _ _ _ = []        

   
{- toId is a necessity regardless of UId rep, so why avoid it here? -}

toIDef ::

       Bool -> 
      (TransClass,
     [(Id,UId)]) {- (mutual) ids being defed |-> uids asgned by toILetDef -}
    -> CDef -> PCM ((TransClass,[(Id,UId)]),[Def])

toIDef _ ccs (CDefComment _) = return (ccs,[])
toIDef rec ccs (CDef ps d)  = do
        (tc,d) <- toIDefn rec ps ccs  d
        return (tc,d)      

toILetDef ::  Bool -> TransClass -> CLetDef -> PCM (TransClass,[LetDef])
toILetDef  _ tc  (CLetDefComment cs) = return (tc,[])
toILetDef  rec tc (CSimple d)  = do 
        let cs = maybe [] (\d -> [d]) (idCDef d)
        cs' <- mapM makeUId cs
        ((tc2,_),d') <- toIDef rec (tc,zip cs cs') d
        return$ (tc2,map DSimple d')

toILetDef _ tc (CMutual ds) = 
         do let cs = catMaybes $ map idCDef ds
            cs' <- mapM makeUId cs
            ((tc2,_),ds') <- mapMAccumL (toIDef True) (tc,zip cs cs') ds
            return (tc2,[DMutual (concat ds')]) 


-- Kolla dubletter
toILetDefs :: Bool -> TransClass ->  [CLetDef] -> PCM (TransClass,[LetDef])
toILetDefs rec tc ds = do 
        (tc',ds') <-  mapMAccumL (toILetDef rec) tc ds
        let ds2 = concat ds'
        checkDup $ concatMap idsLetDef ds2
        return (tc',ds2)



toIlhs :: TransClass -> [CProp] -> [CArg] -> PCM (TransClass,[EProp],Tel)
toIlhs tc ps as  = do 
        let ps' = toIProps ps
        (tc',tel) <- toITel tc as 
        return (tc',ps',tel)

toIlhsT :: TransClass -> [CProp] -> [CArg] -> CType -> PCM (TransClass,[EProp],Tel,Exp)
toIlhsT tc ps as t = do 
        (tc',ps',tel) <- toIlhs tc ps as
        t' <- toIExp tc' False noPar t
        return (tc',ps',tel,t')
toIDefn ::
       Bool 
    -> [CProp]
    -> (TransClass
     ,[(Id,UId)]) {- (mutual) ids being defed |-> uids asgned by toILetDef-}
    -> CDefn
    -> PCM ((TransClass,
            [(Id,UId)]), {- the tail, unless COpen -}
           [Def])



{- MERGE. -}


toIDefn rec ps (tc,ccs@((_,c'):cs)) (CValueT c ctel a e) = do 
        (tc',ps',tel,a') <- toIlhsT tc ps ctel a
        let tc2 = updateTCDef rec tel ccs tc tc'
        e' <- toIExp tc2 True noPar e
        return ((addConstant' (c,c') tc ,cs),[mkEDef False rec ps' c' (varScope (fst tc)) tel a' e'])

toIDefn rec ps (tc@(cit,_),(nc:cs)) (Cnewtype c ctel a cb) = do 
                let d = CValueT c ctel (cSet noPosition) (CSum [cb])
                    d' = exportSummand c ctel cb
                d2 <-  exportElim cit c ctel cb
                let cs' = catMaybes (map idCDefn [d',d2])
                cs2 <- mapM makeUId cs'
                ((tc3,_),ds') <- mapMAccumL (toIDefn False ps) (tc,nc:(zip cs' cs2)++cs) [d,d',d2]
                return ((tc3,cs),concat ds')

toIDefn rec ps (tc,ccs@((_,c'):cs)) (CValueS c ctel a (CClause e)) = 
        do (tc',ps',tel,a') <- toIlhsT tc ps ctel a
           let tc2 = updateTCDef rec tel ccs tc tc'
           e' <- toIExp tc2 True noPar e
           return ((addConstant' (c,c') tc,cs),[mkEDef False rec ps' c' (varScope (fst tc)) tel a' e'])

toIDefn rec ps (tc,ccs@((_,c'):cs)) (CAxiom c ctel a) = 
        do (tc',ps',tel,a') <- toIlhsT tc ps ctel a
           return ((addConstant' (c,c') tc,cs),[mkPN  True False ps' c' (varScope (fst tc)) tel a'])

toIDefn rec ps (tc,ccs@((_,c'):cs)) (CNative c a) = 
        do (tc',ps',tel,a') <- toIlhsT tc ps [] a
           return ((addConstant' (c,c') tc,cs),[mkNative False False ps' c' (varScope (fst tc)) tel a'])

toIDefn rec ps tc_ccs (Ctype c ctel a) = 
   toIDefn rec ps tc_ccs (CValueT c ctel (cSet noPosition) a)

{-     
   do (tc',ps',tel,a') <- toIlhsT tc ps ctel a
           let tc2  = updateTCDef rec tel ccs tc tc'
           a' <- toIExp tc2 False noPar a
           return ((addConstant' (c,c') tc,cs),[mkEDef False rec ps' c' (varScope (fst tc)) tel (eSet noPosition) a'])
-}

toIDefn rec ps (tc,(nc:cs))  (Cdata c ctel mt cbs) = 
        let xs = findDup (map fst cbs)
        in if null xs 
             then do
                let t = maybe (cSet noPosition ) id mt
                    d = CValueT c ctel t (CSum cbs)
                    ds = map (exportSummand c ctel) cbs
                    cs' = catMaybes (map idCDefn ds)
                cs2 <- mapM makeUId cs'
                ((tc3,_),ds') <- mapMAccumL (toIDefn rec ps) (tc,nc:(zip cs' cs2)++cs) (d:ds)
                return ((tc3,cs),concat ds')
             else let n = head xs
                  in raise $ eMsg (getIdPosition n) (EDuplicateCon (ppReadable n)) 

toIDefn rec ps (tc,ccs@((_,c'):cs)) (CValue c e) = 
        do let ps' = toIProps ps
               tc2 = updateTCDef rec [] ccs tc tc
           e' <- toIExp tc2 True noPar e
           return ((addConstant' (c,c') tc ,cs),[mkUnTyped False rec ps' c' (varScope (fst tc))  e'])

toIDefn _ ps (tc,ccs@((_,c'):cs)) (CPackage c ctel e) = 
        do (tc',ps',tel) <- toIlhs tc ps ctel
           let tc2 = updateTCDef False tel ccs tc tc'
           e' <- toIPackageBody tc2 e
           return ((addConstant' (c,c') tc,cs),[mkEDef False False ps' c' (varScope (fst tc)) tel EPackageType e'])


toIDefn _ ps (tc,cs) (COpen e as) = do 
        e' <- toIExp tc False noPar e
        (tc',as') <- toIOpenArgs ps tc as
        return ((tc',cs),[DOpen e' as'])


toIDefn rec ps (tc@(cit,classEnv),ccs@((_,c'):cs)) (CClass (CClassArg c cas t scs) export sds) = do 
        let ds =  if export then concatMap (exportClassMember c cas) sds else []
            --d =  CValueT c cas (cSet noPosition) (CProduct noPosition (map ESigAbs supers ++ sds))
            cs' = catMaybes (map idCDefn ds)
        (tc',ps',tel,t') <- toIlhsT tc ps cas t
        let tc3@(cit',classEnv') = updateTCDef False tel ccs tc tc'
        (tc2,scs') <- toITel tc3 scs
        (_,tel') <- mapMAccumL toISign tc2 sds
        let tel2 = concat tel'
        checkDup (domTel tel ++ domESigDefs tel2)       
        let d = mkEDef False False ps' c' (varScope (fst tc)) tel t' (ESig (getUIdPosition c') ((map ESigAbs scs') ++ tel2))
        superInfo <- mapM (findSupers classEnv) scs'
        let classEnv' = addClass classEnv c' (concat superInfo)
        cs2 <- mapM makeUId cs'
        let cit' = addCst c c' cit
        ((tc3,_),ds') <- mapMAccumL (toIDefn rec ps) ((cit',classEnv'),(zip cs' cs2)++cs) ds
        return ((tc3,cs),d:concat ds')
   where findSupers :: ClassEnv -> Bind -> PCM SuperClasses
         findSupers classEnv (hxs,a) = do
                let xs = map snd hxs
                c <- checkTypes classEnv a
                let superinfo = [(x,c) | x <- xs]
                return  superinfo
         checkTypes classEnv (EApp (EConst c _) [(_,e)]) =
                               if isClass classEnv c 
                                  then case e of
                                        EVar _ _ -> return c
                                        _      -> raise $ eMsg (getExpPos e)(EClassNotVarArg (ppReadable e))
                                  else raise $ eMsg (getUIdPosition c) (ENotClass (ppReadable c))  
         checkTypes classEnv e@(EApp (EConst c _) es) = raise $ eMsg (getExpPos e) (EClassTooMany (ppReadable e)) 
         checkTypes _ e = raise $ eMsg (getExpPos e) (ENotClass (ppReadable e))
                        


toIDefn rec _ (tc@(cit,classEnv),ccs@((_,c'):cs)) (CInstance c cas ( CInstanceArg (CApply (CVar classId) es)) ds) = do
        classn' <- lookupClassName classEnv cit classId
        scsfs <- getSuperClassFields  classEnv classn'
        (tc'@(cit',classEnv'),ps',tel,a) <- toIlhsT tc [] cas (CApply (CVar classId) es)
        let tc2@(cit2,classEnv2) = updateTCDef True tel ccs tc tc'
        classEnv3 <- if rec then addInstance classEnv2 c' a else return classEnv2
        let ds' = map mkHiddenStructFields scsfs
        ((cit3,classEnv3),ds2) <- toILetDefs False (cit2,classEnv2) ds'
        classEnv5 <- addInstance classEnv3 c' a
        r <- mapM (toILetDef False (cit3,classEnv5)) ds
        let ds3 = ds2 ++ concat (map snd r)
        let vcs = domVisibleLetDef ds3 
            st = foldl (\st x -> (toId x,x):st) [] vcs
        classEnv4 <- addInstance classEnv c' a
        return $ ((addConstant' (c,c') (cit,classEnv4),cs),[mkEDef False rec ps' c' (varScope cit) tel a (EStruct (getExpPosLast a) ds3 (varScope cit2) st (abstractLetDef ds3))])
   where mkHiddenStructFields :: UId -> CLetDef
         mkHiddenStructFields c = CSimple (CDef [] (CValueT (toId c) [] (CMeta (getUIdPosition c) False Nothing preMetaVar) (CMeta (getUIdPosition c) False Nothing preMetaVar)))
         getSuperClassFields :: ClassEnv -> UId -> PCM [UId]
         getSuperClassFields classEnv c =  let scs = immediateSuperClasses classEnv c 
                                           in return $ map fst scs
         lookupClassName :: ClassEnv -> CITrans -> Id -> PCM UId
         lookupClassName classEnv cit n = do
                ecv <- liftESTM (lookupId cit n)
                case ecv of  
                        Left x -> raise$ eMsg (getIdPosition n) (ENotClass (ppReadable n))
                        Right c -> if isClass classEnv c then return c else raise$ eMsg  (getIdPosition n) (ENotClass (ppReadable n))


        

toIDefn _ _ _ _ = internalError "toIDefn"



toIPackageBody ::   TransClass -> CPackageBody -> PCM Exp
toIPackageBody tc (CPackageDef ps pos ds) = 
               do let f = mapCLetDef (addModifiers ps) 

                      ds' = map f ds
                  (_,ds2) <- toILetDefs rec tc ds'
                  let xs = varScope (fst tc)
                      vcs = domVisibleLetDef ds2
                      st = foldl (\st x -> (toId x,x):st) [] vcs
                  return (Epackage pos ds2 xs st (abstractLetDef ds2))

toIPackageBody tc (CPackageInstance e) = do 
        toIExp tc False noPar e
        


toISummand ::  TransClass -> CSummand -> PCM ConBind
toISummand tc (i,as) = do 
        (_,as') <- toITel tc as
        return (i,map mkVisibleBind as')

toIIndSummand ::  TransClass -> CIndSummand -> PCM IndConBind
toIIndSummand tc ((i,ctel),hces) =
  do (tc',tel) <- toITel tc ctel
     es <- mapM (toIExpB tc') hces
     return ((i,tel),es)
     

toIOpenArgs ::  [CProp] -> TransClass -> COpenArgs -> PCM (TransClass,OpenArgs)
toIOpenArgs ps tc (COpenArgs us) = 
          do (tc',us') <- mapMAccumL (toIOpenArg ps) tc us
             return (tc',OpenArgs us' (varScope (fst tc)))

toIOpenArg :: [CProp] ->  TransClass -> COArg -> PCM (TransClass,OpenArg)
toIOpenArg ps tc (COArg ps' c) = do 
        (tc',(c',_)) <-  addConstant tc c
        let ps2 = toIProps ([Cprivate,Cconcrete]++ps++ps')
        return (tc',OpenConst ps2 c')
toIOpenArg ps tc (COArgAs ps' c1 c2) = do 
        (tc',(c2',_)) <-  addConstant tc c2
        let ps2 = toIProps ([Cprivate,Cconcrete]++ps++ps')
        return (tc',OpenConstAs ps2 c1 c2')
toIOpenArg ps tc (COArgT ps' c a) = do 
        let ps2 = toIProps ([Cprivate,Cconcrete]++ps++ps')
        a' <- toIExp tc False noPar a
        (tc',(c',_)) <- addConstant tc c
        return (tc',OpenConstT ps2 c' a')
toIOpenArg ps tc (COArgAsT ps' c1 a c2) = do 
        let ps2 = toIProps ([Cprivate,Cconcrete]++ps++ps')
        a' <- toIExp tc False noPar a
        (tc',(c2',_)) <- addConstant tc c2
        return (tc',OpenConstAsT ps2 c1 c2' a')





toIBind :: TransClass -> CArg -> PCM (TransClass,Bind)
toIBind tc (CArg hxs a)  = do 
        a' <- toIExp tc False noPar a
        let (hs,xs') = unzip hxs
        (tc',xs2) <- mapMAccumL (addVariable a') tc xs'
        let b = (zip hs xs2,a')
        return (tc',b)



toITel ::  TransClass -> [CArg] -> PCM (TransClass,Tel)
toITel tc ctel =  mapMAccumL toIBind tc ctel
  


toISign ::   TransClass -> CSign -> PCM ( TransClass,[ESigDef])
toISign tc (CSign xs a) = do 
        a' <- toIExp tc False noPar a
        (tc',xs') <- mapMAccumL (addVariable a') tc xs
        return (tc',[ESigAbs (map ((,) False) xs',a')])
toISign tc (CSignDef d) = do 
        let r = idCDefn d
        cs <- case r of
                Nothing -> return $ []
                Just c -> do c' <- makeUId c
                             return $ [(c,c')]
        ((tc',_),ds) <- toIDefn False [Cprivate] (tc,cs) d
        return (tc',map ESigDefn ds)


{- the only pattern allowed is (CPCon constr [(CPVar CPatArg)]) -}
{- by the way, think of using std type constructors more. -}
toICaseBranch:: TransClass -> (CPat,CExpr) -> PCM (CaseBranch,Exp)
toICaseBranch tc (CPCon i as,e) = do
    (tc',pargs) <- mapMAccumL doPVar tc as
    e' <- toIExp tc' True noPar e
    return (CBConM i pargs Nothing, e')
  where

    doPVar tc (CPVar (CPatId x)) = do
        (tc',x') <- addVariable' tc x
        return (tc',PArg x')
    doPVar tc (CPVar (CPatT x a)) = do
        a' <- toIExp tc False noPar a
        (tc',x') <- addVariable' tc x
        return (tc',PArgT x' a')
    doPVar _ p = raise $ eMsg (getCPatPos p) EPatternNested
toICaseBranch _ (p,_) = raise $ eMsg (getCPatPos p) EPatternNotConstr


toIExpB :: TransClass -> (Bool,CExpr) -> PCM (Bool,Exp)
toIExpB tc (b,e) = do e <- toIExp tc False 10 e
                      return (b,e)

toIExp :: 

       TransClass 
       -> Bool          {- used in many ways: at least
                           1. in Command.hs, whether to start parsing
                              pExprStart or pExp
                           2. in toIExp, whether case/sig is 'allowed' here
                              or not.  There's some discrepancy with CParser.
                              --> Fixed at getMetaInfo.
                        -}
       -> Int           {- precedence -}
       -> CExpr
       -> PCM (Exp)
      
{-
toIExP ::  Bool -> Int -> CExpr -> PCM Exp

toIExp pai pi cexp = do
                            --traceM$"toIExp in:\n"++ppDebug cexp
                            e <- toIExP pai pi cexp
                            --traceM$"toIExp out:\n"++ppDebug e
                            return e
-}

toIExp tc _ _ (CVar i) = mkIdExp tc i

toIExp _ _ _ (CStar p i _) = return (ESort p (Sort i))

toIExp tc pai _ (Clam b e) = do
    ((tc',classEnv'),b') <- toIBind tc b
    let tc2 = addPV (getVarsBind b') $ tc'
    e'<- toIExp (tc2,classEnv') pai noPar e
    return (eAbs [b'] e')


toIExp tc _ _ (CUniv b e) = do 
	(tc',tel) <- toIBind tc b
	e'<- toIExp tc' False noPar e
	return (eProd [tel] e')


toIExp tc _ _ (CArrow h a b) = do 
        a' <- toIExp tc False 1 a
        b' <- toIExp tc False 1 b
        return (EArrow h a' b')

toIExp tc _ _ (Clet ds e) = do 
        (tc',ds') <- toILetDefs rec tc ds
        e' <- toIExp tc' False noPar e
        return (EDef ds' e')

toIExp tc False _ (CProduct p ss) = raise $ eMsg p (EWrongPlace "sig")
toIExp tc _ _ (CProduct p ss) = do 
        (tc',tel') <- mapMAccumL toISign tc ss
        let tel2 = concat tel'
        checkDup (domESigDefs tel2)     
        return (ESig p tel2)

toIExp tc _ _ (CRecord ps p ds) = do 
        let f = mapCLetDef (addModifiers ps)
            ds' = map f ds
        (_,ds2) <- toILetDefs rec tc ds'
        let vcs = domVisibleLetDef ds2
            st = foldl (\st x -> (toId x,x):st) [] vcs
        return (EStruct p ds2 (varScope (fst tc)) st (abstractLetDef ds2))
toIExp tc _ _ (Copen m as e) = do 
        m' <- toIExp tc False noPar m
        (tc',as') <- toIOpenArgs [] tc as
        e' <- toIExp tc' False noPar e
        return (EOpen m' as' e')
toIExp tc _ _ (CSelect e i) = do 
        e' <- toIExp tc False 9 e
        return (EProj e' i)
toIExp _ False _ (CSum cs) = raise $ eMsg noPosition (EWrongPlace "data")
                                                            
toIExp tc _ _ (CSum cs) =
  do cs' <- mapM (toISummand tc) cs
     let xs = findDup (map fst cs)
     if null xs
       then return (EData cs')
       else let n = head xs
            in raise $ eMsg (getIdPosition n) (EDuplicateCon (ppReadable n))

toIExp _ False _ (CIndSum _ _) = raise $ eMsg noPosition (EWrongPlace "idata")
toIExp tc _ _ (CIndSum ctel cs) = do
     --traceM (show ctel)
     (tc',tel) <- toITel tc ctel
     --traceM $ show tel
     cs' <- mapM (toIIndSummand tc') cs
     let xs = findDup (map (fst.fst) cs)
     if null xs
       then return (EIndData tel cs')
       else let n = head xs
            in raise $ eMsg (getIdPosition n) (EDuplicateCon (ppReadable n))

toIExp _ _ _ (CConS i) = return (ECon i [])

toIExp tc _ _ e@(CCon i a) = do 
        a' <- toIExp tc False 9 a
        return (EConF i a' [])

toIExp _ False _ (Ccase e _) = raise $ eMsg noPosition (EWrongPlace "case")

toIExp tc _ _ (Ccase e cs) = do
    e' <- toIExp tc True noPar e
    --let tc' = case e' of {(EVar x) ->  addCaseVar x tc ; _ -> tc}
    cs' <- mapM (toICaseBranch tc) cs
    return (ECase e' cs')


toIExp tc pai pi (CApply (CVar x) [(False,e1),(False,e2)]) | isBinOp x = 
       toIExp tc pai pi (CBinOp e1 x e2)


toIExp tc _ pi (CApply f es) = do 
        f' <- toIExp tc False 9 f
        es' <- mapM (toIExpB tc) es
        return $ eApp' f' es'

{-

toIExp tc _ _ (CBinOp e1 op e2) = do 
        let pi = prec (getFixity op)
        e1' <- toIExp tc False pi e1
        eop <- mkIdExp tc op
        e2' <- toIExp tc False  pi e2
        return (EBinOp e1' eop e2')

-}

toIExp tc _ _ (CBinOp e1 op e2) = do 
        let (pl,pr) = case getFixity op of
                            FInfixl p -> (p, p+1)
                            FInfixr p -> (p+1, p)
                            FInfix  p -> (p, p)
        e1' <- toIExp tc False pl e1
        eop <- mkIdExp tc op
        e2' <- toIExp tc False  pr e2
        return (EBinOp e1' eop e2')



toIExp _ _ _ (CLit p l) = return (ELiteral p l)

toIExp tc _ _ (Cif b e1 e2 ) = do
        b' <- toIExp tc False noPar b
        e1' <- toIExp tc True noPar e1
        e2' <- toIExp tc True noPar e2
        return (EIf b' e1' e2')


toIExp tc pai pi (CMeta p _ aut _ ) = do mkMeta p pai tc pi aut
toIExp tc pai pi (Ccomment _ _ e) = toIExp tc pai pi e
toIExp _ _ _  CPackageType = return EPackageType

toIExp _ _ _ e = error "toIExp"


