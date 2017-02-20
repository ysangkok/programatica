module ProofMonad(module ProofMonad,MetaSubst,Constraint(..)) where

import ISyntax
import qualified AltIntMap as I
import ProofState
import Monads
import CITrans(CITrans,varScope,freeVarScope,scope,addVar)
import Error
import PPrint(ppDebug,ppReadable)
import FString(StrTable)
import Position
import AgdaScans (mapMAccumL)
import Maybe(mapMaybe,isJust)
import Gensym
import qualified MetaVars
--import CITranslate(TM)
import List(delete)
import Monad(when)
import AgdaTrace(trace)
--import MetaVarState



type PCM a  = StateM State a 

type TM a = PCM a


liftTM_PCM :: TM a -> PCM a
liftTM_PCM = id

{-
liftTM_PCM (STM f) = STM (\s -> case f () of
                                    Done (a,_) -> return (a,s)
                                    Err msg -> raise msg)
-}



getUninstantiated :: PCM [MetaVar]
getUninstantiated = readSTM (readUnboundMetaVars.readMetaVarState)


isUninstantiated  :: MetaVar -> PCM(Bool)
isUninstantiated k = do mvs <- getUninstantiated
                        return (k `elem` mvs)


isVisible :: MetaVar -> PCM Bool
isVisible k = do 
        mvv <- readSTME (\s ->
                                 liftMaybeE (I.ilookup k $ readMetaEnv $ readMetaVarState s) (eMsg noPosition (EUninstantiated (show k))))
        return $ (MetaVars.isVisible) $ visibility mvv

isProblematic :: MetaVar -> PCM Bool 
isProblematic k  = do mvv <- readSTME (\s ->
                                 liftMaybeE (I.ilookup k $ readMetaEnv $ readMetaVarState s) (eMsg noPosition (EUninstantiated (show k))))
                      return$ parseInfo mvv


getMetaSubst :: MetaVar -> PCM  Exp
getMetaSubst k = readSTME (\s ->
             liftMaybeE (I.ilookup k $ readMetaSubst $ readMetaVarState s) (eMsg noPosition (EUninstantiated (show k))))

{-
getMetaSubsts ::  PCM [(MetaVar,Exp)]
getMetaSubsts  = readSTM (\s ->
             readMetaSubst $ readMetaVarState s)
-}

getMetaScope :: MetaVar -> PCM [UId]
getMetaScope m = do mvv <- readSTME (\s -> liftMaybeE (I.ilookup m $ readMetaEnv $ readMetaVarState s) (eMsg noPosition $ EInternal ("No such meta variable: " ++ show m ++ "in getMetaScope")))
                    return ( varScope.fst $ transInfo mvv)

getMetaFreeVars :: MetaVar -> PCM [UId]
getMetaFreeVars m = do 
        mvv <- readSTME (\s -> liftMaybeE (I.ilookup m $ readMetaEnv $ readMetaVarState s) (eMsg noPosition $ EInternal ("No such meta variable: " ++ show m ++ "in getMetaScope")))
        return ( freeVarScope.fst $ transInfo mvv)


{-
        if getMetaVarVHidden mvv 
           then return $ freeVarsHidden (getMetaVarVContext mvv) (getMetaVarVEnv mvv)
           else return ( freeVarScope $ getMetaVarVCIT mvv)
     -- In fact here we don't want to test if hidden or not, but rather
     -- if it is a hidden meta variable that has been created during 
     -- type-checking, but I didn't want to add also that information. (CC)
     where freeVarsHidden g r = mapMaybe (isFree r) (domC g) 
           isFree r x = do e <- lookupE r x
                           case e of 
                                EVar y -> if x == y then Just x else Nothing
                                _ -> Nothing
-}              

def :: UId -> PCM Def
def c = do  ds <- readSTM readDefState
            case I.ilookup (getUIdNo c) ds  of
                 Just d -> return d
                 Nothing -> internalError ("def "++show c)
            
getDefState :: PCM DefState
getDefState = readSTM readDefState

getConstraints :: PCM([Constraint])
getConstraints = readSTM (readConstraints.readMetaVarState)

delAllConstraints :: PCM([Constraint])
delAllConstraints = updateSTMR (\s ->  let ps = readMetaVarState s
                                       in (readConstraints ps,writeMetaVarState (writeConstraints [] ps) s))

addConstraint :: (Value,Value) -> PCM ()
addConstraint (v1,v2) = updateSTM (accessMetaVarState (accessConstraints ((:) (Constraint v1 v2))))

addConstraint' :: Constraint -> PCM ()
addConstraint' c = updateSTM (accessMetaVarState (accessConstraints ((:) c)))

{-
getTypingConstraints :: PCM([TypingConstraint])
getTypingConstraints = readSTM (readTypingConstraints.readMetaVarState)

delAllTypingConstraints :: PCM([TypingConstraint])
delAllTypingConstraints = updateSTMR (\s ->  let ps = readMetaVarState s
                                       in (readTypingConstraints ps,writeMetaVarState (writeTypingConstraints [] ps) s))

addTypingConstraint :: MetaVar -> TCEnv -> UId -> LetDef -> PCM ()
addTypingConstraint m tchenv lg d = updateSTM (accessMetaVarState (accessTypingConstraints ((:) (TypingConstraint m tchenv lg d))))

-}

updateMetaInfo :: MetaVar -> Position -> Bool -> TransClass -> Int -> Visibility -> Context  -> Environment -> Judgement MetaVar -> PCM ()
updateMetaInfo m p pai cit pi hidden gamma env j = 
         let mvv = MetaVarV p pai cit pi hidden gamma env j
         in  setMetaInfo m mvv



--getMetaInfo :: MetaVar -> PCM (Position,Bool,TransClass,Int,Context,Environment,Judgement MetaVar)
getMetaInfo :: MetaVar -> PCM MetaVarV
getMetaInfo m = let lookupMeta env = liftMaybeE (I.ilookup m env) (eMsg noPosition  (EInternal "No such meta variable"))
                in do
                      --traceM$"getMetaInfo:0"
                      it <- readSTME (lookupMeta .readMetaEnv.readMetaVarState)
                      --traceM$"getMetaInfo:1"
                      return it
                      
setMetaInfo :: MetaVar -> MetaVarV -> PCM ()
setMetaInfo m mvv = updateSTM $ accessMetaVarState $ accessMetaEnv (I.add (m,mvv) )
                      

updateMetaSubst :: Bool -> MetaVar -> Exp -> PCM()
updateMetaSubst aut k e = do 
         updateSTM $ accessMetaVarState $ accessMetaSubst $ I.add (k,e)
         removeUnboundMetaVar k
         if aut then addBoundMetaVar k else done


flushBoundedMetaVars :: PCM [MetaSubst]
flushBoundedMetaVars = do
        ms <- removeBoundMetaVars 
        mes <- mapM (\m -> (getMetaSubst m >>= \e -> return (True,m,e))) ms
        return (reverse mes)


addUnboundMetaVar :: MetaVar -> PCM ()
addUnboundMetaVar m =
  do --traceM("addMetaVar:m="++show m)
     updateSTM $ accessMetaVarState $ accessUnboundMetaVars ((:) m)

removeUnboundMetaVar :: MetaVar -> PCM()
removeUnboundMetaVar m = updateSTM $ accessMetaVarState $ accessUnboundMetaVars (delete m)



addBoundMetaVar :: MetaVar -> PCM ()
addBoundMetaVar m =
  do --traceM("addMetaVar:m="++show m)
     updateSTM $ accessMetaVarState $ accessBoundMetaVars ((:) m)

removeBoundMetaVars :: PCM [MetaVar]
removeBoundMetaVars = do
    ms <- readSTM$ readBoundMetaVars.readMetaVarState
    updateSTM $ accessMetaVarState $ writeBoundMetaVars []
    return ms 



addDef :: UId -> Def -> PCM ()
addDef c d =
 do
    --traceM ("Def:\n"++ppDebug d)
    updateSTM $ accessDefState $ I.add (getUIdNo c,d)


updateDef :: UId -> Def -> PCM ()
updateDef = addDef 



getSymTab :: PCM SymTab
getSymTab = readSTM readSymTab

putSymTab :: SymTab -> PCM ()
putSymTab tab  = updateSTM (writeSymTab tab)


getClassEnv :: PCM ClassEnv
getClassEnv = readSTM readClassEnv

putClassEnv :: ClassEnv -> PCM ()
putClassEnv tab  = updateSTM (writeClassEnv tab)

getStrTable :: PCM StrTable
getStrTable = readSTM readStrTable

putStrTable :: StrTable -> PCM ()
putStrTable tab  = updateSTM (writeStrTable tab)

topLevelScope :: PCM [UId]
topLevelScope = do st <- getSymTab
                   return (rangeST st)


getTermCounter :: PCM Int
getTermCounter = readSTM readTermCounter

setTermCounter :: Int -> PCM ()
setTermCounter i = updateSTM (writeTermCounter i)

getValue :: PCM (Maybe Value)
getValue = readSTM readValue

setValue :: Maybe Value -> PCM ()
setValue i = updateSTM (writeValue i)
--toggleTrace:: PCM ()

toggleTraceFl = updateSTM $ accessXtraState $ accessTraceFl not

getTraceFl = readSTM $ readTraceFl . readXtraState

traceM' s = do traceOn <- getTraceFl
               when traceOn $ trace (s++"\n") $ return ()



freshVars :: Bool              -- replace dummies ?
           -> [Id]             -- names taken
           -> [UId]            -- uids to be freshened in case of crash.
           -> PCM ([Id],[UId]) -- (names now taken, freshened uids)
freshVars rpl ns0 xs = getStrTable >>= mapxs >>= out where
  mapxs t0 = mapMAccumL f (t0,ns0) xs
  f (t,ns) x | n == n2   = return ((t2,ns),x)
             | otherwise = ((,)(t2, n2:ns) . toUId n2) `liftM` idIndex
    where n        = toId x
          (t2, n2) = if rpl && isDummyId n
                     then freshId' t ns "h" else freshId t ns n
  out ((tbl2,tkn2),xs2) = putStrTable tbl2 >> return (tkn2, xs2)



freshVarsCIT :: Bool              -- replace dummies ?
           -> CITrans             -- names taken
           -> [UId]            -- uids to be freshened in case of crash.
           -> PCM (CITrans,[UId]) -- (names now taken, freshened uids)
freshVarsCIT rpl cit xs = do
    (ns,xs') <- freshVars rpl ns0 xs
    return (foldr (uncurry addVar) cit (zip ns xs'),xs')  
  where ns0 = map toId (scope cit)

idIndex :: PCM Int
idIndex = updateSTMR (\s ->  let (i,is) = readTransState s
                             in (i,writeTransState (i+1,is) s))


metaVarIndex :: PCM Int
metaVarIndex = updateSTMR (\s ->  let (i,is) = readTransState s
                                      (n,gs) = gensymId is
                                  in (n,writeTransState (i,gs) s))

makeUnique :: UId -> PCM UId
makeUnique i = do n <- idIndex
                  return$ putUIdNo i n


isOfUniqueType :: UId -> PCM Bool 
isOfUniqueType c = do
        d <- def c
        case d of 
                (Def _ _ _ _ _ _ _ (DExp  (EData _))) -> return True
                (Def _ _ _ _ _ _ _ (DExp  (EIndData _ _))) -> return True
                _ -> return False
