module ClassEnv where
--import ProofMonad
import Monads(liftMaybeSTM,traceM)
import Position(noPosition)
import Id(Id,UId)
import AltIntMap(ilookup)
import PPrint(ppReadable,ppDebug)
import Error 
-- import CSyntax
import Util(assoc)
import Debug.Trace(trace)
import ISynType
import Maybe (isJust)
import ProofMonad


isClass :: ClassEnv -> UId -> Bool
isClass (cT,_) c = isJust (lookup c cT)

addClass :: ClassEnv -> UId -> SuperClasses  -> ClassEnv
addClass (cT,iT) i scs =  ((i,scs):cT,iT)
         
immediateSuperClasses :: ClassEnv -> UId -> SuperClasses
immediateSuperClasses (cT,_) c = assoc cT c


allSuperClasses :: ClassEnv -> UId -> [(UId,[UId])] 
allSuperClasses (cT,iT) c = 
        let scs = assoc cT c
            scs' = foldr addSuperClasses [] scs
         in  scs'
   where addSuperClasses :: (UId,UId) ->  [(UId,[UId])] ->  [(UId,[UId])]
         addSuperClasses (name,sc) scs = 
                let scs' = allSuperClasses (cT,iT) sc 
                in (sc,[name]):map (mkSC name) scs'
         mkSC :: UId -> (UId,[UId]) -> (UId,[UId]) 
         mkSC name (sc,path) = (sc,name:path)


addInstance :: ClassEnv ->  UId -> Exp -> PCM ClassEnv
addInstance cEnv@(cT,iT) instName instanceT = do
        return $ maybe cEnv addInst' (splitInstance instanceT)  -- add test on well-formedness
    where addInst' (className,instTypeId) = 
                let scs = allSuperClasses (cT,iT) className
                    ii = maybe [] id (lookup instTypeId iT)
                    ii' = map mkInstanceInfo scs
                in --trace ("addInst "++ppDebug (className,instTypeId)) $
                      if isClass cEnv className then (cT,(instTypeId,(className,(instName,[])):ii'++ii):iT)
                      else cEnv
          mkInstanceInfo  :: (UId,[UId]) -> InstanceInfo
          mkInstanceInfo (sC,path) = (sC,(instName,path))
{-
addClass 

addInstance :: ClassEnv -> Id -> CExpr -> PCM (TCEnv,([(Bool,MetaVar,CExpr)])
addInstance classEnv@(superClassesTable,instanceTable) name e = do 
        let mi <- splitInstance e
        maybe (return classEnv) 
              ((class,type) ->  lookupSuperClasses superClasses class >>= buildInstance instanceTable name class type >>= addInstanceToEnv classEnv)

-}

-- CApply (CVar Eq) [(False,CApply (CVar List) [(False,CVar A)])]

splitInstance :: Exp -> Maybe (UId,UId)
splitInstance (EApp (EConst c _) [(_,EVar x _)]) = Just (c,x)
splitInstance (EApp (EConst c _) [(_,EConst x _)]) = Just (c,x)
splitInstance (EApp (EConst c _) [(_,EApp (EConst x _) es)]) = Just (c,x)

splitInstance e =  Nothing

getClassInst :: ClassEnv -> Exp -> Maybe (UId,Exp) 
getClassInst classEnv e = 
   maybe Nothing (\it@(c,e) -> if isClass classEnv c then Just it else Nothing ) (split e)
   where split :: Exp -> Maybe (UId,Exp)
         split (EApp (EConst c _) [(_,EApp e _)]) = Just (c,e)
         split (EApp (EConst c _) [(_,e)]) = Just (c,e)
         split e =  Nothing


lookupInstance :: ClassEnv -> Exp -> UId -> Maybe (UId,[UId])
lookupInstance (cT,iT) instType className = do
        --traceM ("In lookup0 "++ ppDebug (cT,iT))
        --traceM ("In lookup1 "++ ppDebug (instType,className))
        instName <- getInstName instType
        ii <- lookup instName iT
        --traceM ("In lookup2 "++ ppDebug (ii))
        lookup className ii
    where getInstName :: Exp -> Maybe UId 
          getInstName (EVar x _) = Just x
          getInstName (EConst c _) = Just c
          getInstName (EApp h _) = getInstName h
          getInstName e = Nothing -- trace ("inst " ++ ppDebug e) Nothing
