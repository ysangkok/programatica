{-|

 Replaces meta-variables by their substitution, if any.

-}

module ExpToExp(expToExp,telToTel,letsToLets,letToLet)  -- > Solve
where
import AgdaScans
import ISyntax
import MetaVarState
import Monads -- (internalError,traceM,(=<<),liftM,liftM2,join)
import ProofMonad
import PPrint(ppDebug,ppReadable,PDetail(..),pIText)


expToExp :: Exp -> PCM Exp
expToExp e = case e of
      (EMeta m _ _ _ _ _) -> ifM (isUninstantiated m)
                                 (return e)
                                 (getMetaSubst m >>= expToExp ) `handle_` return e
      (EProd     b e          )  -> goAbs  EProd    b e
      (EAbs      b e          )  -> goAbs  EAbs     b e
      (EApp   s   ss)-> do 
                          let (hs,ss') = unzip ss
                          es <- mapM expToExp ss'
                          e <- expToExp s
                          return $ eApp' e (zip hs es)
      (EBinOp e1 e2 e3)  -> do [e1',e2',e3'] <-   mapM expToExp  [e1,e2,e3]
                               return (EBinOp e1' e2' e3')
      (EDef ds e) ->  do ds <- letsToLets ds 
                         e <- expToExp e
                         return (EDef ds e)
      (EOpen e0 oas e1) -> do
                e0 <- expToExp e0
                oas <- oArgsToOArgs oas
                e1 <- expToExp e1
                return (EOpen e0 oas e1)
      (ESig pos sigdefs) -> do sds <- mapM goSig sigdefs
                               return$ ESig pos sds
      
      (EStruct  pos ds xs cs as) -> do ds <- letsToLets ds 
                                       return (EStruct pos ds xs cs as)    
      (Epackage pos ds xs cs as)  -> do ds <- letsToLets ds 
                                        return (Epackage pos ds xs cs as)
      (EProj s n   )-> (`EProj` n) `liftM` expToExp s 
      (ECon  n   vs)-> do let (hs,vs') = unzip vs
                          es <- mapM expToExp vs'
                          return $ ECon n (zip hs es)
      (EConF n t vs)-> do let (hs,vs') = unzip vs
                          es <- mapM expToExp vs'
                          t' <- expToExp t
                          return $ EConF n t' (zip hs es)

      (ECase e cbes) -> do
                e' <- expToExp e
                cbes' <- mapM (\(cb,e) -> (expToExp e >>= (\e' -> return (cb,e')))) cbes
                return (ECase e' cbes')
      (EArrow hidden e1 e2) -> do 
                e1' <- expToExp e1 
                e2' <- expToExp e2
                return$ EArrow hidden e1' e2'
      (EData cbs) ->  do cbs' <- mapM (\(i,tel) -> telToTel tel >>= (\tel' -> return (i,tel'))) cbs
                         return (EData cbs')
      (EVar _ _) -> return e
      (EConst _ _) -> return e
      (ESort _ _) -> return e
      

      ( _ ) -> do --traceM$ "In expToExp "++ ppDebug e
                  return e
    where
    goSig (ESigAbs d) = do 
        bd2 <- bindToBind d
        return (ESigAbs bd2)
    goSig (ESigDefn d) = do
        d' <- defToDef d
        return$ (ESigDefn d')
    goAbs binder bd e = do
      bd2 <- bindToBind bd
      binder bd2 `liftM` expToExp e

bindToBind (xs,a) = do
     a2        <- expToExp a
     return (xs, a2)
        
telToTel tel = mapM bindToBind tel




defToDef (Def blocked rec ps c ys tel a rhs) = do 
        tel2 <- telToTel tel
        a2      <- expToExp a
        let wrap = Def blocked rec ps c ys tel2 a2
        case rhs of (DExp e)-> (wrap . DExp) `liftM` expToExp e
                    ( _    )-> return (wrap PN)
defToDef   (UnTypedDef blocked rec ps c ys  (DExp e)) =
              (UnTypedDef blocked rec ps c ys . DExp) `liftM` expToExp e
defToDef d@(UnTypedDef _ _ _ _ _ _ ) = return d

defToDef   (DOpen    e   oas) = do
        e' <- expToExp e
        oas' <- oArgsToOArgs oas
        return$ DOpen e' oas'


oArgsToOArgs (OpenArgs oas ys) = do 
        oas <- mapM oArgToOArg oas
        return$ (OpenArgs oas ys)

oArgToOArg (OpenConstT   ps   c a)= OpenConstT   ps   c `liftM` (expToExp a)
oArgToOArg (OpenConstAsT ps x c a)= OpenConstAsT ps x c `liftM` (expToExp a)
oArgToOArg oa = return oa


letsToLets ds  = mapM letToLet ds
letToLet (DSimple d  ) = DSimple `liftM`      defToDef d
letToLet (DMutual ds') = DMutual `liftM` mapM defToDef ds'
