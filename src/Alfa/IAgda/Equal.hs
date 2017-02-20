{- | Tests if two values are equal, v1 =? v2 . If we cannot now if they are equal or not because
of a meta-variable, it adds the equality v1 = v2 as a constraint
-}

module Equal ((=?),simplifyCs,simplifyC) where
import Eval
import Monad
import ISyntax
import CSyntax
import CPrinter
import Monads
import PPrint (ppReadable,ppDebug)
import Literal
import Error
import AgdaTrace
import Imitate(expand)
import MetaVarState 
import Position(noPosition)
-- import SimpleSolve (trySolveSafeCs,trySolveSafe)

infix 5 |- 
infix 5 -|-



{-| 'v1 ?= v2' checks whether the values 'v1' are equal 'v2' -}

(=?)      :: Value -> Value -> PCM ()


{- | 'Equal': equality judgement.  Just a piece of syntax to be able
to write 'k |- v := v' in the code -}

data Equal =  Value := Value 

{- | 'k |- v1 := v2': checks wether 'v1' is equal to 'v2' with a maximum 'k'
number of unfolding steps.  'k' is decreased whenever we pass from
'|-' to '-|-'.

The auxiliary judgement 'k -|- v1 := v2' does most of the actual work. -}

(|-)  :: Int -> Equal -> PCM ()
(-|-) :: Int -> Equal -> PCM ()



v1 =? v2 = do n <- getTermCounter
              --traceM ("Equal:\n"++ppDebug (v1,v2))
              n |- v1 := v2 `handle` termError 
          where termError e =
                  case e of
                      (_,EPass (ETermProblem t)) -> 
                             raise $ eMsg (getExpPos v2) (ETermination (ppReadable (Constraint v1 v2)) t)
                      (_,EPass ENotEqual) -> raise $ eMsg (getExpPos v1) (ENotEqualValues (ppReadable v1) (getExpPos v2) (ppReadable v2))
                      _ -> raise e


0 |- (v1 := v2) = do --traceM "kom hit"
                     passError $ (ETermProblem (ppReadable (Constraint v1 v2)))


k |- (v1@(EStop m _) := v2) =  ifM (isUninstantiated m)
                                 (do{v2' <- evalMeta v2;
                                     addConstraint (v1,v2') 
                                      })
                                    
                                 (do{v1' <- evalMeta v1;
                                     k |- v1' := v2})

k |- (v1 := v2@(EStop m _)) = ifM (isUninstantiated m)
                                 (do{v1'<- evalMeta v1;
                                     addConstraint (v1',v2) })
                                 (do{v2' <- evalMeta v2;
                                     k |- v1 := v2'})

-- MT
-- ugly hack until ECase gets CtxInfo and removed from isProbLam.
-- Before unfolding, checks if heads and args are equal individually. If so,
-- ok; otherwise, we cannot give up yet, of course.
-- Needed to avoid infinite unfolding in, say (Carlos' example)
--   rem (m, n :: Nat) :: Nat = ifThenElse Nat (ltNat m n) m (rem (sub m n) n)
-- Since ifThenElse's body isProbLam, "rem m n =? rem m n" does not terminate.
-- BTW, this is actually good for efficiency.

{- Aaaaah, this is no good: this prematurely constrain ?'s in vs1 or vs2
  (030315: in addition, this piece now causes SIGSEGV. why?)
k |- (v1 := v2) = ok_apps_or $ ((k-1)-|-)=<<(unfold v1 === unfold v2) where
  (===) = liftM2 (:=)
  ok_apps_or cont = case (v1,v2) of
    (EApp h1@(EClos _ (EConstV _ _)) vs1,
     EApp h2@(EClos _ (EConstV _ _)) vs2)->
       do { k -|- h1 := h2
          ; mapM_ (k |-) (zipWith (:=) vs1 vs2) } `handle_` cont
    ( _                                 )->                 cont
So back to the known-to-loop original ...
-}

k |- (v1 := v2) = do 
        --traceM ("Equal:\n"++ppDebug (v1,v2))
        v1' <- unfold v1
        v2' <- unfold v2
        (k-1) -|- (v1' := v2')


k -|- (EVar x _ := EVar y _)
                  | x == y = done
                  | otherwise = if isInternalUId x || isInternalUId y 
                                   then passError ENotEqual
                                   else raise $ eMsg (getUIdPosition x) (ENotEqualValues (ppReadable x) (getUIdPosition y) (ppReadable y))

k -|- (EClos env1 v1@(EConstV c1 xs1) := EClos env2 v2@( EConstV c2 xs2))
 | c1 == c2 = do vs1 <- mapM (`lookupEnv` env1) xs1
                 vs2 <- mapM (`lookupEnv` env2) xs2
                 eqVs k vs1 vs2   
 | otherwise = do raise $ eMsg (getUIdPosition c1) (ENotEqualValues (ppReadable c1) (getUIdPosition c2) (ppReadable c2))

k -|- (ESort pos1 s1      := ESort pos2 s2)
  | s1 <=  s2  = done
  | otherwise = do raise $ eMsg pos1 (ENotEqualValues (ppReadable s1) pos2 (ppReadable s2))

k -|- (EPackageType := EPackageType) = done

k -|- (EClos env0 e0@(EAbs (b0,a0) _) := EClos env1 e1@(EAbs (b1,a1) _)) = 
           do --va0 <- eval env0 a0   -- Är detta säkert??
              --va1 <- eval env1 a1
              --va0 =? va1
              let (hs0,xs) = unzip b0
              let (hs1,ys) = unzip b1
              vs <- genVals xs 
              vc0 <- eval' env0 e0 (zip hs0 vs)
              vc1 <- eval' env1 e1 (zip hs1 vs)
              k |- vc0 := vc1


k -|- (v0 := EClos env1 e1@(EAbs (b1,a1) _)) = 
           do                          -- Är detta säkert??
              let (hs,xs) = unzip b1
              vs <- genVals xs
              vc0 <- vApp v0 (zip hs vs)
              vc1 <- eval' env1 e1 (zip hs vs)
              k |- vc0 := vc1


k -|- (EClos env0 e0@(EAbs (hxs,a0) _) := v1) = 
           do                         -- Is this safe?
              let (hs,xs) = unzip hxs
              vs <- genVals xs
              vc0 <- eval' env0 e0 (zip hs vs)
              vc1 <- vApp v1 (zip hs vs)
              k |- vc0 := vc1

k -|- (EClos env0 (EProd (xs ,a0) b0) := EClos env1 e1@(EProd (ys,a1) b1)) = 
           do va0 <- eval env0 a0
              va1 <- eval env1 a1
              k |- va1 := va0
              let n = (min (length xs) (length ys))
              matchingHidden (take n xs) (take n ys)
              vs <- genValN n
              let (env0',xs',_) = updatesEnv' env0 xs vs
                  (env1',ys',_) = updatesEnv' env1 ys vs

              v0 <- eval env0' (eProd [(xs',a0)] b0) 
              v1 <- eval env1' (eProd [(ys',a1)] b1) 
              k |- v0 := v1
         


k -|- ((EArrow h va0 vb0) := (EArrow h' va1 vb1)) = 
           do unless (h==h') $ raise$ eMsg (getExpPos va1) EHiddenArgs
              k |- va1 := va0
              k |- vb0 := vb1
            
k -|- (EClos env0 (EProd ((h,x):xs ,a0) b0) := (EArrow h' va1 vb1)) = 
           do unless (h==h') $ raise $ eMsg (getExpPos va1) EHiddenArgs
              va0 <- eval env0 a0
              k |- va1 := va0
              v <- genVal x 
              let env0' = updateEnv env0  x v
              v0 <- eval env0' (eProd [(xs,a0)] b0) 
              k |- v0 := vb1

k -|- ((EArrow h va1 vb1) := EClos env0 (EProd ((h',x):xs ,a0) b0)) = 
           do unless (h==h') $ raise$ eMsg (getExpPos a0) EHiddenArgs
              va0 <- eval env0 a0
              k |- va0 := va1
              v <- genVal x
              let env0' = updateEnv env0  x v
              v0 <- eval env0' (eProd [(xs,a0)] b0) 
              k |- vb1 := v0


k -|- (EApp v1 vs1   := EApp v2 vs2) = do 
	k |- v1 := v2
        let vs1' = map snd vs1  
            vs2' = map snd vs2
	eqVs k vs1' vs2'
   
  {- now, how to justify we don't check the length of vs1 and vs2? (MT) 
     We only test equality on expressions that has the same type (CC) 
   -}
    
                    


k -|- (ECon i0 vs0 := ECon i1 vs1)
   | i0 == i1 = eqVs k (map snd vs0) (map snd vs1)
   | otherwise = raise $ eMsg (getIdPosition i0) (ENotEqualValues (ppReadable i0) (getIdPosition i1) (ppReadable i1))


k -|- (EConF i0 e0 es0 := EConF i1 e1 es1)
   | i0 == i1 = do k |- e0 := e1  
                   eqVs k (map snd es0) (map snd es1)
   | otherwise = raise $ eMsg (getIdPosition i0) (ENotEqualValues (ppReadable i0) (getIdPosition i1) (ppReadable i1))




k -|- (EProj v0 n0 := EProj v1 n1)
        | n0 == n1 = k |- v0 := v1
        | otherwise = do 
                        raise $ eMsg (getIdPosition n0) (ENotEqualValues (ppReadable n0) (getIdPosition n1) (ppReadable n1))


k -|- (EClos env0 (EStruct _ _ xs0 st0 _) := EClos env1 (EStruct _ _ xs1 st1 _))
       | ns0 == ns1 = do vs0 <- mapM (flip (vConst env0) xs0)  cs0
                         vs1 <- mapM (flip (vConst env1) xs1) cs1
                         eqVs k vs0 vs1      
       |otherwise = passError ENotEqual
    where st0' =  st0
          st1' = st1
          ns0 = map fst st0'
          ns1 = map fst st1'
          cs0 = map snd st0'
          cs1 = map snd st1'

k -|- (v0 := EClos env1 (EStruct _ _ xs1 st _))
       | cs /= []  = do vs0 <- zipWithM vProj (repeat v0) ns
                        vs1 <- mapM (flip (vConst env1) xs1) cs
                        eqVs k vs0 vs1      
       |otherwise = passError ENotEqual
   where st' = st
         ns = map fst st'
         cs = map snd st'
 
 
 

k -|- (EClos env0 (EStruct _ _ xs0 st _) := v1)
       | cs /= []  = do vs0 <- mapM (flip (vConst env0) xs0) cs
                        vs1 <- zipWithM vProj (repeat v1) ns
                        eqVs k vs0 vs1      
       |otherwise = passError ENotEqual
   where st' = st
         ns = map fst st'
         cs = map snd st'
 

     
         


k -|- (EClos env0 (Epackage _ _ xs0 cs0 _) := EClos env1 (Epackage _ _ xs1 cs1 _))
       | cs0 == cs1 = do vs0 <- mapM (\x -> lookupEnv x env0) xs0
                         vs1 <- mapM (\x -> lookupEnv x env1) xs1
                         eqVs k vs0 vs1      
       |otherwise = passError ENotEqual

{-
k -|- (EClos env0 (EIndData ctx0 _ _) := EClos env1 (EIndData ctx1 _ _)) =
      if getCtxOcc ctx0 == getCtxOcc ctx1
       then do vs0 <- mapM (flip lookupEnv env0) (getCtxFV ctx0)
               vs1 <- mapM (flip lookupEnv env1) (getCtxFV ctx1)
               eqVs k vs0 vs1
       else passError ENotEqual
-}

k -|- (v1@(EStop m _) := v2) =  addConstraint (v1,v2)


k -|- (v1 := v2@(EStop m _)) = addConstraint (v1,v2)


k -|- ((ELiteral _ l) := (ELiteral _ l')) = do 
                                               if l == l' 
                                                 then done
                                                 else passError ENotEqual  


k -|- ((EIf b1 v1 v2) := (EIf b2 v1' v2')) = do 
        k |- b1 := b2
        k |- v1 := v1'
        k |- v2 := v2'



k -|- (v1 := v2) = do v1' <- expand (Just []) [] v1
                      v2' <- expand (Just []) [] v2
                      raise $ eMsg (getExpPos v1) (ENotEqualValues (ppReadable v1') (getExpPos v2) (ppReadable v2'))

eqVs k [] [] = do --traceM "done eqConst"
                  done
eqVs k (v:vs) (v':vs') = do k |- v := v'
                            eqVs k vs vs'
eqVs _ _ _ = passError ENotEqual

matchingHidden :: [(Bool,UId)] -> [(Bool,UId)] -> PCM ()
matchingHidden ((h,_):hxs) ((h',x):hxs')| h == h' = matchingHidden hxs hxs' 
matchingHidden [] [] = done
matchingHidden _ _ = raise$ eMsg noPosition EHiddenArgs


{-
eqTel :: Int -> (Tel,Environment) -> (Tel,Environment) -> PCM ()
eqTel _ ([],env1) ([],env2) = return ()
eqTel k ((xs1,a1):tel1,env1) ((xs2,a2):tel2,env2) =
                       do va1 <- eval env1 a1
                          va2 <- eval env2 a2
                          mes <- k |- va1 := va2
                          vs <- genVals (map snd xs1)
                          let (env1',xs1',_) = updatesEnv' env1 xs1 vs
                              (env2',xs2',_) = updatesEnv' env2 xs2 vs
                          mes' <- eqTel k (addBindTel tel1 (xs1',a1),env1') (addBindTel tel2 (xs2',a2),env2')
                          return $ mes ++ mes'

{- ------- -}
--eqTel0 ::(Tel,Environment) -> (Tel,Environment) -> PCM ()
--eqTel0 telenv telenv' = do { n <- getTermCounter; eqTel n telenv telenv'}
-}




simplifyC :: Constraint -> PCM ()
simplifyC c@(Constraint v1 v2) = v1 =? v2 `handle` \e ->
   raise $ eMsg (noPosition) (ENotEqualConstraints (ppReadable c)
                              (if isPassMsg e then "" else prEMsg e))


errSimpl c e = raise $ eMsg (noPosition) (ENotEqualConstraints (ppReadable c)
                              (if isPassMsg e then "" else prEMsg e))




simplifyCs :: PCM ()
simplifyCs = do cs <- delAllConstraints
                mapM simplifyC cs
                done


