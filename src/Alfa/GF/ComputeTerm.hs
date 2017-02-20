module ComputeTerm where

import Operations
import Tokens
import Grammar
import Macros
import SymbolTable
import PrGrammar
import Lookup
import Rename
import Monad (liftM2)

-- generic computation functions. AR 4/12/1999 -- 31/3/2000

substitute :: Token a => Substitution a -> Term a -> Err (Term a) -- simultaneous
substitute subst trm = case trm of
   Var x -> case lookup x subst of
     Just t -> return t
     _ -> return trm --- ??
   _ -> composOp (substitute subst) trm

substitutes :: Token a => Substitution a -> Term a -> Err (Term a) -- sequential
substitutes subst trm = case subst of
   [] -> return trm
   xa:xas -> do
     trm' <- substitutes xas trm 
     substitute [xa] trm'

alfaConv :: [(Ident,Ident)] -> Term a -> Term a
alfaConv xs trm = case trm of
  Cons x -> case lookup x xs of
     Just y -> Cons y
     _ -> trm
  Var x -> case lookup x xs of
     Just y -> Var y
     _ -> trm
  _ -> composSafeOp (alfaConv xs) trm

defexpand :: Token a => (Ident -> Err (Term a)) -> Term a -> Err (Term a)
defexpand defs trm = case trm of
   Cons c -> case defs c of
     Ok (Cons c') | c' == c -> return trm 
     Ok trm' -> defexpand defs trm'
     _ -> prtBad "def expansion fails for constant" trm
   _ -> composOp (defexpand defs) trm

betaconv :: Token a => Term a -> Err (Term a)
betaconv trm = case trm of
  App (Abs x b) a -> do
    a' <- betaconv a
    b' <- betaconv b
    b'' <- substitute [(x,a')] b'
    betaconv b''
  App b a -> do
    a' <- betaconv a
    b' <- betaconv b
    (if a'==a && b'==b then return else betaconv) $ App b' a'
  UpdRecord r (l,v) -> do
    r' <- betaconv r
    v' <- betaconv v
    updRecord r' (l,v') 
  Project (Record r) i -> do
    t <- lookupErr i r
    betaconv t
  Let dd b -> do    --- should we take Let in another occasion ?
    let (xx,aa) = unzip [(x,a) | (x,_,a) <- dd]
    aa' <- mapM betaconv aa
    b'  <- substitutes (zip xx aa') b
    betaconv b'  
  _ -> composOp betaconv trm

betaOnlyConv :: Token a => Term a -> Err (Term a) -- without Let conversion
betaOnlyConv trm = case trm of
  App (Abs x b) a -> do
    a' <- betaOnlyConv a
    b' <- betaOnlyConv b
    b'' <- substitute [(x,a')] b'
    betaconv b''
  App b a -> do
    a' <- betaOnlyConv a
    b' <- betaOnlyConv b
    (if a'==a && b'==b then return else betaOnlyConv) $ App b' a'
  Project (Record r) i -> do
    t <- lookupErr i r
    betaOnlyConv t
  UpdRecord r (l,v) -> do
    r' <- betaOnlyConv r
    v' <- betaOnlyConv v
    updRecord r' (l,v') 
  _ -> composOp betaOnlyConv trm

projectConv :: Token a => Term a -> Err (Term a) -- only projections
projectConv trm = case trm of
  Project (Record r) i -> do
    t <- lookupErr i r
    projectConv t
  UpdRecord r (l,v) -> do
    r' <- projectConv r
    v' <- projectConv v
    updRecord r' (l,v') 
  _ -> composOp projectConv trm

caseselect :: Token a => Term a -> Err (Term a)
caseselect trm = case trm of
  Select (Cases [([PattVar x],val)]) [t] -> do
    subst <- return [(x,t)]  --- for multiple patts as well - or curry them !!
    trm' <- substitute subst val
    caseselect trm'
  Select (Cases cases) ts | all isInConstantForm ts -> do
    ts' <- mapM caseselect ts         -- don't select for unknown tag !
    (val,subst) <- findMatch cases ts'
    trm' <- substitute subst val
    caseselect trm'
  Select (Select (Cases cc) x) y -> do
    cc' <- mapPairListM (\ (_,t) -> caseselect (Select t y)) cc
    return (Select (Cases cc') x)
  _ -> composOp caseselect trm

isInConstantForm :: Term a -> Bool
isInConstantForm trm = case trm of
  Cons _  -> True
  App c a -> isInConstantForm c && isInConstantForm a
  Record r -> all (isInConstantForm . snd) r
  _ -> False ---- isInArgVarForm trm

isInArgVarForm :: Term a -> Bool
isInArgVarForm trm = case trm of
  ArgVar _ -> True  -- to be able to generate parsers
  App c a  -> isInArgVarForm a || isInArgVarForm c --- 22/9/2000
  Project a _ -> isInArgVarForm a
  _ -> isInConstantForm trm ---- False

findMatch :: Token a => [([Patt],Term a)] -> [Term a] -> Err (Term a,Substitution a)
findMatch cases terms =
 case cases of
   []   -> Bad ( "no applicable case for" +++ unwords (map prt terms))
   (patts,_):_ | length patts /= length terms -> 
       Bad ("wrong number of args for patterns :" +++ 
            unwords (map prt patts) +++ "cannot take" +++ unwords (map prt terms))
   (patts,val):cc -> 
     case mapM tryMatch (zip patts terms) of
       Ok substs -> return (val, concat substs)
       _         -> findMatch cc terms

tryMatch :: Token a => (Patt, Term a) -> Err [(Ident, Term a)]
tryMatch (p,t) =
 do t' <- termForm t
    (case (p,t') of
      (PattVar x, _) -> return [(x,t)]
      (PattCons p pp, ([], Cons f, tt)) | 
            p `eqStrIdent` f && length pp == length tt ->
         do matches <- mapM tryMatch (zip pp tt)
            return (concat matches)
      (PattRec r, ([],Record r',[])) |
            all (`elem` map fst r') (map fst r) ->
         do matches <- mapM tryMatch [(p,a) | (l,p) <- r, let Just a = lookup l r']
            return (concat matches)
      _ -> prtBad "no match in case expr for" t)

-- expand a case expression into a table ; cases taken of tag types only

expandCases :: Token a => [LType a] -> LTerm a -> ConcreteST a -> 
                                                          Err [([Patt],LTerm a)]
expandCases typs trm st = 
  do tts <- allPatts typs st
     mapM (mkCase trm) (combinations tts)
   where
    mkCase t tt = case t of
      Cases cc -> do (tr,subst) <- findMatch cc (map patt2term tt)
                     tr' <- substitute subst tr
                     return (tt,tr')
      _ -> return (tt, mkSelect t (map patt2term tt))

linCompute :: Token a => ConcreteST a -> Substitution a -> LTerm a -> Err (LTerm a)
linCompute cnc env trm = case trm of
  Var x -> case lookup x env of
    Just trm' -> return trm' -- we may assume everything's computed in env !
    _ -> return trm --- prtBad ("unknown variable in context" +++ prContext env +++
                     --- ", therefore cannot lincompute") trm
  Closure g t -> linCompute cnc (g ++ env) t
  Cons c -> case lookupLin c cnc of
    Ok (Cons c') | c' == c -> return trm
    Ok trm' -> return trm' ---- lic trm'
    Bad s -> prtBad (s +++ "therefore cannot lincompute") trm
  Select cc vv -> do
    cc' <- lic cc
    vv' <- mapM lic vv
    (case cc' of ----
       Cases [([PattVar x],val)] -> do
         let subst = zip [x] vv' 
         linCompute cnc (subst ++ env) val
       Cases cc'' | all isInConstantForm vv' -> do
         (val,subst) <- errIn ("selecting constform from" +++ prt cc') $ 
                              findMatch cc'' vv' 
         linCompute cnc (subst ++ env) val
       Cases _ | all isInArgVarForm vv' -> return $ (Select cc' vv')
       Cases _ -> return $ (Select cc' vv')
       _ -> prtBad "lincompute cannot select cases from" cc')
---  Cases _ -> return trm ---
  UpdRecord t lv -> do
    t' <- lic t
    updRecord t' lv
  Project r l -> do
    r' <- lic r
    (case r' of
       Record r'' -> do
          trm' <- case lookup l r'' of
                    Just t@(ArgVar (c,(_,k))) -> case l of --- bug fix for GFtoCF ?!
                       Label (s,i) | l == linLabel i -> return $ ArgVar (c,(i,k))
                       _ -> return trm
                    Just t -> Ok t
                    _ -> Bad ("cannot find label" +++ prt l +++ "in" +++ prt r') 
          lic trm'
       ArgVar (c,(_,k)) -> case l of
           Label (s,i) | l == linLabel i -> return $ ArgVar (c,(i,k)) --- i not vis.
           _ -> return trm
-----       Project (ArgVar _) _ -> return trm --- not visible ; why this case ?
       _ -> prtBad ("lincompute cannot project" +++ prt l +++ "from") r')
  Let defs t -> do
    let (vars,vals0) = unzip [(x,a) | (x,_,a) <- defs]
    vals <- mapM lic vals0 --- aren't the defs in the scopes of each other ?
    let subst = zip vars vals
    linCompute cnc (subst ++ env) t
  Concat t1 t2 -> 
    liftM2 Concat (lic t1) (lic t2)
  Glue t1 t2 ->
    liftM2 Glue (lic t1) (lic t2)
  ArgVar _ -> return trm -- this is visible! 
  _ -> composOp lic trm
---    prtBad "cannot lincompute term" trm
 where lic = linCompute cnc env 



--- old GF computation for syntax trees; to be unified with the former

computeTerm = compute

type Definition = (Patt,Trm)
type Substit = Substitution ()

compute :: AbstractST -> Trm -> Trm
compute abstr c = compt [] c where
  compt vv c = case c of
    Prod x a b  -> Prod x (compt (x:vv) a) (compt (x:vv) b) --- why in a ??
    Abs x b     -> Abs  x                  (compt (x:vv) b)
    Typed t _   -> compt vv t
    _ -> case findAMatch defs (mkApp f xx) of 
           Ok (g,b) -> mkAbs yy (compt vv' (substTerm vv' g' b))
                         where 
                           g'  = [(x,alphaFresh vv' t) | (x,t) <- g]
                           vv' = yy ++ vv
           _  -> mkAbs yy (mkApp f xx)
          where 
            (yy,f,xx) = (yy2,h,map (compt (yy2 ++ vv)) zz) 
                            where Ok (yy2,h,zz) = termForm (beta c) 
            defs = case f of 
                     Cons g -> case lookupType g abstr of
                                 Ok t -> [(PattCons g [],t)]
                                 _ -> defsOfFun g abstr 
                     _ -> []

beta :: Trm -> Trm
beta c = case c of
  App (Abs x b) a -> beta $ substTerm [] [(x,beta a)] (beta b)
  App f         a -> let a'= beta a ; f' = beta f in 
                     (if a'==a && f'==f then id else beta) $ App f' a'
  Prod x a b      -> Prod x (beta a) (beta b)
  Abs x b         -> Abs x (beta b)
  _               -> c

substTerm  :: [Symb] -> Substitution a -> Term a  -> Term a
substTerm ss g c = case c of
  Var x       -> maybe c id $ lookupId x g
  App f a     -> App (substTerm ss g f) (substTerm ss g a)
  Abs x b     -> let y = mkFreshIdent ss x in 
                   Abs y (substTerm (y:ss) ((x, Var y):g) b)
  Prod x a b  -> let y = mkFreshIdent ss x in 
                   Prod y (substTerm ss g a) (substTerm (y:ss) ((x, Var y):g) b)
----  Meta m      -> Closure g c --- the substitution should remain!
  _           -> c
 where
  lookupId x s = lookup (symid x) [(symid y, v) | (y,v) <- s] ---
  g' = [(x,t) | (x,t) <- g, notx x t]
         where
           notx x (Var y) = x /= y
           notx _ _       = True

closureSubst :: Trm -> Trm
closureSubst = cls [] where 
  cls xs tr = case tr of
    Closure [] t -> cls xs t
    Closure g (Meta m) -> if null g' then tr else Closure g' (Meta m)
                     where g' = filter (not . triv) [(x,cls xs v) | (x,v) <- g]
    Closure g t -> substTerm xs g' (cls xs t) --- [] or xs ??
                     where g' = filter (not . triv) [(x,cls xs v) | (x,v) <- g]
    App f a -> App (cls xs f) (cls xs a) --- should be covered by _
    _ -> composSafeOp (cls xs) tr ---
  triv (x,Var y) = eqStrIdent y x --- 
  triv _ = False

-- pattern matching

findAMatch :: [Definition] -> Trm -> Err (Substit,Trm)
findAMatch [] t = Bad "no match"
findAMatch ((p,b) :dd) t = case matches p t of 
  Ok g -> Ok (g,b)
  _    -> findAMatch dd t
 where
   matches p c = case (p,termForm c) of
     (PattVar x, _) -> Ok [(x,c)]
     (PattCons c xx, Ok ([], Cons f, bb)) -> 
        if   f `eqStrIdent` c && length xx == length bb && 
             all (\ (x,b) -> ifMatch x b) (zip xx bb)
        then Ok (concat [okError (matches x b)| (x,b) <- zip xx bb])
        else Bad "pattern does not match application"
     (_,_) -> Bad "pattern does not match term"
   ifMatch p c = isNotError $ matches p c

alphaFresh :: [Symb] -> Trm -> Trm
alphaFresh v c = alphaConv (newVars v c) c where
   newVars v c =
     case c of App f a -> let g  = newVars v f in g ++ newVars (map snd g ++ v) a
               Abs x b -> let x' = mkFreshIdent v x  in (x, x') : newVars (x' : v) b
               _       -> []
   alphaConv g c =
     case c of App f a -> App (alphaConv g f) (alphaConv g a)
               Abs x b -> case lookup x g of Just x' -> Abs x' (alphaConv g b)
                                             _       -> Abs x  (alphaConv g b)
               Var x   -> case lookup x g of Just x' -> Var x'
                                             _       -> c
               _       -> c

betaCompute :: AbstractST -> Trm -> Trm
betaCompute defs = beta . (compute defs)

-- definitional equality

eqTerm :: AbstractST -> Trm -> Trm -> Bool
eqTerm defs a b = 
  betaCompute defs (closureSubst a) 
  `alphaEq` 
  betaCompute defs (closureSubst b)

eqType :: AbstractST -> Type -> Type -> Bool
eqType = eqTerm

alphaEq :: Eq a => Term a -> Term a -> Bool
alphaEq tr tr' =
 case (tr,tr') of
   (Abs x b,  Abs  x' b')      -> alphaEq b (substTerm [x] [(x',Var x)] b')
   (Prod x a b, Prod x' a' b') -> alphaEq a a' && 
                                  alphaEq b (substTerm [x] [(x',Var x)] b')
   (App f a,  App f' a')       -> alphaEq f f' && alphaEq a a'
   (Cons a, Cons b)            -> eqStrIdent a b
   _                           -> tr == tr'

