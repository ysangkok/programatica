module Refine where

import Operations
import Tokens ()
import Predefined (exampleLiterals)
import Grammar
import Macros
import SymbolTable
import Lookup (lookupCat, lookupFun, funsForCat)
import PrGrammar
import Unify
import TC
import ComputeTerm
import List (nub, nubBy, partition, (\\))

-- AR 9/5/2000 -- 4/2/2001

-- the abstract type of term + metas

newtype TermInfo = TI (Trm, Type, Cont, Problems)

-- refinement operations

refineWithTrm :: AbstractST -> TermInfo -> MetaSymb -> Trm -> Err TermInfo
refineWithTrm abs ti@(TI (trm0,ty0,co0,ps0@(ms0,cs0))) meta trm = do
  let trm' = refreshMetasInTrm (metaSymbsOfTI ti) trm
  (i,s,c) <- infoOfMeta ti meta
  typ     <- typeOfMeta ti meta
---  ps      <- checkType (abs,i,s,c) ps0 trm' typ ---
---  let (ms,cs) = problemsOut abs ps              ---
  (ms,cs) <- justTypeCheck abs c trm' typ ---
  let cs1 = addConstr (Meta meta,trm') (cs ++ cs0)
  ti' <- derivedRefinements abs (TI (trm0, ty0, co0, (ms ++ ms0, cs1)))
  return $ computeConstrsInTI abs ti'

refineWithRef :: AbstractST -> TermInfo -> MetaSymb -> Trm -> Type -> Err TermInfo
refineWithRef abs ti meta ref typ = do
  bounds <- contextOfMeta ti meta
  cont <- contextOfLType typ
  trm <- mkIncomplTrm (map fst bounds) cont ref
  refineWithTrm abs ti meta trm

refineWithFun :: AbstractST -> TermInfo -> MetaSymb -> Fun -> Err TermInfo
refineWithFun abs ti meta fun = do
  typ  <- lookupFun fun abs 
  refineWithRef abs ti meta (Cons fun) typ

derivedRefinements :: AbstractST -> TermInfo -> Err TermInfo
derivedRefinements abs ti@(TI (trm, ty, co, ps@(ms, cs))) = do
  let (msubst,cs1) = reduceConstrs cs
      ms1 = [(m,tc) | (m,tc) <- ms, not (elem m (map fst msubst))]
      ti1 = metaSubstTI msubst (TI (trm, ty, co, (ms1, cs1)))
  solveTrivials abs ti1

-- an expensive operation rechecking whole info
checkTermInfo :: AbstractST -> TermInfo -> Err TermInfo
checkTermInfo abs ti@(TI (trm,ty,co,ps)) = do
  ps1 <- justTypeCheck abs co ty  TypeType
  ps2 <- justTypeCheck abs co trm ty  --- co as well ?
  ti' <- derivedRefinements abs (TI (trm,ty,co, foldr1 unionProblems [ps,ps1,ps2]))
  return $ computeConstrsInTI abs ti'

computeConstrsInTI :: AbstractST -> TermInfo -> TermInfo
computeConstrsInTI abs ti@(TI (trm,ty,co,(ms,cs))) = 
  TI (trm,ty,co,(ms, filter (\ (x,y) -> x /= y) (computeConstrs abs cs)))

-- basic operations on TermInfo

mkTermInfo :: Trm -> Type -> Cont -> Metas -> Constraints -> TermInfo
mkTermInfo t ty co qs cs = TI (t,ty,co,(qs,cs))

infoOfTI :: TermInfo -> (Trm, Type, Cont, Metas, Constraints)
infoOfTI (TI (tr, ty, co, (ms,cs))) = (tr, ty, co, ms, cs)

termOfTI     ti = case infoOfTI ti of (t, _, _, _, _) -> t
typeOfTI     ti = case infoOfTI ti of (_, t, _, _, _) -> t
contextOfTI  ti = case infoOfTI ti of (_, _, c, _, _) -> c
metasOfTI    ti = case infoOfTI ti of (_, _, _, q, _) -> q
constrsOfTI  ti = case infoOfTI ti of (_, _, _, _, c) -> c

metaSymbsOfTI = map fst . metasOfTI

isCompleteTI, isPossibleTI, isImpossibleTI :: TermInfo -> Bool
isCompleteTI   = null . metasOfTI
isPossibleTI   = all consistentConstrs . constrsOfTI
isImpossibleTI = not . isPossibleTI

typeOfMeta ti meta = do 
  (t,_) <- maybeErr ("unknown meta" +++ prt meta) $ lookup meta $ metasOfTI ti
  return t
infoOfMeta ti meta = do 
  (_,c) <- maybeErr ("unknown meta" +++ prt meta) $ lookup meta $ metasOfTI ti
  return c
contextOfMeta ti meta = do
  (_,_,c) <- infoOfMeta ti meta
  return c

firstMetaOfTI :: TermInfo -> Err MetaSymb
firstMetaOfTI ti = case metaSymbsOfTI ti of
  []   -> Bad "no metas"
  m:_  -> Ok $ m

addConstr (x,y) constrs = (x,y) : constrs

initTermInfo :: AbstractST -> Cat -> Err TermInfo --- not OK yet 10/7/2000
initTermInfo st cat = do                          --- try e.g. Elem in haskell.*
  let cont = errVal [] $ lookupCat cat st
  typ <- mkIncomplTrm [] cont (Cons cat)
  let trm = Meta $ mkFreshMeta [] cat
  checkTermInfo st $ mkTermInfo trm typ [] [] []

metaSubstTI :: Unifier -> TermInfo -> TermInfo
metaSubstTI msubst ti@(TI (tr,ty,co,(qs,cs))) = TI (tr2, ty2, co2, (qs2,cs2)) where
  tr2 = subsub tr
  ty2 = subsub ty
  co2 = subsubc co
  qs2 = [(x, (subsub t, (i, subsubc s, subsubc c))) | (x,(t,(i,s,c))) <- qs]
  cs2 = [(subsub t1, subsub t2) | (t1,t2) <- cs]
  subsub = closureSubst . substMetas msubst
  subsubc c = [(x, subsub t) | (x,t) <- c]

mkIncomplTrm :: [Ident] -> Cont -> Trm -> Err Trm
mkIncomplTrm bounds cont ref = do
  xx0 <- mapM (typeSkeleton . snd) cont
  let (xxs,cs) = unzip [(length hs, c) | (hs,c) <- xx0]
      xx       = zip xxs (map Meta (mkFreshMetas [] cs))
  let args = [mkAbs xs t | (i,t) <- xx, let xs = mkIdents bounds i]
  return $ mkApp ref args

-- nub symmetrically equal constraints, then unify
reduceConstrs :: Constraints -> (Unifier, Constraints)
reduceConstrs = flip unifyAll [] . nubBy eqConstr where
  eqConstr (a,b) (c, d) = (a == c && b == d) || (a == d && b == c)  

mkFirstMeta :: TermInfo -> MetaSymb -> TermInfo
mkFirstMeta ti m = mkTermInfo t y c ms' cs where
  ms' = uncurry (++) $ partition ((==m) . fst) ms
  (t,y,c,ms,cs) = infoOfTI ti

addMetasToTInfo :: TermInfo -> Metas -> TermInfo
addMetasToTInfo ti ms' = 
  let (t,y,c,ms,cs) = infoOfTI ti in mkTermInfo t y c (ms ++ ms') cs

addConstrsToTInfo :: TermInfo -> Constraints -> TermInfo
addConstrsToTInfo ti cs' = 
  let (t,y,c,ms,cs) = infoOfTI ti in mkTermInfo t y c ms (cs ++ cs')

sortGoalsFromLeft :: TermInfo -> TermInfo
sortGoalsFromLeft ti = mkTermInfo trm y c qs' cs where
  (trm,y,c,qs,cs) = infoOfTI ti
  qs' = if null gsInT then qs else [(q,qi) | q <- nub gsInT, (q',qi) <- qs, q == q']
  gsInT = goalsInTerm trm
  goalsInTerm t = case t of
    App f a -> goalsInTerm f ++ goalsInTerm a
    Abs _ b -> goalsInTerm b
    Meta m -> [m]
    _ -> []

-- perform unique refinements
solveTrivials :: AbstractST -> TermInfo -> Err TermInfo
solveTrivials ab ti@(TI (tr, ty, co, (qs, cs))) = stv allRefs ti where
  allRefs = [(m, refinementsOfGoal ab ti m) | (m,_) <- qs] 
  stv refs ti = case firstTrivGoal refs of
    Just ((m,(t,ty)),rest) -> do
      ti' <- refineWithRef ab ti m t ty
      --- before, as stv was part of refineWith: return ti'
      --- stv rest ti' -- not good either, since rest may shrink!
      solveTrivials ab ti'
    _  -> return ti
  firstTrivGoal qq = case qq of
    mtt@(m,[tty]) : rest | not (isRecursiveType (snd tty)) -> Just ((m,tty),qq)
    _        : q -> firstTrivGoal q  --- a subtlety : otherwise may loop...but
    _            -> Nothing          --- we should detect circular chains as well!

-- refinements from theory & context
refinementsOfGoal :: AbstractST -> TermInfo -> MetaSymb -> [(Trm,Type)]
refinementsOfGoal abstr ti meta = errList $ do
  typ0 <- typeOfMeta    ti meta
  cont <- contextOfMeta ti meta
  let comp = compute abstr
  let typ  = comp typ0
  cat <- valCat typ
  let funs = [(Cons f,ty) | (f,t) <- funsForCat cat abstr, 
                            let ty = comp t,
                            Ok t' <- [valType ty],
                            consistentTerms  t' typ]
      lits = [(Literal c s, Cons c) | (c,ss) <- exampleLiterals,
                                      eqCat cat c,
                                      s <- ss]
      vars = [(Var x,t') | (x,t) <- cont, 
                           let t' = comp t,
                           consistentTerms t' typ]
  return $ funs ++ lits ++ vars

computeConstrs :: AbstractST -> Constraints -> Constraints
computeConstrs abs cs = [(compute abs x, compute abs y) | (x,y) <- cs]

-- potentially unifiable terms ; assumes terms are in normal form
-- this is just a heuristic to display possible refinements: 
-- too liberal is better than too restrictive
consistentTerms :: Trm -> Trm -> Bool
consistentTerms a b = 
 case (a, b) of
   (Meta _,     _     )     -> True
   (Var _ ,     _     )     -> True
   (_,          Meta _)     -> True
   (_,          Var _ )     -> True
   (Typed t _,  e)          -> cts t e
   (e,          Typed t _)  -> cts e t
   (Cons c,     Cons d)     -> eqStrIdent d c
   (App f a   , App g b)    -> cts f g && cts a b
   (Abs x b,    Abs y c)    -> cts b c
   (Prod x a f, Prod y b g) -> cts a b && cts f g
   (Literal s a, Literal t b) -> s == t && a == b
   (Closure g a, Closure d b) -> cts a b   --- could check cts g d as well... 
   (Closure _ (Meta _),    _) -> True
   (_,    Closure _ (Meta _)) -> True
   (_         , _)          -> False
  where cts = consistentTerms

consistentConstrs (a,b) = or [incompleteTerm a , incompleteTerm b , a == b]

incompleteTerm :: Trm -> Bool
incompleteTerm t = case t of
   Meta _ -> True
   App f a -> incompleteTerm f || incompleteTerm a
   Abs _ b -> incompleteTerm b
   Prod _ a b -> incompleteTerm a || incompleteTerm b
   Closure _ a -> incompleteTerm a   -- a can only be Meta
   Typed t _ -> incompleteTerm t
   _ -> False 

-- apply a type-safe non-deterministic transformation, such as compute
mkAllInTI :: (Trm -> [Trm]) -> TermInfo -> [TermInfo]
mkAllInTI tc ti = [mkTermInfo t' y c q p | let (t,y,c,q,p) = infoOfTI ti, t'<- tc t]

prTermInfo :: TermInfo -> String
prTermInfo (TI (tr,ty,co,(qs,cs))) =
 prt tr +++ ":" +++ prt ty +++ prContext co ++
 if null qs && null cs then "" else "\nwhere" ++++
 unlines (map prMetaInfo qs) ++++
 unlines (map prConstr cs)
  where
   prMetaInfo (m,(t,(i,s,c))) = prt m +++ ":" +++ prt t +++ prContext c
----                                  +++ show i +++ prContext s ---- debugging
   prConstr (t1,t2) = prt t1 +++ "=" +++ prt t2
