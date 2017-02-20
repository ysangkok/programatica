module TypeCheck where

import Operations
import Grammar (Trm, Term(..), Cont, Type, Patt(..))
import Macros
import Lookup
import Tokens ()
import PrGrammar
import TC
import Refine
import ComputeTerm

-- exporting TC functions with derived refinements

-- consider if a term can be typed, and return either empty list or singleton

typeCheckTerm :: Theory -> Trm -> [Trm]
typeCheckTerm abstr t = errVal [] derRef where
  derRef = do
    ti <- typeInferInfo abstr [] t
    return $ if isPossibleTI ti then [termOfTI ti] else []  

-- return whole term info

typeCheckInfo :: Theory -> Cont -> Trm -> Type -> Err TermInfo
typeCheckInfo st cont trm typ = do
  (qs,cs) <- justTypeCheck st cont trm typ
  derivedRefinements st $ mkTermInfo trm typ cont qs cs
  
typeInferInfo :: Theory -> Cont -> Trm -> Err TermInfo
typeInferInfo st cont trm = do
  (typ,(qs,cs)) <- justTypeInfer st cont trm
  derivedRefinements st $ mkTermInfo trm typ cont qs cs

-- check a Def definition

chDefinition :: Theory -> (Patt,Trm) -> Err Problems
chDefinition gr (patt,def) = do
  let patt' = refreshPatt patt
  let trm   = patt2termMeta patt'
  (typ,cont) <- typeContPatt gr trm
  ttp  <- justTypeCheck gr cont trm typ
  ttt  <- justTypeCheck gr cont def typ
  let (qs,cs) = unionProblems ttp ttt
      cs'     = [(x,y) | (x,y) <- cs, not (consistentTerms x y)] --- too liberal
  return (qs,cs')

typeContPatt :: Theory -> Trm -> Err (Type,Cont)
typeContPatt gr patt = do
  (_,t,patts) <- termForm patt
  (case t of
     Cons fun -> do
       typ       <- lookupFun fun gr
       (xx,c,aa) <- typeForm typ
       let substPt = zip (map fst xx) patts
       -- first the type
       let xx2 = drop (length patts) xx
       let typ = substTerm [] substPt (mkProd (xx2,(Cons c),aa))
       -- then the context
       let xx' = [(v, substTerm [] substPt t) | (v,t) <- xx]
       let cont = cPatts patts xx'
       -- put them together
       return (typ,cont)
     _ -> prtBad "cannot infer type of pattern" t)
 where
   cPatts ps zs = case (ps,zs) of
       (Var x : pp, (_,typ) : xxs)  -> (x, Closure [] typ) : cPatts pp xxs
       (patt  : pp, _       : xxs)  -> contPatt patt ++ cPatts pp xxs
       _  -> []
   contPatt = snd . errVal (undefined, []) . typeContPatt gr



