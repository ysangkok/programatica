module Update where

import Operations
import Tokens
import Grammar
import PrGrammar
import Macros
import SymbolTable
import Rename
import Optimize (optimizeLInfo)
import Predefined
import Lookup (lookupFun)
import List (sortBy)

-- update functions. AR 23/1/2000 -- 5/6

-- build and update binary search trees for grammars

mkGrammarST :: Token a => Grammar a -> Err (GrammarST a)
mkGrammarST = updOptGrammarST predefGrammarST

-- update grammar, then optimize linearization by inlining

updOptGrammarST :: Token a => GrammarST a -> Grammar a ->  Err (GrammarST a)
updOptGrammarST  st0 gr = do
  let gr1 = updateGrammarST st0 gr
  optGrammarST st0 gr1

optGrammarST :: Token a => GrammarST a -> GrammarST a ->  Err (GrammarST a)
optGrammarST st0 gr1@(abs,(cnc,mx)) = do
  cnc' <- mapMTree (optimizeNewOper gr1) cnc
  cnc''<- mapMTree (optimizeNewLin (abs,(cnc',mx))) cnc'
  return (abs,(cnc'',mx))
 where
   -- first optimize operations and lintypes
   optimizeNewOper g ii@(_,IdentOper _ _) = optimizeLInfo g ii
   optimizeNewOper g ii@(_,IdentLType _) = optimizeLInfo g ii
   optimizeNewOper g ii = return ii   
   -- then optimize lins and lincats
   optimizeNewLin g ii@(_,IdentLin _ _) = optimizeLInfo g ii 
   optimizeNewLin g ii@(_,IdentLintype _) = optimizeLInfo g ii 
   optimizeNewLin g ii = return ii  

{-- this does not work:
optGrammarST st0 st@(abs,(cnc,mx)) = do
  cnc' <- mapMTree (optimizeLInfo st) cnc
  return (abs, (cnc', mx))

-- moreover,
--- does not work if you want to update by changing old defs : 
---   optimizeNewOper g ii@(ident,IdentOper _ _) = 
---     case lookupTree (const "") ident cnc0 of
---       Bad _ -> optimizeLInfo g ii
---       _ -> return ii                 -- optimize new parts only

--}



plainUpdOptGrammarST :: Token a => GrammarST a -> Grammar a -> GrammarST a
plainUpdOptGrammarST st0 gr = case updOptGrammarST st0 gr of
                         Ok st -> st
                         _ -> st0

tryUpdateSTByDef :: Token a => GrammarST a -> Def -> GrammarST a
tryUpdateSTByDef gr def = 
  plainUpdOptGrammarST gr (Grammar (Abstract [def], emptyConcrete))

tryUpdateSTByLDef :: Token a => GrammarST a -> LDef a -> GrammarST a
tryUpdateSTByLDef gr ldef = 
  plainUpdOptGrammarST gr (Grammar (emptyAbstract, Concrete [ldef]))

-- update a grammar

updateGrammarST :: GrammarST a -> Grammar a -> GrammarST a
updateGrammarST (abs0,cnc0) (Grammar (abs,cnc)) = (abs', cnc') where 
 abs' = updateAbstractST abs0 abs
 cnc' = updateConcreteST cnc0 abs' cnc

updateAbstractST :: AbstractST -> Abstract -> AbstractST
updateAbstractST (abs0,mx0) abs@(Abstract defs) = (st2, mx) where
 (st2,mx)  = (mapTree (renameInfo (st1,mx1)) st1, mx1)
 (st1,mx1) = mkST mx0 abs0 abs0' mkIdentInfos defs
 abs0'     = mapTree (\ (Ident (c,(n,_)),_) -> (c,n)) abs0

-- in concrete ST, constants have the same numbers as in abstract
updateConcreteST :: ConcreteST a -> AbstractST -> Concrete a -> ConcreteST a
updateConcreteST c0@(cnc0,mx0) abs cnc@(Concrete ldefs) = (st1, mx) where
 (st0,mx)  = mkST mx0 cnc0 abs' mkIdentLInfos ldefs
 abs'      = mapTree (\ (Ident (c,(n,_)),_) -> (c,n)) (fst abs)
 st1       = mapTree renameNew st0  -- to avoid renaming old parts
 renameNew ii@(ident,_) = case lookupTree (const "") ident cnc0 of
   Bad _ -> renameLInfo (abs,(st0,mx)) ii
   _ -> ii

-- minimize a grammar

minimizeGrammarST :: Token a => GrammarST a -> GrammarST a
minimizeGrammarST (abs,cnc) = (balanceST abs, minimizeConcreteST cnc)

balanceGrammarST (a,c) = (balanceST a, balanceST c)

minimizeConcreteST :: Token a => ConcreteST a -> ConcreteST a
minimizeConcreteST (cc,m) = (cc',m) where 
  cc' = sorted2tree $ filter good $ tree2list cc
  good (_, IdentOper _ _) = False
  -- good (_, IdentLType _) = False --- these should be removed as well
  good _ = True                     --- fix needed in optGrammarST above

balanceST :: Ord a => (BinTree (a,b), c) -> (BinTree (a,b),c)
balanceST (t,a) = (buildTree (tree2list t),a)
-- balance by sort and rebuild

--- 1/12/2000 as a part of optimization (i -m file).
--- would be better to remove opers, but the tree must be kept in balance.


-- the main auxiliary function
mkST :: Int -> BinTree (Ident,d) -> BinTree (String,Int) -> 
               ([c] -> [(Ident,d)]) -> [c] -> (BinTree (Ident,d),Int)
mkST mx base old mkInfos dd = (updatesTree infos base, length infos + mx') where
 infos = [(Ident (c,(checkOld c n,0)),d) | 
               (n,(Ident (c,_),d)) <- zip [mx'+1 ..] (mkInfos dd)]
 checkOld c n = case lookupTree id c old of
   Ok n' -> n'
   _     -> n
 mx' = mx + length (tree2list old) -- to avoid number clashes

-- to get a grammar back from ST grammar
--- must be completed by definitions
st2grammar :: GrammarST a -> Grammar a
st2grammar (abs,conc) = Grammar (st2abstract abs, st2concrete conc)

st2abstract :: AbstractST -> Abstract
st2abstract = Abstract . (concatMap st2Def) . resortIdents . tree2list . fst 

st2Def (c,info) = case info of
   IdentCat context -> DefCat c context : []
   IdentType typ -> DefType c typ : []
   IdentFun typ -> DefFun c typ : []
   IdentDef pts -> [DefDef p t | (p,t) <- pts]
   IdentData pts -> [DefData (stripAdHocIdent c) pts]

st2concrete :: ConcreteST a -> Concrete a
st2concrete = Concrete . (concatMap st2LDef) . resortIdents . tree2list . fst 

st2LDef (c,info) = case info of
   IdentTagType params -> DefParam c params : []
   IdentOper ty t -> DefOper c ty t : []
   IdentLType ty -> DefLType c ty : []
   IdentLintype ty -> DefLintype c ty : []
   IdentDefault xx t -> DefDefault (stripAdHocIdent c) xx t : []
   IdentLin xx t -> DefLin c xx t : []
   IdentVar cats -> DefVar c cats : []
   IdentTokenizer t -> DefTokenizer t : []
   _ -> []

updateTerm :: AbstractST -> Trm -> Trm
updateTerm st trm = mkTyped st $ fst $ renameTermA st trm

updateTermInContext :: AbstractST -> [Ident] -> Trm -> Trm
updateTermInContext st vs = mkTyped st . fst . renameTermInContext st vs

-- replace each x by <x:A> where A is determined by hte argument place
mkTyped :: AbstractST -> Trm -> Trm
mkTyped st trm = case termForm trm of
  Ok (xx,(Cons fun),args) -> mkTerm (xx,(Cons fun), typedArgs fun args)
  _ -> trm
 where 
  typedArgs fun args = case lookupFun fun st of
    Ok typ -> case typeForm typ of
      Ok (cont,_,_) -> map typedVar (zip cont args)
      _ -> args
    _ -> args
  typedVar ((_,typ),Var x) = Typed (Var x) typ
  typedVar (_,t) = mkTyped st t

updateIdent :: AbstractST -> Ident -> Ident
updateIdent abs c = case updateTerm abs (Var c) of
  Cons c' -> c'
  _ -> c

updateLDef :: Token a => GrammarST a -> LDef a -> (LDef a,(Ident,IdentLInfo a))
updateLDef gr@(_,(cnc,_)) ldef = let
  linfo1 = head $ mkIdentLInfos [ldef] -- never empty
  linfo2 = renameLInfo gr linfo1
  linfo3 = case optimizeLInfo gr linfo2 of
             Ok li -> li
             _ -> linfo2
  ldefs2 = st2LDef linfo2 -- never empty for lin definitions
  in (if null ldefs2 then ldef else head ldefs2, linfo3)

resortIdents :: [(Ident,d)] -> [(Ident,d)]
resortIdents = sortBy (\ (Ident (_,(i,_)),_) (Ident (_,(j,_)),_) -> compare i j)

