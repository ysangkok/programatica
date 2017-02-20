module AgdaToGrammar where

import qualified UAbstract as A
import Operations
import Types
import Grammar
import PGrammar (pEntry)
import Skeleton(skEntry)
import A2GSyntax (GKind (..), gKind, prodFormA, varsOfContext, decls2defs)

-- annotated Agda theories as GF grammars. AR 15/10/1999 -- 19/11

-- datatypes
type AType   = A.Exp
type GType   = Term
type ADef    = A.DefB
type GDef    = (Ident, GType)
type ATheory = [ADef]

type AATheory   = [(ADef, Entry)] -- annotated Agda theory
type Annotation = (Ident, Entry)  -- Entry comes from GF

-- to extend the grammar agda0 by an annotated Agda theory
a2gG :: Grammar -> AATheory -> Grammar  
a2gG gr@((tok,pars,ops),(cats,vars,rules,defs)) ath = 
 ((tok,pars,ops),(cats,vars,rules ++ rules',defs)) where
  rules' = map adef2rule ath

-- to produce a GF rule from an annotated Agda definition
adef2rule :: (ADef, Entry) -> (Ident,Rule)
adef2rule (def,ent) = (fun,(typ,ent)) where (fun,typ) = a2gD def

-- to produce an annotated Agda theory from a theory and list of annotations
a2aatheory :: Grammar -> (ATheory,[Annotation]) -> AATheory
a2aatheory gr das@(defs,_) = zip defs (map ((findEntry gr das) . a2gD) defs)

findEntry :: Grammar -> (ATheory,[Annotation]) -> GDef -> Entry
findEntry gr (defs,anns) def@(fun,typ) = 
 maybe (defaultEntry gr fun typ) id (lookup fun anns)

-- auxiliaries

a2gDD :: ADef -> [ADef] -- to open packages
a2gDD d =
 case d of
   A.Value _ -> [d]
   A.Package (_,(cont, A.PackageDef decls)) -> filter real $ decls2defs decls
   _ -> []
  where 
   real (A.Open _) = False
   real _          = True

a2gD :: ADef -> GDef -- systematically ignoring definiens
a2gD d =
 case d of
   A.Value (A.Var f, (cont, _, typ)) -> mkGDef f cont typ
   _ -> ("IllegalDefinition", Ground (Cat "Set") []) --- for robustness

mkGDef :: Ident -> A.Context -> AType -> GDef
mkGDef f (A.Ctx _ cont) typ = (f, prods cont' typ') where 
 cont' = concat [mkArgTypes (Symb x) typ | (A.Var x, typ) <- cont ++ cont2]
 typ'  = a2gT False typ2
 (cont2,typ2) = prodFormA typ

defaultEntry :: Grammar -> String -> Type -> Entry
defaultEntry gr@((_,pars,_),(cats,_,_,_)) f ty = skEntry (pars,cats) f ty

a2gT :: Bool -> AType -> GType
a2gT isArg t = 
 case t of
   A.ESort _                -> gfSet isArg
   A.ESig ll                -> case ll of
                                 A.SigField _ typ:_ -> a2gT isArg typ -- ??
                                 []        -> gfElem isArg
   A.EPi (A.Var x A.:- a) b -> a2gT isArg b
   A.EAnnot _ t' -> a2gT isArg t'
   _                        -> gfElem isArg

mkArgTypes :: Symb -> AType -> Context
mkArgTypes x typ = varTyps ++ [(x,a2gT True t)] where
 varTyps = [(Symb z, Ground (Cat "Var") []) | (A.Var z,_) <- xx]
 (xx,t)  = prodFormA typ 

gfSet :: Bool -> GType 
gfSet isArg = Ground (Cat (if isArg then "Set" else "BsSet")) []

gfElem :: Bool -> GType 
gfElem isArg = Ground (Cat  (if isArg then "Elem" else "BsElem")) [] 

