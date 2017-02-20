module AlfaToGrammar where

import qualified AlfaPluginKit as A
import AIdent
import Operations
import Tokens
import Grammar
import PrGrammar
import SymbolTable
import Update
import Macros
import A2GSyntax 
import GFCFIdents

-- annotated Alfa theories as GF grammars. AR 15/10/1999 -- 19/11 -- 17/7/2000

-- datatypes
type AType   = A.Exp
type GType   = Trm
type ADef    = A.DefB
type GDef    = Def
type ATheory = [ADef]

type ATok    = Str ---
type AToks   = [ATok] ---

type AATheory   = (ATheory, [Annotation]) -- annotated Alfa theory
type Annotation = LDef AToks

type AbstractAST = AbstractST
type ConcreteAST = ConcreteST AToks
type GrammarAST  = GrammarST AToks
type GFCFA = GFCF ATok

-- to extend the grammar agda0 by an annotated Alfa theory
a2gG :: GrammarST AToks -> AATheory -> GrammarST AToks
a2gG gr ath = plainUpdOptGrammarST gr ath' where
 ath' = aatheory2grammar ath

-- to produce a GF grammar from an annotated Alfa theory
aatheory2grammar :: AATheory -> Grammar AToks
aatheory2grammar (adefs,ldefs) = Grammar (atheory2abstract adefs, Concrete ldefs)

atheory2abstract :: ATheory -> Abstract
atheory2abstract adefs = Abstract (map a2gD (concatMap a2gDD adefs))

alfaDecls2Abstract :: A.Decls -> Abstract
alfaDecls2Abstract decls =
  atheory2abstract [def | A.Decl _ defs <- decls, A.DefA _ def <- defs]

-- to produce a default linearization
defaultGFAlfaLin :: Ident -> Type -> Annotation
defaultGFAlfaLin fun typ = DefLin fun xx (mkAlfaCons (parenthIf lterm)) where
  lterm = foldr1 Concat (fun' : map useAlfaExp (map Var xx))
  fun'  = Tok (readTok (idFromGF (prt fun)))
  mkAlfaCons c = mkApp (Cons (zIdent "mkAlfaCons")) [c]
  useAlfaExp e = mkApp (Cons (zIdent "useAlfaExp")) [e]
  xx = case typeForm typ of
         Ok (c,_,_) -> mkFreshIdents [] (replicate (length c) (zIdent "x"))
         _ -> []
  parenthIf t = if null xx then t else 
                   Concat (Tok (readTok "(")) (Concat t (Tok (readTok ")")))

-- to complete a grammar by providing defaults for missing annotations
addDefaultAnnots :: Abstract -> [Annotation] -> [Annotation]
addDefaultAnnots (Abstract funs) annots = map (uncurry defaultGFAlfaLin) wanted 
  where
    wanted = [(fun,typ) | DefFun fun typ <- funs, noLin fun]
    noLin fun = notElem (symid fun) [(symid f) | DefLin f _ _ <- annots]
                         --- this is terrible. AR

-- auxiliaries

a2gD :: ADef -> GDef -- systematically ignoring definiens
a2gD d =
 case d of
   A.Value (A.Var s, (cont, _, typ)) | isConstrId s -> mkGDefCC s cont typ
     -- name clash problem!!
   A.Value (f, (cont, _, typ)) -> mkGDef (consToGF f) cont typ
   A.Data  (f, (cont, _     )) -> mkGDef (consToGF f) cont A.eSet
   A.Axiom (f, (cont,    typ)) -> mkGDef (consToGF f) cont typ
   _ -> error "IllegalDefinition" ---

a2gDD :: ADef -> [ADef] -- to open packages
a2gDD d =
 case d of
   A.Value (_,(c,A.ESum cc,_)) -> d : map mkConstrDef cc
   A.Value _ -> [d]
   A.Axiom _ -> [d]
   A.Data (_,(cont,cc)) -> d : map mkConstrDef cc ++ map (mkConstrFunDef cont) cc
   A.Package (_,(cont, A.PackageDef decls)) -> 
     concat $ map a2gDD $ filter real $ decls2defs decls
   _ -> []
  where 
   real (A.Open _) = False
   real _          = True

mkConstrDef :: A.Constructor -> ADef
mkConstrDef (A.Constr (c,(cont,_))) = 
  A.Value (A.Var (constrToGF c),(cont,A.ePreMetaVar, A.ePreMetaVar))
     -- name clash problem!!

mkConstrFunDef :: A.Context -> A.Constructor -> ADef
mkConstrFunDef cont0 (A.Constr (A.Con c,(cont,_))) = 
  A.Value (A.Var c,(cont0 `addAContext` cont,A.ePreMetaVar, A.ePreMetaVar))

mkGDef :: String -> A.Context -> AType -> GDef
mkGDef f (A.Ctx _ cont) typ = DefFun (zIdent f) (mkProd (cont', typ', [])) where 
 cont' = concat [mkArgTypes (zIdent x) typ | (A.Var x, typ) <- cont ++ cont2]
 typ'  = a2gT False typ2
 (cont2,typ2) = prodFormA typ

-- for a constructor: cannot require eta expansion!
mkGDefCC :: String -> A.Context -> AType -> GDef
mkGDefCC f (A.Ctx _ cont) typ = DefFun (zIdent f) (mkProd (cont', typ', [])) where 
 cont' = [(zIdent x, gfExp True) | (A.Var x, _) <- cont ++ cont2]
 typ'  = a2gT False typ2
 (cont2,typ2) = prodFormA typ

a2gT :: Bool -> AType -> GType
a2gT isArg t = 
 case t of
   A.EPi (A.Var x A.:- a) b -> a2gT isArg b
   A.EAnnot _ t'            -> a2gT isArg t'
   _                        -> gfExp isArg

mkArgTypes :: Ident -> AType -> Cont
mkArgTypes x typ = varTyps ++ [(x,a2gT True t)] where
 varTyps = [(zIdent z, Var (zIdent "Exp")) | (v@(A.Var z),_) <- xx,v/=A.noname]
 (xx,t)  = prodFormA typ 

gfExp :: Bool -> GType 
gfExp isArg = Var (zIdent (if isArg then "Exp" else "Cons"))

addAContext :: A.Context -> A.Context -> A.Context
addAContext (A.Ctx m dd) (A.Ctx _ dd') = A.Ctx m (dd ++ dd') --- from Alfa?

