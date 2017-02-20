module G2ASyntax where

import Tokens
import qualified Grammar as G
import PrGrammar

import UAbstract
import AbstractOps(eFuns,eSet,eType)
import GSyntax
import A2GSyntax (termFormA)
import AIdent

-- from GSyntax to Alfa syntax. AR 19/11/1999 -- 31/3/2000

-- from GExp to Exp, etc.

class Fg a => FFg a where 
  ffg :: a -> Exp -- a two-variable class would be nicer!

fgTrm :: G.Trm -> Exp
fgTrm t = case t of
  G.Meta (G.MetaSymb (_,i)) -> EMeta i
  _ -> ffg ((fg t) :: GExp)

-- syntax-level transformations

instance FFg GExp where
 ffg (GExpMeta int) = EMeta int --- ever needed ??
 ffg (GExpLet defs set) = ELet (ffgDefs defs) (ffg set)
 ffg (GExpProj el (GMkVar s)) = EProj (ffg el) (Label (ffgString s))
 ffg (GExpSig cont) = ESig [SigField (Label l) e | (Var l, e) <- ffgBindings cont]
 ffg (GExpAbs cont e) = eabss (ffgContext cont) (ffg e)
 ffg (GExpFunIndep es e) = eFuns (ffgExprs es) (ffg e)
 ffg (GExpFun cont e) = epis (ffgContext cont) (ffg e)
 ffg (GemptySig) = ESig []
 ffg (GemptySet) = ESum []
 ffg (GemptyCase e) = ECase (ffg e) []
 ffg (GExpCase e bb) = ECase (ffg e) (ffgBranches bb)
 ffg (GExpSum cc) = ESum (ffgConstructors cc)
 ffg (GExpCons bs) = ffg bs
 ffg (GExpApp set ee) = app (ffg set) (ffgExprs ee)
 ffg (GExpStr defs) = EStr (ffgDefs defs)
 ffg GemptyStruct = EStr []
-- ffg (GExpCon (GString c)) = ECon (constrFromGF c)
 ffg (GExpVar (GMkVar v)) = EVar (varFromGF v)
 ffg (GExpTyped e1 e2) = ETyped (ffg e1) (ffg e2)
 ffg (GExpProofOf e1 e2) = EProofOf (ffg e1) (ffg e2)
 ffg (GExpHypothesis e1 (GMkVar v)) = EProofOf (ffg e1) (EVar (varFromGF v))

instance FFg GCons where
  ffg (GCons f ee) = app af (map ffg ee)
    where af = case f of
	         "Set" -> eSet
		 "Typ" -> eType
		 _ -> if isConstrId f
		      then ECon (constrFromGF f)
		      else EVar (consFromGF f)
  ffg (GVarApp f ee) = app (EVar (varFromGF f)) (ffgExprs ee)

instance FFg GVar where
 ffg v = EVar (ffgVar v)

ffgDef :: GDef -> DefB
ffgDef (GDefOpen var vars) = 
 Open (EVar (getAVar var), [OpenArg [] x Nothing Nothing | x <- ffgVars vars])
ffgDef (GDefPackage var cont defs) = 
 Package (getAVar var,(ffgContext cont, PackageDef (ffgDefs defs)))
ffgDef (GDefComment s) =
 CommentDef (ffgString s)
ffgDef (GDefBind e1 e2) =
 Binding (getAVar e1, ffg e2)
ffgDef (GDefVal e0 cont set e) =
 Value (getAVar e0,(ffgContext cont, (ffg e), ffg set))
ffgDef (GDefAxiom e0 cont set) =
 Axiom (getAVar e0,(ffgContext cont, ffg set))
ffgDef (GDefRec e0 cont set e bb) =
 Value (getAVar e0,(ffgContext cont, ffg set, ECase (ffg e) (ffgBranches bb)))
ffgDef (GDefValData bs cont typ cc) =
 Value (getAVar bs,(ffgContext cont, ffg typ, ESum (ffgConstructors cc)))
ffgDef (GDefData bs cont _ cc) =
 Data (getAVar bs,(ffgContext cont, ffgConstructors cc))

ffgDefs :: GListDef -> Decls
ffgDefs dd = [Decl True [defA (ffgDef d) | d <- getDefs dd]] where
 getDefs GNilDef = []
 getDefs (GConsDef def defs) = def : getDefs defs

ffgContext :: GListDecl -> Context
ffgContext = ctx . ffgBindings

ffgBindings GNilDecl = []
ffgBindings (GConsDecl decl cont) = ffgDecl decl ++ ffgBindings cont

ffgDecl :: GDecl -> [(Var,Exp)]
ffgDecl (GMkDecl vars set) = let e = ffg set in [(x,e) | x <- ffgVars vars]

ffgString :: GString -> String
ffgString (GString s) = s

ffgVar :: GVar -> Var
ffgVar (GMkVar s) = varFromGF s

ffgVars :: GListVar -> [Var]
ffgVars (GOneVar v) = [ffgVar v]
ffgVars (GConsVar v vv) = ffgVar v : ffgVars vv

ffgBranches :: GListBranch -> Branches
ffgBranches GNilBranch = []
ffgBranches (GConsBranch b bb) = ffgBranch b : ffgBranches bb

ffgBranch :: GBranch -> Branch
ffgBranch (GMkBranch c e) = Branch (constrFromGF c',(xx,ffg e)) where (c',xx) = ccForm c

ffgExprs :: GListExp -> [Exp]
ffgExprs GNilExp = []
ffgExprs (GConsExp e ee) = ffg e : ffgExprs ee

ffgConstructors :: GListConstr -> [Constructor]
ffgConstructors GNilConstr = []
ffgConstructors (GConsConstr c cc) = ffgCon c : ffgConstructors cc where
 ffgCon (GMkConstr e cont) = Constr (constrFromGF (fst (ccForm e)), (ffgContext cont, []))

-- auxiliaries

ccForm :: GExp -> (String,[Var])
ccForm (GExpCons (GCons c ee)) = (c, getVarsFromExprs (map ffg ee))
--ccForm (GExpApp (GExpCon (GString c)) ee) = (c,getVarsFromExprs (ffgExprs ee))
ccForm e = ("BRANCH" ++ prt (gf e), []) ---

getAVar :: FFg e => e -> Var
getAVar e = 
 case termFormA (ffg e) of
   (EVar v,_) -> v
   _          -> Var $ "exprVAR"

getVarsFromExprs :: [Exp] -> [Var]
getVarsFromExprs = map findVar where
 findVar (EVar v) = v
 findVar e = Var $ "getVAR"

eabss :: Context -> Exp -> Exp
eabss (Ctx _ bs) = eabss' bs
eabss' [] e = e
eabss' ((x,a):c) e = EAbs (x :- a) (eabss' c e)

epis :: Context -> Exp -> Exp
epis (Ctx _ bs) = epis' bs

epis' [] e = e
epis' ((x,a):c) e = EPi (x :- a) (eabss' c e)

