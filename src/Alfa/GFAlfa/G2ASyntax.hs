module G2ASyntax where

import Types (Cat(..))
import UAbstract
import GSyntax
import PrTypes (prTerm)
import A2GSyntax (termFormA)
import AuxParsing 
import Chart (parseC)
import Operations ()
import Grammar ()

-- from GSyntax to Alfa syntax. AR 19/11/1999 -- 13/12

-- parsing

parseExpr :: PEnv -> String -> [Exp]
parseExpr e s = map ((ffg :: (GExpr -> Exp)) . fg) $ parseC e (Cat "Expr",[],[],0) s

parseSet :: PEnv -> String -> [Exp]
parseSet e s = map ((ffg :: (GSet -> Exp)) . fg) $ parseC e (Cat "Set",[],[],0) s

parseElem :: PEnv -> String -> [Exp]
parseElem e s = map ((ffg :: (GElem -> Exp)) . fg) $ parseC e (Cat "Elem",[],[],0) s

-- syntax-level transformations

ffgDef :: GDef -> DefB
ffgDef (GDfOpen var vars) = 
 Open (ffg var, [(x, (Nothing,Nothing)) | x <- ffgVars vars])
ffgDef (GDfPackage var cont defs) = 
 Package (ffgVar var,(ffgContext cont, PackageDef (ffgDefs defs)))
ffgDef (GDfElim var cont set e bb) =
 Value (ffgVar var,(ffgContext cont, ffg set, ECase (ffg e) (ffgBranches bb)))
ffgDef (GDfIntro var cont set e) =
 Value (ffgVar var,(ffgContext cont, ffg set, (ffg e)))
ffgDef (GDfThm bs cont set e) =
 Value (getAVar bs,(ffgContext cont, ffg set, (ffg e)))
ffgDef (GDfComment s) =
 CommentDef (ffgString s)
ffgDef (GDfBindExpl e1 e2) =
 Binding (getAVar e1, ffg e2)
ffgDef (GDfBindRec e1 e2 bb) =
 Binding (getAVar e1, ECase (ffg e2) (ffgBranches bb))
ffgDef (GDfExpl e0 cont set e) =
 Value (getAVar e0,(ffgContext cont, ffg set, (ffg e)))
ffgDef (GDfRec e0 cont set e bb) =
 Value (getAVar e0,(ffgContext cont, ffg set, ECase (ffg e) (ffgBranches bb)))
ffgDef (GDfData bs cont cc) =
 Data (getAVar bs,(ffgContext cont, ffgConstructors cc))

class Fg a => FFg a where 
  ffg :: a -> Exp -- a two-variable class would be nicer!

instance FFg GSet where
 ffg (GMetaSet s) = EMeta (read (ffgString s))
 ffg (GLetSet defs set) = ELet (ffgDefs defs) (ffg set)
 ffg (GProjSet el (Gvar s)) = EProj (ffg el) (Label (ffgString s))
 ffg (GSig cont) = ESig [SigField (Label l) e | (Var l, e) <- ffgBindings cont]
 ffg (GabstrSet cont e) = eabss (ffgContext cont) (ffg e)
 ffg (GFun cont e) = epis (ffgContext cont) (ffg e)
 ffg (GemptySig) = ESig []
 ffg (GemptySet) = ESum []
 ffg (GemptyCaseSet e) = ECase (ffg e) []
 ffg (GCaseSet e bb) = ECase (ffg e) (ffgBranches bb)
 ffg (GData cc) = ESum (ffgConstructors cc)
 ffg (GUseBsSet bs) = ffg bs
 ffg (GAppSet set ee) = app (ffg set) (ffgExprs ee)
 ffg (GvarSet v) = ffg v

instance FFg GElem where
 ffg (GMetaElem s) = EMeta (read (ffgString s))
 ffg (GLetElem defs e) = ELet (ffgDefs defs) (ffg e)
 ffg (GStruct defs) = EStr (ffgDefs defs)
 ffg (GProjElem el (Gvar s)) = EProj (ffg el) (Label (ffgString s))
 ffg (Gabstract cont e) = eabss (ffgContext cont) (ffg e)
 ffg (GemptyCaseElem e) = ECase (ffg e) []
 ffg GemptyStruct = EStr []
 ffg (GCaseElem e bb) = ECase (ffg e) (ffgBranches bb)
 ffg (GUseBsElem bs) = ffg bs
 ffg (GconElem c) = ECon (Con (ffgString c)) []
 ffg (GAppElem e ee) = app (ffg e) (ffgExprs ee)
 ffg (GvarElem v) = ffg v

instance FFg GBsSet where
 ffg (GBsSet f ee) = app (EVar (Var f)) (map ffg ee)

instance FFg GBsElem where
 ffg (GBsElem f ee) = app (EVar (Var f)) (map ffg ee)

instance FFg GExpr where
 ffg (GsetExpr e) = ffg e
 ffg (GelemExpr e) = ffg e

instance FFg GVar where
 ffg v = EVar (ffgVar v)


ffgDefs :: GDefs -> Decls
ffgDefs dd = [Decl True [defA (ffgDef d) | d <- getDefs dd]] where
 getDefs GemDefs = []
 getDefs (GmrDefs def defs) = def : getDefs defs

ffgContext :: GContext -> Context
ffgContext = ctx . ffgBindings

ffgBindings GemCont = []
ffgBindings (GmrCont decl cont) = ffgDecl decl ++ ffgBindings cont

ffgDecl :: GDecl -> [(Var,Exp)]
ffgDecl (GcnDecl vars set) = let e = ffg set in [(x,e) | x <- ffgVars vars]

ffgString :: GString -> String
ffgString (GString s) = s

ffgVar :: GVar -> Var
ffgVar (Gvar s) = Var (ffgString s)

ffgVars :: GVars -> [Var]
ffgVars (GoneVar v) = [ffgVar v]
ffgVars (GmrVars v vv) = ffgVar v : ffgVars vv

ffgBranches :: GBranches -> Branches
ffgBranches GemBran = []
ffgBranches (GmrBran b bb) = ffgBranch b : ffgBranches bb

ffgBranch :: GBranch -> Branch
ffgBranch (GbranElem c e) = Branch (Con c',(xx,ffg e)) where (c',xx) = ccForm c
ffgBranch (GbranSet c e) = Branch (Con c',(xx,ffg e)) where (c',xx) = ccForm c

ffgExprs :: GExprs -> [Exp]
ffgExprs GemExpr = []
ffgExprs (GmrExpr e ee) = ffg e : ffgExprs ee

ffgConstructors :: GConstructors -> [Constructor]
ffgConstructors GemCon = []
ffgConstructors (GmrCon c cc) = ffgCon c : ffgConstructors cc where
 ffgCon (Gconstr e cont) = Constr (Con (fst (ccForm e)), (ffgContext cont, []))

-- auxiliaries

ccForm :: GElem -> (String,[Var])
ccForm (GUseBsElem (GBsElem c ee)) = (c, getVarsFromExprs (map ffg ee))
ccForm (GAppElem (GconElem (GString c)) ee) = (c,getVarsFromExprs (ffgExprs ee))
ccForm e = ("BRANCH" ++ prTerm (gf e), [])

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

