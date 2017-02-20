module GSyntax where

import Types
import PrTypes (prTerm)
import Operations () -- type synonyms

-- syntax for Agda-GF interface. AR 11/11/1999 -- 19/11
-- most of this code was generated automatically from the GF grammar
-- by the program GrammarToHaskell

-- first the hand-written code

data GString = GString String

class Gf a where gf :: a -> Term
class Fg a where fg :: Term -> a

instance Gf GString where gf (GString s) = Predef ("String",s)

instance Fg GString where 
 fg t = 
  case termForm t of
    ([],Predef ("String",s),[]) -> GString s
    _ -> error ("no GString " ++ prTerm t)

appc :: String -> [Term] -> Term
appc s tt = apps (Cons (Fun s)) tt

data GBsElem = GBsElem String [GExpr]
data GBsSet  = GBsSet  String [GExpr]

instance Gf GBsElem where
 gf (GBsElem c ee) = appc c (map fromExpr ee) --deviation from pattern

instance Gf GBsSet where
 gf (GBsSet c ee) = appc c (map fromExpr ee) --deviation from pattern

instance Fg GBsElem where
 fg t =
  case termForm t of
    ([], Cons (Fun f), xx) -> GBsElem f (map fg xx)
    _ -> error ("no BsElem " ++ prTerm t)

instance Fg GBsSet where
 fg t =
  case termForm t of
    ([], Cons (Fun f), xx) -> GBsSet f (map fg xx)
    _ -> error ("no BsElem " ++ prTerm t)

fromExpr :: GExpr -> Term 
fromExpr (GelemExpr e) = gf e
fromExpr (GsetExpr  e) = gf e


-- then the generated code


data GDefs =
   GmrDefs GDef GDefs 
 | GemDefs 

data GDef =
   GDfOpen GVar GVars 
 | GDfPackage GVar GContext GDefs 
 | GDfElim GVar GContext GSet GExpr GBranches 
 | GDfIntro GVar GContext GSet GElem 
 | GDfThm GBsElem GContext GSet GElem 
 | GDfComment GString 
 | GDfBindExpl GExpr GExpr 
 | GDfBindRec GExpr GExpr GBranches 
 | GDfExpl GExpr GContext GSet GExpr 
 | GDfRec GExpr GContext GSet GExpr GBranches 
 | GDfData GBsSet GContext GConstructors 

data GSet =
   GMetaSet GString 
 | GProjSet GElem GVar 
 | GLetSet GDefs GSet 
 | GSig GContext 
 | GabstrSet GContext GSet 
 | GFun GContext GSet 
 | GemptySig 
 | GemptySet 
 | GemptyCaseSet GElem 
 | GCaseSet GElem GBranches 
 | GData GConstructors 
 | GUseBsSet GBsSet 
 | GAppSet GSet GExprs 
 | GvarSet GVar 

data GElem =
   GMetaElem GString 
 | GProjElem GElem GVar 
 | GLetElem GDefs GElem 
 | GStruct GDefs 
 | Gabstract GContext GElem 
 | GemptyStruct 
 | GemptyCaseElem GElem 
 | GCaseElem GElem GBranches 
 | GUseBsElem GBsElem 
 | GconElem GString 
 | GAppElem GElem GExprs 
 | GvarElem GVar 

data GVar = Gvar GString 

data GVars =
   GmrVars GVar GVars 
 | GoneVar GVar 

data GContext =
   GmrCont GDecl GContext 
 | GemCont 

data GDecl = GcnDecl GVars GSet 

data GConstructors =
   GmrCon GConstructor GConstructors 
 | GemCon 

data GConstructor = Gconstr GElem GContext 

data GBranches =
   GmrBran GBranch GBranches 
 | GemBran 

data GBranch =
   GbranElem GElem GElem 
 | GbranSet GElem GSet 

data GExprs =
   GmrExpr GExpr GExprs 
 | GemExpr 

data GExpr =
   GsetExpr GSet 
 | GelemExpr GElem 



instance Gf GDefs where
 gf (GmrDefs x1 x2) = appc "mrDefs" [gf x1, gf x2]
 gf GemDefs = appc "emDefs" []

instance Gf GDef where
 gf (GDfOpen x1 x2) = appc "DfOpen" [gf x1, gf x2]
 gf (GDfPackage x1 x2 x3) = appc "DfPackage" [gf x1, gf x2, gf x3]
 gf (GDfElim x1 x2 x3 x4 x5) = appc "DfElim" [gf x1, gf x2, gf x3, gf x4, gf x5]
 gf (GDfIntro x1 x2 x3 x4) = appc "DfIntro" [gf x1, gf x2, gf x3, gf x4]
 gf (GDfThm x1 x2 x3 x4) = appc "DfThm" [gf x1, gf x2, gf x3, gf x4]
 gf (GDfComment x1) = appc "DfComment" [gf x1]
 gf (GDfBindExpl x1 x2) = appc "DfBindExpl" [gf x1, gf x2]
 gf (GDfBindRec x1 x2 x3) = appc "DfBindRec" [gf x1, gf x2, gf x3]
 gf (GDfExpl x1 x2 x3 x4) = appc "DfExpl" [gf x1, gf x2, gf x3, gf x4]
 gf (GDfRec x1 x2 x3 x4 x5) = appc "DfRec" [gf x1, gf x2, gf x3, gf x4, gf x5]
 gf (GDfData x1 x2 x3) = appc "DfData" [gf x1, gf x2, gf x3]

instance Gf GSet where
 gf (GMetaSet x1) = appc "MetaSet" [gf x1]
 gf (GProjSet x1 x2) = appc "ProjSet" [gf x1, gf x2]
 gf (GLetSet x1 x2) = appc "LetSet" [gf x1, gf x2]
 gf (GSig x1) = appc "Sig" [gf x1]
 gf (GabstrSet x1 x2) = appc "abstrSet" [gf x1, gf x2]
 gf (GFun x1 x2) = appc "Fun" [gf x1, gf x2]
 gf GemptySig = appc "emptySig" []
 gf GemptySet = appc "emptySet" []
 gf (GemptyCaseSet x1) = appc "emptyCaseSet" [gf x1]
 gf (GCaseSet x1 x2) = appc "CaseSet" [gf x1, gf x2]
 gf (GData x1) = appc "Data" [gf x1]
 gf (GUseBsSet x1) = appc "UseBsSet" [gf x1]
 gf (GAppSet x1 x2) = appc "AppSet" [gf x1, gf x2]
 gf (GvarSet x1) = appc "varSet" [gf x1]

instance Gf GElem where
 gf (GMetaElem x1) = appc "MetaElem" [gf x1]
 gf (GProjElem x1 x2) = appc "ProjElem" [gf x1, gf x2]
 gf (GLetElem x1 x2) = appc "LetElem" [gf x1, gf x2]
 gf (GStruct x1) = appc "Struct" [gf x1]
 gf (Gabstract x1 x2) = appc "abstract" [gf x1, gf x2]
 gf GemptyStruct = appc "emptyStruct" []
 gf (GemptyCaseElem x1) = appc "emptyCaseElem" [gf x1]
 gf (GCaseElem x1 x2) = appc "CaseElem" [gf x1, gf x2]
 gf (GUseBsElem x1) = appc "UseBsElem" [gf x1]
 gf (GconElem x1) = appc "conElem" [gf x1]
 gf (GAppElem x1 x2) = appc "AppElem" [gf x1, gf x2]
 gf (GvarElem x1) = appc "varElem" [gf x1]

instance Gf GVar where gf (Gvar x1) = appc "var" [gf x1]

instance Gf GVars where
 gf (GmrVars x1 x2) = appc "mrVars" [gf x1, gf x2]
 gf (GoneVar x1) = appc "oneVar" [gf x1]

instance Gf GContext where
 gf (GmrCont x1 x2) = appc "mrCont" [gf x1, gf x2]
 gf GemCont = appc "emCont" []

instance Gf GDecl where gf (GcnDecl x1 x2) = appc "cnDecl" [gf x1, gf x2]

instance Gf GConstructors where
 gf (GmrCon x1 x2) = appc "mrCon" [gf x1, gf x2]
 gf GemCon = appc "emCon" []

instance Gf GConstructor where gf (Gconstr x1 x2) = appc "constr" [gf x1, gf x2]

instance Gf GBranches where
 gf (GmrBran x1 x2) = appc "mrBran" [gf x1, gf x2]
 gf GemBran = appc "emBran" []

instance Gf GBranch where
 gf (GbranElem x1 x2) = appc "branElem" [gf x1, gf x2]
 gf (GbranSet x1 x2) = appc "branSet" [gf x1, gf x2]

instance Gf GExprs where
 gf (GmrExpr x1 x2) = appc "mrExpr" [gf x1, gf x2]
 gf GemExpr = appc "emExpr" []

instance Gf GExpr where
 gf (GsetExpr x1) = appc "setExpr" [gf x1]
 gf (GelemExpr x1) = appc "elemExpr" [gf x1]



instance Fg GDefs where
 fg t =
  case termForm t of
    ([], Cons (Fun "mrDefs"),[x1,x2]) -> GmrDefs (fg x1) (fg x2)
    ([], Cons (Fun "emDefs"),[]) -> GemDefs 
    _ -> error ("no Defs " ++ prTerm t)

instance Fg GDef where
 fg t =
  case termForm t of
    ([], Cons (Fun "DfOpen"),[x1,x2]) -> GDfOpen (fg x1) (fg x2)
    ([], Cons (Fun "DfPackage"),[x1,x2,x3]) -> GDfPackage (fg x1) (fg x2) (fg x3)
    ([], Cons (Fun "DfElim"),[x1,x2,x3,x4,x5]) -> GDfElim (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
    ([], Cons (Fun "DfIntro"),[x1,x2,x3,x4]) -> GDfIntro (fg x1) (fg x2) (fg x3) (fg x4)
    ([], Cons (Fun "DfThm"),[x1,x2,x3,x4]) -> GDfThm (fg x1) (fg x2) (fg x3) (fg x4)
    ([], Cons (Fun "DfComment"),[x1]) -> GDfComment (fg x1)
    ([], Cons (Fun "DfBindExpl"),[x1,x2]) -> GDfBindExpl (fg x1) (fg x2)
    ([], Cons (Fun "DfBindRec"),[x1,x2,x3]) -> GDfBindRec (fg x1) (fg x2) (fg x3)
    ([], Cons (Fun "DfExpl"),[x1,x2,x3,x4]) -> GDfExpl (fg x1) (fg x2) (fg x3) (fg x4)
    ([], Cons (Fun "DfRec"),[x1,x2,x3,x4,x5]) -> GDfRec (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
    ([], Cons (Fun "DfData"),[x1,x2,x3]) -> GDfData (fg x1) (fg x2) (fg x3)
    _ -> error ("no Def " ++ prTerm t)

instance Fg GSet where
 fg t =
  case termForm t of
    ([], Meta (Cat "Set",s), []) -> GMetaSet (GString s) --- by hand
    ([], Cons (Fun "MetaSet"),[x1]) -> GMetaSet (fg x1)
    ([], Cons (Fun "ProjSet"),[x1,x2]) -> GProjSet (fg x1) (fg x2)
    ([], Cons (Fun "LetSet"),[x1,x2]) -> GLetSet (fg x1) (fg x2)
    ([], Cons (Fun "Sig"),[x1]) -> GSig (fg x1)
    ([], Cons (Fun "abstrSet"),[x1,x2]) -> GabstrSet (fg x1) (fg x2)
    ([], Cons (Fun "Fun"),[x1,x2]) -> GFun (fg x1) (fg x2)
    ([], Cons (Fun "emptySig"),[]) -> GemptySig 
    ([], Cons (Fun "emptySet"),[]) -> GemptySet 
    ([], Cons (Fun "emptyCaseSet"),[x1]) -> GemptyCaseSet (fg x1)
    ([], Cons (Fun "CaseSet"),[x1,x2]) -> GCaseSet (fg x1) (fg x2)
    ([], Cons (Fun "Data"),[x1]) -> GData (fg x1)
    ([], Cons (Fun "UseBsSet"),[x1]) -> GUseBsSet (fg x1)
    ([], Cons (Fun "AppSet"),[x1,x2]) -> GAppSet (fg x1) (fg x2)
    ([], Cons (Fun "varSet"),[x1]) -> GvarSet (fg x1)
    _ -> error ("no Set " ++ prTerm t)

instance Fg GElem where
 fg t =
  case termForm t of
    ([], Meta (Cat "Elem",s), []) -> GMetaElem (GString s) --- by hand
    ([], Cons (Fun "MetaElem"),[x1]) -> GMetaElem (fg x1)
    ([], Cons (Fun "ProjElem"),[x1,x2]) -> GProjElem (fg x1) (fg x2)
    ([], Cons (Fun "LetElem"),[x1,x2]) -> GLetElem (fg x1) (fg x2)
    ([], Cons (Fun "Struct"),[x1]) -> GStruct (fg x1)
    ([], Cons (Fun "abstract"),[x1,x2]) -> Gabstract (fg x1) (fg x2)
    ([], Cons (Fun "emptyStruct"),[]) -> GemptyStruct 
    ([], Cons (Fun "emptyCaseElem"),[x1]) -> GemptyCaseElem (fg x1)
    ([], Cons (Fun "CaseElem"),[x1,x2]) -> GCaseElem (fg x1) (fg x2)
    ([], Cons (Fun "UseBsElem"),[x1]) -> GUseBsElem (fg x1)
    ([], Cons (Fun "conElem"),[x1]) -> GconElem (fg x1)
    ([], Cons (Fun "AppElem"),[x1,x2]) -> GAppElem (fg x1) (fg x2)
    ([], Cons (Fun "varElem"),[x1]) -> GvarElem (fg x1)
    _ -> error ("no Elem " ++ prTerm t)

instance Fg GVar where
 fg t =
  case termForm t of
    ([], Cons (Fun "var"),[x1]) -> Gvar (fg x1)
    _ -> error ("no Var " ++ prTerm t)

instance Fg GVars where
 fg t =
  case termForm t of
    ([], Cons (Fun "mrVars"),[x1,x2]) -> GmrVars (fg x1) (fg x2)
    ([], Cons (Fun "oneVar"),[x1]) -> GoneVar (fg x1)
    _ -> error ("no Vars " ++ prTerm t)

instance Fg GContext where
 fg t =
  case termForm t of
    ([], Cons (Fun "mrCont"),[x1,x2]) -> GmrCont (fg x1) (fg x2)
    ([], Cons (Fun "emCont"),[]) -> GemCont 
    _ -> error ("no Context " ++ prTerm t)

instance Fg GDecl where
 fg t =
  case termForm t of
    ([], Cons (Fun "cnDecl"),[x1,x2]) -> GcnDecl (fg x1) (fg x2)
    _ -> error ("no Decl " ++ prTerm t)

instance Fg GConstructors where
 fg t =
  case termForm t of
    ([], Cons (Fun "mrCon"),[x1,x2]) -> GmrCon (fg x1) (fg x2)
    ([], Cons (Fun "emCon"),[]) -> GemCon 
    _ -> error ("no Constructors " ++ prTerm t)

instance Fg GConstructor where
 fg t =
  case termForm t of
    ([], Cons (Fun "constr"),[x1,x2]) -> Gconstr (fg x1) (fg x2)
    _ -> error ("no Constructor " ++ prTerm t)

instance Fg GBranches where
 fg t =
  case termForm t of
    ([], Cons (Fun "mrBran"),[x1,x2]) -> GmrBran (fg x1) (fg x2)
    ([], Cons (Fun "emBran"),[]) -> GemBran 
    _ -> error ("no Branches " ++ prTerm t)

instance Fg GBranch where
 fg t =
  case termForm t of
    ([], Cons (Fun "branElem"),[x1,x2]) -> GbranElem (fg x1) (fg x2)
    ([], Cons (Fun "branSet"),[x1,x2]) -> GbranSet (fg x1) (fg x2)
    _ -> error ("no Branch " ++ prTerm t)

instance Fg GExprs where
 fg t =
  case termForm t of
    ([], Cons (Fun "mrExpr"),[x1,x2]) -> GmrExpr (fg x1) (fg x2)
    ([], Cons (Fun "emExpr"),[]) -> GemExpr 
    _ -> error ("no Exprs " ++ prTerm t)

instance Fg GExpr where
 fg t =
  case termForm t of
    ([], Cons (Fun "setExpr"),[x1]) -> GsetExpr (fg x1)
    ([], Cons (Fun "elemExpr"),[x1]) -> GelemExpr (fg x1)
    ([], Cons (Fun "UseBsElem"),[x1]) -> GelemExpr (GUseBsElem (fg x1)) ---
    ([], Cons (Fun "UseBsSet"),[x1]) -> GsetExpr (GUseBsSet (fg x1)) ---
    ([], Meta (Cat "Set",s), []) -> GsetExpr (GMetaSet (GString s)) ---
    ([], Meta (Cat "Elem",s), []) -> GelemExpr (GMetaElem (GString s)) ---
    ([], Meta (Cat "Expr",s), []) -> GelemExpr (GMetaElem (GString s)) ---
    _ -> error ("no Expr " ++ prTerm t)

