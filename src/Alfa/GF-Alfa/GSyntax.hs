module GSyntax where

import Tokens
import Grammar
import PrGrammar ---(prt)
import Macros
import Operations
import Predefined

-- syntax for Agda-GF interface. AR 11/11/1999 -- 6/3/2000
-- most of this code was generated automatically from the GF grammar
-- by the program GrammarToHaskell

-- first the hand-written code

data GString = GString String

class Gf a where gf :: a -> Trm
class Fg a where fg :: Trm -> a

instance Gf GString where 
  gf (GString s) = mkLitString s

instance Fg GString where 
 fg t = 
  case termForm t of
    Ok ([], Literal _ s, []) -> GString s
    _ -> error ("no GString" +++ prt t)

data GCons = GCons String [GExp] | GVarApp GString GListExp

instance Gf GCons where 
  gf (GCons c ee) = appc c (map gf ee) --deviation from pattern
  gf (GVarApp s ee) = appc "VarApp" [gf s, gf ee]

instance Fg GCons where
  fg t =
    case termForm t of
      Ok ([], Cons (Ident ("VarApp",_)),[x1,x2]) -> GVarApp (fg x1) (fg x2)
      Ok ([], Cons f, xx) -> GCons (symid f) (map fg xx)
      _ -> error ("no Cons " ++ prt t)


-- then the generated code

data GTheory = GMkTheory GListDef 

data GDefinition = GMkDefinition GDef 

data GObject = GMkObject GExp 

data GExp =
   GemptyCase GExp 
 | GemptyStruct 
 | GemptySig 
 | GemptySet 
-- | GExpCon GString 
 | GExpCase GExp GListBranch 
 | GExpProj GExp GVar 
 | GExpLet GListDef GExp 
 | GExpSum GListConstr 
 | GExpStr GListDef 
 | GExpAbs GListDecl GExp 
 | GExpFun GListDecl GExp 
 | GExpFunIndep GListExp GExp 
 | GExpSig GListDecl 
 | GExpMeta Int 
 | GExpCons GCons 
 | GExpApp GExp GListExp
 | GExpVar GVar 
 | GExpTyped GExp GExp
 | GExpProofOf GExp GExp
 | GExpHypothesis GExp GVar

data GListExp =
   GConsExp GExp GListExp 
 | GNilExp 

data GVar = GMkVar GString 

data GListVar =
   GConsVar GVar GListVar 
 | GOneVar GVar 

data GDef =
   GDefComment GString 
 | GDefOpen GCons GListVar 
 | GDefPackage GCons GListDecl GListDef 
 | GDefBind GCons GExp 
 | GDefAxiom GCons GListDecl GExp 
 | GDefRec GCons GListDecl GExp GExp GListBranch 
 | GDefData GCons GListDecl GExp GListConstr 
 | GDefValData GCons GListDecl GExp GListConstr 
 | GDefVal GCons GListDecl GExp GExp 

data GListDef =
   GConsDef GDef GListDef 
 | GNilDef 

data GDecl = GMkDecl GListVar GExp 

data GListDecl =
   GConsDecl GDecl GListDecl 
 | GNilDecl 

data GConstr = GMkConstr GExp GListDecl 

data GListConstr =
   GConsConstr GConstr GListConstr 
 | GNilConstr 

data GBranch = GMkBranch GExp GExp 

data GListBranch =
   GConsBranch GBranch GListBranch 
 | GNilBranch 



instance Gf GTheory where gf (GMkTheory x1) = appc "MkTheory" [gf x1]

instance Gf GDefinition where gf (GMkDefinition x1) = appc "MkDefinition" [gf x1]

instance Gf GObject where gf (GMkObject x1) = appc "MkObject" [gf x1]

instance Gf GExp where
 gf (GemptyCase x1) = appc "emptyCase" [gf x1]
 gf GemptyStruct = appc "emptyStruct" []
 gf GemptySig = appc "emptySig" []
 gf GemptySet = appc "emptySet" []
-- gf (GExpCon x1) = appc "ExpCon" [gf x1]
 gf (GExpVar x1) = appc "ExpVar" [gf x1]
 gf (GExpCase x1 x2) = appc "ExpCase" [gf x1, gf x2]
 gf (GExpProj x1 x2) = appc "ExpProj" [gf x1, gf x2]
 gf (GExpLet x1 x2) = appc "ExpLet" [gf x1, gf x2]
 gf (GExpSum x1) = appc "ExpSum" [gf x1]
 gf (GExpStr x1) = appc "ExpStr" [gf x1]
 gf (GExpAbs x1 x2) = appc "ExpAbs" [gf x1, gf x2]
 gf (GExpFun x1 x2) = appc "ExpFun" [gf x1, gf x2]
 gf (GExpFunIndep x1 x2) = appc "ExpFunIndep" [gf x1, gf x2]
 gf (GExpSig x1) = appc "ExpSig" [gf x1]
 gf (GExpMeta x1) = Meta (MetaSymb (zIdent "Exp", x1)) --- not machine-gen.
 gf (GExpCons x1) = appc "ExpCons" [gf x1]
 gf (GExpApp x1 x2) = appc "ExpApp" [gf x1, gf x2]
 gf (GExpTyped x1 x2) = appc "ExpTyped" [gf x1, gf x2]
 gf (GExpProofOf x1 x2) = appc "ExpProofOf" [gf x1, gf x2]
 gf (GExpHypothesis x1 x2) = appc "ExpHypothesis" [gf x1, gf x2]

instance Gf GListExp where
 gf (GConsExp x1 x2) = appc "ConsExp" [gf x1, gf x2]
 gf GNilExp = appc "NilExp" []

instance Gf GVar where gf (GMkVar x1) = appc "MkVar" [gf x1]

instance Gf GListVar where
 gf (GConsVar x1 x2) = appc "ConsVar" [gf x1, gf x2]
 gf (GOneVar x1) = appc "OneVar" [gf x1]

instance Gf GDef where
 gf (GDefComment x1) = appc "DefComment" [gf x1]
 gf (GDefOpen x1 x2) = appc "DefOpen" [gf x1, gf x2]
 gf (GDefPackage x1 x2 x3) = appc "DefPackage" [gf x1, gf x2, gf x3]
 gf (GDefBind x1 x2) = appc "DefBind" [gf x1, gf x2]
 gf (GDefAxiom x1 x2 x3) = appc "DefAxiom" [gf x1, gf x2, gf x3]
 gf (GDefRec x1 x2 x3 x4 x5) = appc "DefRec" [gf x1, gf x2, gf x3, gf x4, gf x5]
 gf (GDefData x1 x2 x3 x4) = appc "DefData" [gf x1, gf x2, gf x3, gf x4]
 gf (GDefValData x1 x2 x3 x4) = appc "DefValData" [gf x1, gf x2, gf x3, gf x4]
 gf (GDefVal x1 x2 x3 x4) = appc "DefVal" [gf x1, gf x2, gf x3, gf x4]

instance Gf GListDef where
 gf (GConsDef x1 x2) = appc "ConsDef" [gf x1, gf x2]
 gf GNilDef = appc "NilDef" []

instance Gf GDecl where gf (GMkDecl x1 x2) = appc "MkDecl" [gf x1, gf x2]

instance Gf GListDecl where
 gf (GConsDecl x1 x2) = appc "ConsDecl" [gf x1, gf x2]
 gf GNilDecl = appc "NilDecl" []

instance Gf GConstr where gf (GMkConstr x1 x2) = appc "MkConstr" [gf x1, gf x2]

instance Gf GListConstr where
 gf (GConsConstr x1 x2) = appc "ConsConstr" [gf x1, gf x2]
 gf GNilConstr = appc "NilConstr" []

instance Gf GBranch where gf (GMkBranch x1 x2) = appc "MkBranch" [gf x1, gf x2]

instance Gf GListBranch where
 gf (GConsBranch x1 x2) = appc "ConsBranch" [gf x1, gf x2]
 gf GNilBranch = appc "NilBranch" []



instance Fg GTheory where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkTheory",_)),[x1]) -> GMkTheory (fg x1)
    _ -> error ("no Theory " ++ prt t)

instance Fg GDefinition where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkDefinition",_)),[x1]) -> GMkDefinition (fg x1)
    _ -> error ("no Definition " ++ prt t)

instance Fg GObject where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkObject",_)),[x1]) -> GMkObject (fg x1)
    _ -> error ("no Object " ++ prt t)

instance Fg GExp where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("emptyCase",_)),[x1]) -> GemptyCase (fg x1)
    Ok ([], Cons (Ident ("emptyStruct",_)),[]) -> GemptyStruct 
    Ok ([], Cons (Ident ("emptySig",_)),[]) -> GemptySig 
    Ok ([], Cons (Ident ("emptySet",_)),[]) -> GemptySet 
--    Ok ([], Cons (Ident ("ExpCon",_)),[x1]) -> GExpCon (fg x1)
    Ok ([], Cons (Ident ("ExpVar",_)),[x1]) -> GExpVar (fg x1)
    Ok ([], Cons (Ident ("ExpCase",_)),[x1,x2]) -> GExpCase (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpProj",_)),[x1,x2]) -> GExpProj (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpLet",_)),[x1,x2]) -> GExpLet (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpSum",_)),[x1]) -> GExpSum (fg x1)
    Ok ([], Cons (Ident ("ExpStr",_)),[x1]) -> GExpStr (fg x1)
    Ok ([], Cons (Ident ("ExpAbs",_)),[x1,x2]) -> GExpAbs (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpFun",_)),[x1,x2]) -> GExpFun (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpFunIndep",_)),[x1,x2]) -> GExpFunIndep (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpSig",_)),[x1]) -> GExpSig (fg x1)
    Ok ([], Cons (Ident ("ExpCons",_)),[x1]) -> GExpCons (fg x1)
    Ok ([], Cons (Ident ("ExpApp",_)),[x1,x2]) -> GExpApp (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpTyped",_)),[x1,x2]) -> GExpTyped (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpProofOf",_)),[x1,x2]) -> GExpProofOf (fg x1) (fg x2)
    Ok ([], Cons (Ident ("ExpHypothesis",_)),[x1,x2]) -> GExpHypothesis (fg x1) (fg x2)
    Ok ([], Meta (MetaSymb (_,int)),_) -> GExpMeta int --- not machine
    _ -> error ("no Exp " ++ prt t)

instance Fg GListExp where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("ConsExp",_)),[x1,x2]) -> GConsExp (fg x1) (fg x2)
    Ok ([], Cons (Ident ("NilExp",_)),[]) -> GNilExp 
    _ -> error ("no ListExp " ++ prt t)

instance Fg GVar where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkVar",_)),[x1]) -> GMkVar (fg x1)
    _ -> error ("no Var " ++ prt t)

instance Fg GListVar where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("ConsVar",_)),[x1,x2]) -> GConsVar (fg x1) (fg x2)
    Ok ([], Cons (Ident ("OneVar",_)),[x1]) -> GOneVar (fg x1)
    _ -> error ("no ListVar " ++ prt t)

instance Fg GDef where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("DefComment",_)),[x1]) -> GDefComment (fg x1)
    Ok ([], Cons (Ident ("DefOpen",_)),[x1,x2]) -> GDefOpen (fg x1) (fg x2)
    Ok ([], Cons (Ident ("DefPackage",_)),[x1,x2,x3]) -> GDefPackage (fg x1) (fg x2) (fg x3)
    Ok ([], Cons (Ident ("DefBind",_)),[x1,x2]) -> GDefBind (fg x1) (fg x2)
    Ok ([], Cons (Ident ("DefAxiom",_)),[x1,x2,x3]) -> GDefAxiom (fg x1) (fg x2) (fg x3)
    Ok ([], Cons (Ident ("DefRec",_)),[x1,x2,x3,x4,x5]) -> GDefRec (fg x1) (fg x2) (fg x3) (fg x4) (fg x5)
    Ok ([], Cons (Ident ("DefData",_)),[x1,x2,x3,x4]) -> GDefData (fg x1) (fg x2) (fg x3) (fg x4)
    Ok ([], Cons (Ident ("DefValData",_)),[x1,x2,x3,x4]) -> GDefValData (fg x1) (fg x2) (fg x3) (fg x4)
    Ok ([], Cons (Ident ("DefVal",_)),[x1,x2,x3,x4]) -> GDefVal (fg x1) (fg x2) (fg x3) (fg x4)
    _ -> error ("no Def " ++ prt t)

instance Fg GListDef where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("ConsDef",_)),[x1,x2]) -> GConsDef (fg x1) (fg x2)
    Ok ([], Cons (Ident ("NilDef",_)),[]) -> GNilDef 
    _ -> error ("no ListDef " ++ prt t)

instance Fg GDecl where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkDecl",_)),[x1,x2]) -> GMkDecl (fg x1) (fg x2)
    _ -> error ("no Decl " ++ prt t)

instance Fg GListDecl where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("ConsDecl",_)),[x1,x2]) -> GConsDecl (fg x1) (fg x2)
    Ok ([], Cons (Ident ("NilDecl",_)),[]) -> GNilDecl 
    _ -> error ("no ListDecl " ++ prt t)

instance Fg GConstr where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkConstr",_)),[x1,x2]) -> GMkConstr (fg x1) (fg x2)
    _ -> error ("no Constr " ++ prt t)

instance Fg GListConstr where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("ConsConstr",_)),[x1,x2]) -> GConsConstr (fg x1) (fg x2)
    Ok ([], Cons (Ident ("NilConstr",_)),[]) -> GNilConstr 
    _ -> error ("no ListConstr " ++ prt t)

instance Fg GBranch where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("MkBranch",_)),[x1,x2]) -> GMkBranch (fg x1) (fg x2)
    _ -> error ("no Branch " ++ prt t)

instance Fg GListBranch where
 fg t =
  case termForm t of
    Ok ([], Cons (Ident ("ConsBranch",_)),[x1,x2]) -> GConsBranch (fg x1) (fg x2)
    Ok ([], Cons (Ident ("NilBranch",_)),[]) -> GNilBranch 
    _ -> error ("no ListBranch " ++ prt t)

