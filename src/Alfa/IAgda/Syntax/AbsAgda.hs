module AbsAgda where

-- Haskell module generated by the BNF converter

newtype Kind = Kind String deriving (Eq,Ord,Show)
newtype Infix = Infix String deriving (Eq,Ord,Show)
newtype PIdent = PIdent ((Int,Int),String) deriving (Eq,Ord,Show)
data Module =
   Module [Decl]
  deriving (Eq,Ord,Show)

data Decl =
   DDef [DefAttr] Def
 | DImp Import
  deriving (Eq,Ord,Show)

data Def =
   Value AIdent [VarDecl] Exp Exp
 | Binding AIdent Exp
 | Package AIdent [Typing] PackageBody
 | Open Exp [OpenArg]
 | Data AIdent [Typing] [Constructor]
 | Type AIdent [Typing] Exp
 | Axiom AIdent [Typing] Exp
 | Mutual [Def]
  deriving (Eq,Ord,Show)

data Exp =
   EVar AIdent
 | ECon AIdent
 | ESet
 | EType
 | EKind Kind
 | EMeta
 | EString String
 | EChar Char
 | EInt Integer
 | EDouble Double
 | EProj Exp AIdent
 | EApp Exp Exp
 | EInfix Exp Infix Exp
 | ESig [FieldDecl]
 | EStr [Binding]
 | ESum [Constructor]
 | EIndSum [IConstructor]
 | EPi VarDecl Exp
 | EFun Exp Exp
 | EAbs VarDecl Exp
 | ELet [Decl] Exp
 | ECase Exp [Branch]
 | EOpen Exp [OpenArg] Exp
 | EConst AIdent
 | EMetaN Integer
  deriving (Eq,Ord,Show)

data Typing =
   TDecl VarDecl
 | TExp Exp
  deriving (Eq,Ord,Show)

data VarDecl =
   VDecl [BIdent] Exp
  deriving (Eq,Ord,Show)

data FieldDecl =
   FDecl AIdent Exp
  deriving (Eq,Ord,Show)

data Branch =
   BranchCon AIdent [AIdent] Exp
 | BranchInf AIdent Infix AIdent Exp
 | BranchVar AIdent Exp
  deriving (Eq,Ord,Show)

data Constructor =
   Cnstr AIdent [Typing]
  deriving (Eq,Ord,Show)

data IConstructor =
   ICnstr AIdent [Typing] [Exp]
  deriving (Eq,Ord,Show)

data Binding =
   Bind AIdent Exp
  deriving (Eq,Ord,Show)

data PackageBody =
   PackageDef [Decl]
 | PackageInst Exp
  deriving (Eq,Ord,Show)

data OpenArg =
   OpenArgSimple [DefAttr] AIdent
 | OpenArgTyped [DefAttr] AIdent Exp
 | OpenArgAs [DefAttr] AIdent AIdent
 | OpenArgFull [DefAttr] AIdent Exp AIdent
  deriving (Eq,Ord,Show)

data DefAttr =
   Private
 | Public
 | Abstract
 | Concrete
  deriving (Eq,Ord,Show)

data Import =
   Import String
  deriving (Eq,Ord,Show)

data AIdent =
   I Infix
 | F PIdent
  deriving (Eq,Ord,Show)

data BIdent =
   C AIdent
  deriving (Eq,Ord,Show)

