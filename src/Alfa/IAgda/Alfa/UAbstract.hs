module Alfa.UAbstract(module Alfa.UAbstract,MetaVar(..),was,asTextBy) where
import MetaVars
import Alfa.UAnnots

ePreMetaVar = EMeta preMetaVar

ePos = EAnnot . EPos . N
eComment = EAnnot . EComment
eCon0 = ECon
eCon2 = app2 . ECon
eCon = app . ECon

newtype Sort = Sort String deriving (Eq,Show,Read)
newtype Var = Var String deriving (Eq,Show,Read)
newtype Con = Con String deriving (Eq,Ord,Show,Read)
newtype Label = Label String deriving (Eq,Show,Read)
type Vars = [Var]

--type MetaVar = Int -- moved to UMetaVar


type ExpAnnot = UAnnot Exp Bindings

-- When changing this, the following modules probably need to be changed too:
-- ConvFromAgda, ConvToAgdaInEnv, DrawAlf, USubstitute, SubstMeta, ...?
data Exp
    = EMeta MetaVar           -- place holder
    | ESort Sort              -- sort, e.g. Set or Type
    | EVar Var                -- variable
    | EApp Exp Exp            -- application
--  | ECon Con Exps           -- constructor application, C e1 .. en
    | ECon Con                -- constructor
--  | ELCon Con Exps          -- lazy constructor application
    | EAbs Typing Exp         -- abstraction, \ x::t -> e
    | ELet Decls Exp          -- local declaration, let decls in e
    | ECase Exp Branches      -- case ananalysis, case e of p1->e1 .. p2->e2

--  | EFun Exp Exp            -- independent function type t -> t
    | EPi Typing Exp          -- dependent function type, (x::t) -> t
    | ESum Constructors       -- data type, data { C1 ..., Cn ... }

    | EProj Exp Label         -- projection e1.x
    | EOpen Exp OpenArgs Exp  -- multiple projecton
    | ESig Sig                -- signature, sig { x1:e1 ... xn:en }
    | EStr Str                -- structure, struct { x1=e1 ... xn=en }

    | EAnnot ExpAnnot Exp     -- annotation
    | EProofOf Exp Exp        -- let ndgoal::e1=e2 in ndgoal
    | ETyped Exp Exp          -- let it::e2=e1 in it
--  | ECom Com                -- theory, theory { decls }
--  | EExit
    -- Literals (not present in Agda's abstract syntax)
    | EChar Char              -- Character literal
    | EString String          -- String literal
    | EInt Integer            -- Integer literal
    | ERat Rational           -- Rational literal
#ifdef IAGDA
    | EEmptyIndSum            -- idata {} -- just to be able to test if idata
                                          -- is a possible refinement
#endif
    deriving (Eq,Show,Read)

type Exps = [Exp]
--type Str = Bindings --old
type Str = Decls
newtype Branch = Branch (Con,([Var],Exp)) deriving (Eq,Show,Read)
type Branches = [Branch]
data Typing = Var :- Exp deriving (Eq,Show,Read)
type Typings = [Typing]
--type Sig = [(Label,Exp)] --old
data SigPart = SigField Label Exp | SigDef DefB deriving (Eq,Show,Read)
type Sig = [SigPart]
type Binding = (Var,Exp)
type Bindings = [Binding]
newtype Constructor = Constr (Con,(Context,EqRestr)) deriving (Eq,Show,Read)
type EqRestr = [Exp] -- for the idata construction in Makoto's new Agda
type Constructors = [Constructor]

data Context
  = Ctx (Maybe LayoutDirection) Bindings -- should be Typings!
  deriving (Eq,Show,Read)

ctx = Ctx Nothing
emptyCtx = ctx []

newtype Module = Module Decls deriving (Show,Read)
newtype Import = Import FilePath deriving (Eq,Show,Read)

type Decls = [Decl]
data Decl
  = Comment Comment
  | Decl Correct Defs -- a group of mutually recursive definitions
  | ImportDecl Import
  deriving (Eq,Show,Read)

type Comment = String
type Correct = Bool

decl' = Decl True

type Defs = [Def]
data Def = DefA DefAttrs DefB deriving (Eq,Show,Read)
data DefB
  = Value   (Var,(Context,Exp,Exp))     -- f (x1::t1,...,xn::tn) = e :: t
  | Binding Binding                     -- x = e
  | Package (Var,(Context,PackageBody)) -- package p(...) ...
  | Open    (Exp,OpenArgs) -- !! incomplete -- open e use ...
  | Data    (Var,(Context,Constructors)) -- data D(...) = C1 .. | ... | Cn ..
  | Type    (Var,(Context,Exp))         -- type T (x1::t1,...,xn::tn) = e
  | Axiom   (Var,(Context,Exp))         -- postulate f (x1::t1,...,xn::tn)  :: t
  | CommentDef String
  deriving (Eq,Show,Read)

defA = DefA (noPosition,[],Nothing)

mapDefB f (DefA da defb) = DefA da (f defb)

type DefAttrs = (Position,Props,Maybe DeclDetail)

type Props = [Prop]
data Prop = Private | Public | Abstract | Concrete deriving (Eq,Show,Read,Enum)
allProps = [Private ..]

data PackageBody
  = PackageDef Decls
  | PackageInst Exp
  deriving (Eq,Show,Read)

type OpenArgs =  [OpenArg]
--type OpenArg = (Var,(Maybe Exp,Maybe Var))
data OpenArg
  = OpenArg Props Var (Maybe Exp) (Maybe Var) -- x :: t as y
  deriving (Eq,Show,Read)

arity :: Exp -> Int
arity (EPi _ e) = 1 + arity e
arity _ = 0

--absExp :: [Name] -> Exp -> Exp
absExp [] e = e
absExp (n:ns) e = EAbs n (absExp ns e)

uAbsExp ns = absExp [n:-ePreMetaVar|n<-ns]

namesDecl :: Decl -> [Var]
namesDecl = defsDecl
  where
    --defsDecl :: Decl -> [Def]
    defsDecl (Decl _ d) = concatMap defaNames d
    defsDecl _ = []

defaNames (DefA _ defb) = defNames defb
defNames (Value (n,_)) = [n]
defNames (Binding (n,_)) = [n]
defNames (Package (n,_)) = [n]
defNames (Open (_,oargs)) = concatMap openDefNames oargs
  where
    openDefNames (OpenArg _ n _ optn2) =
       case optn2 of
         Nothing -> [n]
         Just n2 -> [n2]
defNames (Data (n,_)) = [n]
defNames (Type (n,_)) = [n]
defNames (Axiom (n,_)) = [n]
defNames (CommentDef _) = []
    
namesDecls = concatMap namesDecl

namesCtx (Ctx _ bs) = map fst bs

piExp :: [(Var,Exp)] -> Exp -> Exp
piExp [] t = t
piExp ((n,t):nts) t2 = EPi (n:-t) (piExp nts t2)

app2 f e1 e2 = f `EApp` e1 `EApp` e2

app e1 [] = e1
app e1 (e:es) = app (EApp e1 e) es

{- old:
app e es = apps e (reverse es)
  where
    apps :: Exp -> Exps -> Exp
    apps e1 [] = e1
    apps e1 (e:es) = EApp (apps e1 es) e
-}

typings2ctx nts = ctx [(x,t) | x:-t <- nts]
ctx2typings (Ctx _ ctx) = map (uncurry (:-)) ctx
