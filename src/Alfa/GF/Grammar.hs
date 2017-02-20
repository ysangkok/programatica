module Grammar where
import Tokens

-- AR 23/1/2000 -- 1/4

newtype Grammar a = Grammar (Abstract,Concrete a)  deriving (Read, Show)
newtype Abstract = Abstract [Def]  deriving (Read, Show)
newtype Concrete a = Concrete [LDef a]  deriving (Read, Show)
type Trm = Term ()
type Cont = Context ()
type Type = Term ()
type Cat = Ident
type Fun = Ident
type LType a = Term a
type LTerm a = Term a
type Symb = Ident

data Def =                  -- judgements in abstract syntax
   DefCat Cat Cont 
 | DefFun Fun Type
 | DefType Ident Type 
 | DefDef Patt Trm 
 | DefData Ident [Ident] 
  deriving (Read, Show)

data LDef a =               -- judgements in concrete syntax
   DefParam Ident [Param a]
 | DefOper Ident (LTerm a) (Term a)
 | DefLType Ident (LTerm a) 
 | DefLintype Cat (LTerm a)
 | DefLin Fun [Ident] (Term a) 
 | DefDefault Cat [Ident] (Term a) 
 | DefVar Ident [Cat]
 | DefTokenizer Ident
  deriving (Read, Show)

data Term a =
   Var Ident
 | Cons Ident
 | Literal Ident String
 | App (Term a) (Term a) 
 | Abs Ident (Term a)
 | Meta MetaSymb
 | Prod Ident (Term a) (Term a)
 | Typed (Term a) (Term a)
 | Closure (Context a) (Term a) -- for type checking
 | TypeType
 | Record [Assign a]       -- below this only for concrete syntax
 | RecType [Labelling a] 
 | UpdRecord (Term a) (Assign a)
 | UpdRecType (Term a) (Labelling a)
 | Project (Term a) Label
 | Cases [Case a]
 | Select (Term a) [Term a]
 | Table [Term a] (Term a)
 | Let [LocalDef a] (Term a) 
 | Tok a
 | Concat (Term a) (Term a) 
 | TypeStr
 | LiT (Term a)       -- lin type corresponding to a type
 | ArgVar (Ident,(Int,Int)) -- to generate parsers: (cat, (position, bindings))
 | Glue (Term a) (Term a)   -- below this for compatibility with old GF
 | Alts Direct (Altern a) 
 | Strs [Term a]
 | TypeStrs
  deriving (Read, Show, Eq)

data Patt =
   PattCons Ident [Patt]
 | PattRec  [(Label,Patt)]
 | PattVar  Ident 
  deriving (Read, Show, Eq)

data Direct = DPrefix | DPostfix deriving (Read, Show, Eq)

newtype Ident = Ident (String,(Int,Int))  deriving (Read, Show, Eq, Ord)
newtype Label = Label (String,Int)  deriving (Read, Show, Eq, Ord) -- record label
newtype MetaSymb = MetaSymb (Ident,MetaIdent) deriving (Read, Show, Eq)
type MetaIdent = Int

type Decl a = (Ident,Term a) 
type Context a = [Decl a]

type Labelling a = (Label, Term a) 
type Assign a = (Label, Term a) 
type Case a = ([Patt], Term a) 
type LocalDef a = (Ident, Term a, Term a) 

type Param a = (Ident, Context a) 
type Altern a = (Term a, [(Term a,Term a)]) 

type Substitution a =  [(Ident, LTerm a)]

-- when using labelled records as linearizations, hard-wire certain label names

linLabel, varLabel, inhLabel, strLabel :: Int -> Label 
linLabel i = Label ("s",i)
varLabel i = Label ("v",i)
inhLabel i = Label ("inh",i)
strLabel i = Label ("str",i)

-- empty grammar

emptyGrammar :: Grammar a
emptyGrammar = Grammar (emptyAbstract,emptyConcrete)
emptyAbstract = Abstract []
emptyConcrete = Concrete []

-- the class Token a is usually needed for Concrete a
