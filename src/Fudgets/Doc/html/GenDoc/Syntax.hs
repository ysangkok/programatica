module Syntax where

-- Abstract syntax of HBC Haskell 1.3 interface files

data Interface = Interface ConId [TopDecl]
  deriving (Show)

data TopDecl				  -- Top level declarations
  = Data (Con VarId) (Maybe Constructors) -- data decl (Nothing = abstract type)
  | Type (Con VarId) Type		  -- type synonum
  | Class (Con VarId) [Signature]	  -- class declaration
  | Instance (Ctx VarId) (Con Type)	  -- instance declaration
  | Value Signature			  -- type signature, name :: type
  | Infix Fixity Int Id			  -- fixity declaration
  deriving (Show,Eq)

type Id = String
type ConId = String
type VarId = String

type Constructors = [(Ctx VarId,Constructor)]	-- rhs of data declaration

data Constructor
  = PlainCon (Con Type)
  | LabelledCon ConId [([VarId],Type)]		-- Haskell 1.3 record types
  deriving (Show,Eq)

data Signature = Sig VarId CType deriving (Show,Eq)

data Fixity = L | R | N deriving (Show,Eq)

infixL = Infix L
infixR = Infix R
infixN = Infix N

data Con a					-- Constructor application
  = Con ConId [a]				-- Plain type constructor
  | List a					-- List type, [a]
  | Fun a a                                     -- Function type, a->b
  | Tuple [a]					-- Tuple type, (a,b,c)
  deriving (Show,Eq)

data CType = CT (Ctx VarId) Type deriving (Show,Eq)	-- ctx => type

type Ctx a = [Con a]

data Type					-- Type expressions
  = VarT VarId					-- Type variable
  | ConT (Con Type)				-- Type constructor application
  | AppT VarId [Type]				-- type variables of higher kind
  deriving (Show,Eq)
