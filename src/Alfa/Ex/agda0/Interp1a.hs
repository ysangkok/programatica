-- The Interpreter Example in Haskell
module Interp1a where

-- Identifiers:
newtype Id = Id String
           deriving (Eq)

-- Abstract syntax of expressions:
data Exp
  = NCon Int
  | BCon Bool
  | Ap Exp Exp
  | Lam Id Typ Exp -- \ x::t -> e
  | Var Id

-- A union type for values:
data Value
  = NVal Int
  | BVal Bool
  | FVal (Value->Value)
  | Error

-- Value environments (store):
type VEnv = Id -> Value

extendV :: Id -> Value -> VEnv -> VEnv
extendV i v venv = \ i' -> if i'==i then v else venv i'

interp :: Exp -> VEnv -> Value
interp e venv =
  case e of
    NCon n -> NVal n
    BCon b -> BVal b
    Ap e1 e2 -> case interp e1 venv of
                  FVal f -> f (interp e2 venv)
		  _ -> Error
    Lam i _ e -> FVal (\ v -> interp e (extendV i v venv))
    Var i -> venv i

--------------------------------------------------------------------------------
venv0 i = Error
x = Id "x"

venv1 = extendV (Id "+") plus venv0
  where
    plus = FVal (\v1->FVal (\v2->case (v1,v2) of
			           (NVal n1,NVal n2) -> NVal (n1+n2)
			           _ -> Error))

--------------------------------------------------------------------------------

-- Abstract syntax of type expressions:
data Typ = Bool | Nat | Fun Typ Typ
         deriving (Eq)

-- Type environment
type TEnv = Id -> Typ

-- Extending a type environment:
extend :: Id -> Typ -> TEnv -> TEnv
extend i t tenv = \ i' -> if i'==i then t else tenv i'

typ :: Exp -> TEnv -> Maybe Typ
typ e tenv =
  case e of
    NCon _ -> return Nat
    BCon _ -> return Bool
    Ap e1 e2 -> do Fun targ tres <- typ e1 tenv
		   t2 <- typ e2 tenv
		   t2 =:= targ
		   return tres
    Lam i t e -> do te <- typ e (extend i t tenv)
		    return (Fun t te)
    Var i -> return (tenv i)

t1 =:= t2 = if t1==t2
            then return t1
	    else fail "type mismatsh"

