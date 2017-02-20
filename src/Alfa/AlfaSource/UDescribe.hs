module UDescribe where
import UAbstract

-- A function that gives a brief textual description of what a piece of
-- abstract syntax is.

class Describe s where describe :: s -> String

instance Describe Exp where
  describe e =
    case e of
      EMeta metavar             -> "meta variable"
      ESort sort                -> "sort"
      EVar var                  -> "variable"
      EApp exp1 exp2            -> "application"
      ECon con                  -> "constructor"
      EAbs typing exp           -> "abstraction"
      ELet decls exp            -> "local definition"
      ECase exp branches        -> "case expression"

      EPi typing exp            -> "function type"
      ESum constructors         -> "data type"

      EProj exp label           -> "projection"
      EOpen exp1 openargs exp2  -> "multiple projection"
      ESig sig                  -> "signature"
      EStr str                  -> "structure"

      EAnnot expannot exp       -> "annotation"
      EProofOf exp1 exp2        -> "nd-style proof"
      ETyped exp1 exp2          -> "typed expression"
      EChar char                -> "character literal"
      EString string            -> "string literal"
      EInt int                  -> "int literal"
      ERat rat                  -> "rational literal"
#ifdef IAGDA
      EEmptyIndSum              -> "idata{}" -- just to be able to test if idata
					      -- is a possible refinement
#endif

instance Describe DefB where
  describe d =
    case d of
      Value   (var,(context,exp1,exp2))     -> "definition with type signature"
      Binding (Var v,e)                   -> "definition without type signature"
      Package (Var v,(context,packagebody)) -> "package declaration"
      Open    (exp,openargs)                -> "open declaration"
      Data    (var,(context,constructors))  -> "data definition"
      Type    (var,(context,exp))           -> "type definition"
      Axiom   (var,(context,exp))           -> "postulate"
      CommentDef string                     -> "comment"


-- + more instances...
