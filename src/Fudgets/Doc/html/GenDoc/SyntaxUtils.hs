module SyntaxUtils where
import Syntax
--import Maybe(Maybe)

isType (Data _ _) = True
isType (Type _ _) = True
isType (Class _ _) = True
isType _ = False

isInstance (Instance _ _) = True
isInstance _ = False

defName (Data (Con n _) _) = n
defName (Type (Con n _) _) = n
defName (Class (Con n _) _) = n
defName (Instance _ (Con n _)) = n --hmm
defName (Value (Sig n  _)) = n
defName (Infix _ _ n) = n

funArgs (ConT c) = funArgsC c
funArgs _ = []

funArgsC (Fun a r) = a:funArgs r
funArgsC _ = []

funResult (ConT c) = funResultC c
funResult t = t

funResultC (Fun _ r) = funResult r
funResultC c = ConT c

funT a b = ConT (Fun a b)
