{--}
module ParsOps(ParsOps2..,lit,Error(..)) where
import ParsOps2 renaming (lit to tok,lits to toks)

-- An error message contains a descriptive message and the remaining input.
type Error a = (String, [a])

lit f = token `bind` \ t ->
        case f t of
	  Just y  -> unit y
	  Nothing -> fail "unexpected token" -- push t back on remaining input.

{-}
module ParsOps(OldParsOps..) where
import OldParsOps
-}
