import ParsOps3

main = interact (unlines . map (show . parse (expr {-`chk` eof-})) . lines)

expr =  readInt `mapP` some (oneOf ['0'..'9'])
	 `orelse`
	unit (+) `chk` tok '(' `ap` expr `chk` tok '+' `ap` expr `chk` tok ')'

readInt = read :: (String->Int)
