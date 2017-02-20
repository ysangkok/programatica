module UParse where
import AlfParserMonad
import UAbstract
import AlfLex(is_ph,num_of_string)

infixr 1 `symThenP`
infixl 2 `chkSymP`

pair x y = (x,y)

commentsP = manyP commentP

symbolP' s = commentsP `thenP` symbolP s
idenP' = commentsP `thenP` idenP
conP' = commentsP `thenP` conP

p   `chkSymP`  sym = p            `chkP`  symbolP' sym
sym `symThenP` p   = symbolP' sym `thenP` p

----star :: Parse a -> Parse [a]
----plus :: Parse a -> Parse [a]

star f = optionalP [] (plus f)
plus f = unitP (:) `apP` f `apP` optionalP [] plus_lem
  where plus_lem = "," `symThenP` plus f

wrapP l r p = l `symThenP` p `chkSymP` r

inParenP = wrapP "(" ")"
inBracesP = wrapP "{" "}"
--inBracketsP = wrapP "[" "]" -- 
inAngleBracketsP = wrapP "<" ">"

--parse_names :: Parse [Name]
parse_names = star parse_Name

--parse_iden :: Parse String
parse_iden = idenP'

--parse_Name :: Parse Name
parse_Name  = unitP Var `apP` idenP'

--parse_eq :: Parse (Exp,Exp)
parse_eq =
  unitP pair `apP` expP `chkSymP` "=" `apP` expP

--parse_comment :: Parse Comment
parse_comment = inAngleBracketsP (star parse_eq)

--parse_def :: Parse (Name,(Context,Exp,Exp))
parse_def =
    unitP (\n ds u v->DefA ([],Nothing) (Value (n,(ds,u,v))))
    `apP` parse_Name `apP` parse_ctx
    `chkSymP` "=" `apP` expP `apP` parse_dec

parse_ctx = optionalP [] (inParenP (parse_binds parse_dec))

--parse_decl :: Parse Decl
parse_decl = mutrec_decl `orP` simple_decl
  where
    mutrec_decl =
      unitP Decl
      `chkSymP` "decl"
      `apP` inBracesP (plus parse_def)

    simple_decl = unitP (Decl.(:[])) `apP` parse_def

--parse_com :: Parse Com
{-
parse_com =
    unitP com `apP` inBracesP parse_decls
  where
    com = com' . reverse
    com' [] = Unit
    com' (d:ds) = Comp (com' ds) d
-}

parse_decls = star parse_decl

--parse_pat :: Parse ([Name],Exp) 
parse_pat = unitP pair `apP` parse_ns `chkSymP` "->" `apP` expP

--parse_pats :: Parse (Table ([Name],Exp))
parse_pats = parse_binds' conP' parse_pat

---- parse_bind :: Parse a -> Parse (Name,a)
parse_bind' n f = unitP (pair . Var) `apP` n `apP` f
parse_bind = parse_bind' idenP'

--parse_binds :: Parse a -> Parse (Table a)
parse_binds f = star (parse_bind f)
parse_binds' n f = star (parse_bind' n f)

--parse_inst :: Parse Exp
parse_inst = "=" `symThenP` expP

--parse_str :: Parse (Table Exp)
parse_str = parse_binds parse_inst

--parse_dec :: Parse Exp
parse_dec = ":" `symThenP` expP

parse_pi =
  unitP pair
  `apP` optionalP [] (inParenP (parse_binds parse_dec))
  `apP` optionalP [] parse_comment

--parse_sum :: Parse (Table (Context,Comment))
parse_sum = parse_binds' conP' parse_pi

--parse_sig :: Parse Sig
parse_sig = parse_binds parse_dec

appExp (ECon c us) u = ECon c (us ++ [u])
--appExp (ELCon c us) u = ELCon c (us ++ [u])
appExp v u = EApp v u

parse_ns = manyP parse_Name

idenOrPh s =
  if is_ph s 
  then let k = num_of_string (tail s)
       in --if k == 0 
	  --then unitP (EMeta (-1))
	  --else
	  unitP (EMeta k)
  else unitP (appIdent (Var s) [])

{- Concrete syntax of expressions:

exp = exp bexp
    | bexp
    | \ names -> exp
    | let com in exp
    | ( context ) -> exp

bexp = bexp . ident
     | aexp

aexp = name
     | con
     | sort
     | aexp . ident
     | data { constructors }
     | case exp of { branches }
     | sig { context }
     | struct { bindings }
     | theory { com }
     | ?
     | ( exp )

-}

parse_exp = expP -- for backwards compatibility

--expP, bexpP, aexpP :: Parse Exp

expP = 
        unitP uAbsExp `chkSymP` "\\" `apP` parse_ns
		     `chkSymP` "->" `apP` parse_exp
  `orP` unitP ELet `chkSymP` "let" `apP` parse_decls
	           `chkSymP` "in"  `apP` parse_exp
  `orP` unitP piExp `apP` inParenP (parse_binds parse_dec)
                    `chkSymP` "->"
	            `apP` parse_exp
  `orP` unitP app `apP` someP bexpP
  where
    app = foldl1 appExp

bexpP = unitP proj `apP` aexpP `apP` manyP ("." `symThenP` parse_Name)
  where
    proj e sels = foldl EProj e sels

aexpP =
        ((idenP' `orP` conP') `bindP` idenOrPh)
  `orP` unitP (ESort (Sort "Set")) `chkSymP` "Set"
  `orP` unitP (ESort (Sort "Type")) `chkSymP` "Type"
  `orP` unitP (ESort (Sort "Theory")) `chkSymP` "Theory"
  `orP` unitP ECase `chkSymP` "case" `apP` parse_exp
	            `chkSymP` "of"
	            `apP` inBracesP (unitP (map Branch) `apP` parse_pats)
  `orP` unitP ESum `chkSymP` "data" `apP` inBracesP parse_sum
  `orP` unitP ESig `chkSymP` "sig" `apP` inBracesP parse_sig
  --`orP` unitP ECom `chkSymP` "theory" `apP` parse_com
  --`orP` unitP EStr `chkSymP` "struct" `apP` inBracesP parse_str
  `orP` inParenP expP
