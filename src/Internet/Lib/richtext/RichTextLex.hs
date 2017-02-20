module RichTextLex where

-- WARNING! This has lately been used only for parsing HTML. It may not
-- work well for richtext any more.

data RichTextLex
  = Chars String
  | FmtCmd String
  | Comment String
  deriving (Eq,Show)

showRtl (Chars s) = s
showRtl (FmtCmd s) = '<':s++">"

showRtls = concatMap showRtl

rtlex = stripgarb . lex
  where lex s =
	    case break ('<'==) s of
	      (s1,[]) -> [Chars s1]
	      (s1,'<':'!':'-':'-':s2) ->
	         case getComment s2 of
	           (c,s3) -> Chars s1:Comment c:lex s3
	      (s1,'<':s2) ->
		 case break ('>'==) s2 of
		   (s21,[]) -> [Chars s1]
		   (s21,'>':s22) -> Chars s1:FmtCmd s21:lex s22
        stripgarb ts =
	  case ts of
	    [] -> []
	    Chars "":ts -> stripgarb ts
	    Chars cs:ts -> pchars cs:stripgarb ts
	    {-
	    FmtCmd x:Chars cs:ts ->
		case dropWhile (=='\n') cs of
		  ""  -> stripgarb (FmtCmd x:ts)
		  cs' -> FmtCmd x:pchars cs':stripgarb ts
	    -}
	    t:ts -> t:stripgarb ts
	pchars = Chars
	--pchars cs = Chars (concatMap printable cs)
	printable c =
	  case c of
	    '\t' -> "        "
	    '\n' -> " "
	    c    -> [c]

getComment s =
  case break (`elem` "<-") s of
    (s1,'-':'-':'>':s2) -> (s1,s2) -- end of comment
    (s1,'-':'-':'!':'>':s2) -> (s1,s2) -- ends comments in some buggy HTML code
    (s1,'<':'!':'-':'-':s2) ->
	case getComment s2 of -- get nested comment
	  (s21,s22) ->
	     case getComment s22 of -- get rest of outer comment
	       (s221,s222) -> (s1++"<!--"++s21++"-->"++s221,s222)
    (s1,c:s2) -> case getComment s2 of
	          (s21,s22) -> (s1++c:s21,s22)
    (s1,"") -> (s1,"") -- EOF inside comment
