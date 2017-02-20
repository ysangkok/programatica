module Parser(interfacesParser) where
import ParsOps
import Syntax
import SyntaxUtils(funT)
--import ListUtil(chopList)
import List(nub)
import Char(isUpper,isLower,isDigit)
import RmComments
import Lex

-- interfacesParser parses a sequence of interface files, so you can
-- concatenate a number of interface files and parse them in one go.
interfacesParser =
   parse (some interfaceP `chk` eof) .
   filter (""/=) .
   hlex .
   rmcomments

-- interfaceP: parse one interface
interfaceP =
  unit Interface `chk` tok "interface"
                 `ap` conIdP
                 `chk` tok "where"
		 `chk` tok "{"
		 `ap` topDeclsP
		 `chk` tok "}"

topDeclsP = unit concat `ap` manySep (tok ";") topDeclP

topDeclP = unit (:[]) `ap` (dataP `orelse`
                            typeDecP `orelse`
			    classP `orelse`
			    instanceP `orelse`
			    valueP `orelse`
			    infixP)
	   `orelse`
	   unit [] `chk` garbageP	-- skip unrecognised stuff

garbageP = some (scan (`notElem` [";","}"]))

classP =
  unit Class `chk` tok "class"
             `apCut` classConP
	     `chk` tok "where"
	     `chk` tok "{"
	     `ap` manySep (tok ";") signatureP
	     `chk` tok "}"
  where
    classConP = unit Con `ap` classIdP `ap` some varP
    classIdP  = conIdP `orelse` kindedConIdP

kindP = unit (foldr1 funT) `ap` someSep (tok "->") aKindP
aKindP = 
  unit (VarT "*") `chk` tok "*" `orelse`
  parenP kindP

valueP = unit Value `ap` signatureP

signatureP = unit Sig `ap` varP `chk` tok "::" `apCut` ctypeP
instanceP  =
  unit Instance `chk` tok "instance"
                `apCut` optCtxP
		`ap` tconP

infixP = unit Infix `ap` infixKwP `apCut` numP `ap` opP
  where
    infixKwP = lit i
    i "infixl" = Just L
    i "infixr" = Just R
    i "infixn" = Just N
    i _ = Nothing

dataP = unit Data `chk` (tok "data" `orelse` tok "newtype") -- !!
                  `apCut` conP varP
                  `ap` maybeP (tok "=" `cap`
		              manySep (tok "|")
				      (unit (,) `ap` optCtxP `ap` constructorP)
			      `chk` maybeP (tok "deriving" `chk` typeP))
		  -- deriving clauses are thrown away and need not be parsed
		  -- properly
  where
    constructorP =
	unit LabelledCon `ap` dataConIdP
			 `chk` tok "{"
			 `ap` someSep commaP lblP
			 `chk` tok "}"
      `orelse`
        unit PlainCon `ap` dataConP
    lblP = unit (,) `ap` someSep commaP varP `chk` tok "::" `ap` typeP

dataConIdP = conIdP `orelse` parenP conOpP
dataConP = conP'' dataConIdP typeP atypeP

typeDecP = unit Type `chk` tok "type" 
                     `apCut` conP varP
		     `chk` tok "="
		     `ap` typeP

ctypeP = unit CT `ap` optCtxP `ap` typeP

optCtxP = optional [] (ctxP `chk` tok "=>")
ctxP = unit (:[]) `ap` conP tvarP
       `orelse`
       parenP (someSep commaP (conP tvarP))

typeP = unit funT `ap` atypeP `chk` tok "->" `ap` typeP
	`orelse`
	unit ConT `ap` tconP
        `orelse`
	unit AppT `ap` tvarP `ap` some atypeP
        `orelse`
	atypeP
	-- Should be left factorized for efficiency !!

atypeP = unit VarT `ap` tvarP
         `orelse`
	 unit ConT `ap` aconP typeP
	 `orelse`
	 parenP typeP


aconP argP =
    unit (flip Con []) `ap` conIdP
    `orelse`
    listP argP
    `orelse`
    tupleP argP
    `orelse`
    parenP (unit (Tuple []))
  where
    listP argP = unit List `chk` tok "[" `ap` argP `chk` tok "]"
    tupleP argP = parenP (unit Tuple `ap` some2Sep commaP argP)

tconP = conP' typeP atypeP
conP argP = conP' argP argP

conP' = conP'' tyconIdP
conP'' conIdP argP aargP =
  unit Fun `ap` aargP `chk` tok "->" `ap` argP
  `orelse`
  unit Con `ap` conIdP `ap` many aargP
  `orelse`
  aconP argP

tvarP = idP' isVar
 where isVar c = c=='?' || isLower c -- existential type variables: ?a
				-- bug: next char should be a lowercase letter

--tvarP = prefixIdP "?" varP'  -- existential type variables: ?a
-- where varP' = idP' isLower

tyconIdP = funTypeIdP `orelse` kindedConIdP `orelse` conIdP
  where
     funTypeIdP = unit "(->)" `chk` parenP (tok "->")

kindedConIdP = parenP (conIdP `chk` tok "::" `chk` kindP)

conIdP = qualifiedP conIdP'

varP = qualifiedP varP' `orelse` parenP (qualifiedP opP)
 where varP' = idP' isLower

qualifiedP p =
    conIdP' `cap` tok "." `cap` p
  `orelse`
    p

--conIdP' = prefixIdP "_" (idP' isUpper)

conIdP' = idP' isCon
  where isCon c = c=='_' || isUpper c
		-- bug: next char should be an uppercase letter

idP' first = lit (\s->case s of
                       c:cs | first c && s `notElem` reserved -> Just s
		       _ -> Nothing)

{-
prefixIdP s p =
  unit (s++) `chk` tok s `ap` p `orelse` p
  -- workaround for lexer limitation: _LibDialogue, ?a
-}

opP = qualifiedP opP'

opP' = lit (\s->case s of
                 c:cs | isOp c -> Just s
		 _ -> Nothing)

conOpP = lit conOp
  where
     conOp s@(':':s2) | all isOp s2 = Just s
     conOp _ = Nothing

numP = unit (read::(String->Int)) `ap` scan (all isDigit)

commaP = tok ","

parenP p = lparP `cap` p `chk` rparP
  where 
    lparP = tok "("
    rparP = tok ")"

some2Sep sepP argP = unit (:) `ap` argP `chk` sepP `ap` someSep sepP argP

reserved = [
  "data","type","module","interface","deriving","where",
  "class","instance"] -- incomlete...

--}

isOp :: Char -> Bool
isOp '~' = True; isOp '=' = True; isOp '*' = True
isOp '%' = True; isOp '/' = True; isOp ':' = True
isOp '+' = True; isOp '@' = True; isOp '.' = True
isOp '>' = True; isOp '&' = True; isOp '$' = True
isOp '|' = True; isOp '-' = True
isOp '!' = True; isOp '<' = True
isOp '^' = True; isOp '#' = True; isOp '?' = True
isOp '\\' = True
isOp _ = False
