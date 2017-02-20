--This module is the interface to the parser. All conversion between text and
--abstract syntax should go via this module. (TextConv would be a better name.)
module FileConv where

-- Pick one of the supported text representations:
--import qualified UFileConv as FC
import qualified AgdaFileConv as FC

--import AlfModules(Module,Decl)
import UAbstract(Exp,Context,Var,Con,Label,Module,Decl)
import AlfSyntax --(IsSyntax)
--- Printing

printModule :: Module -> String
printModule = FC.printModule

prSyntax, prSyntax1line :: IsSyntax a => a -> String
prSyntax = FC.prSyntax
prSyntax1line = FC.prSyntax1line

prVar = FC.prVar
prCon = FC.prCon

--- Parsing

parseModule :: String -> String -> Either String Module
parseModule = FC.parseModule

type ErrorPos = (String,(Int,Int)) -- filename, line, column
type Error = (ErrorPos,String)

type Parser a = FC.Parser a

parse :: Parser a -> String -> Either String a
parse' :: Parser a -> String -> Either Error a

declsP :: Parser [Decl]
expP :: Parser Exp
ctxP :: Parser Context
varP :: Parser Var
conP :: Parser Con
labelP :: Parser Label

mapP :: (a->b) -> Parser a -> Parser b
someP,manyP,commaSeqP :: Parser a -> Parser [a]

---

parse = FC.parse
parse' = FC.parse'

declsP = FC.declsP
expP = FC.expP
ctxP = FC.ctxP
varP = FC.varP
conP = FC.conP
labelP = FC.labelP

mapP = FC.mapP
manyP = FC.manyP
someP = FC.someP
commaSeqP = FC.commaSeqP
