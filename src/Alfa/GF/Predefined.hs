module Predefined where

import Operations
import Tokens
import Grammar
import SymbolTable
import Macros
import Parsers
import CF
import GFCFIdents
import Char

-- inbuilt GF definitions. AR 11/2/2000 -- 21/3

predefGrammarST :: GrammarST a
predefGrammarST = (predefAbstractST, predefConcreteST)

predefAbstractST :: AbstractST
predefAbstractST = (buildTree predefInfos, 0)

predefConcreteST :: ConcreteST a
predefConcreteST = (buildTree predefLInfos, 0)

predefCF :: Token a => GFCF a
predefCF = ([], literalCFPredef)

-- to add a new one, edit the following four definitions.

predefInfos :: [(Ident,IdentInfo)]
predefInfos =
 [
  (identString, IdentCat []),
  (identInt,    IdentCat [])
-- add your own predef cat here
 ]

predefLInfos :: [(Ident,IdentLInfo a)]
predefLInfos =
 [
  (identString, IdentLintype linTypeStr),
  (identInt,    IdentLintype linTypeStr)
-- add your own predef lintype here
 ]

-- objects of these types are literals
lookupLiteral :: String -> Err (Type, Trm)
lookupLiteral s = case s of
  _ | lg > 3 && take 2 s == beginLitString && drop (lg-2) s == endLitString
                              -> return (typeString, Literal identString s) 
    | lg > 0 && all isDigit s -> return (typeInt, Literal identInt s) 
-- add your own literal format here
    | otherwise -> Bad ("unknown literal" +++ s)
 where lg = length s

-- finding literals in grammar files and term input
pLiteral :: Parser Char (Ident,String)
pLiteral = 
  lits beginLitString ... longestOfMany pAlphanum ... lits endLitString 
     *** (\ (x,(y,z)) -> (identString, x ++ y ++ z))
 |||
  longestOfSome (satisfy isDigit)  
     *** (\n -> (identInt,n))
-- add your own literal here
-- this can be reduced to the former if we use Parser String x.

exampleLiterals :: [(Ident,[String])] -- used e.g. in random generation
exampleLiterals =
  [(identString, ["``foo''", "``bar''", "``x''","``NN''"]),
   (identInt,    ["0", "5", "18", "2001"])
  ]

beginLitString ="``"
endLitString ="''"


typeString, typeInt :: Type
typeString = Cons identString
typeInt = Cons identInt

identString, identInt :: Ident
identString = Ident ("String",(-1,0))
identInt = Ident ("Int",(-2,0))

isLiteral :: String -> Bool
isLiteral s = case lookupLiteral s of
  Ok _ -> True
  _ -> False

linearizeLiteral :: Token a => String -> LTerm a
linearizeLiteral s = linAsStr (readTok s)

-- finding literals in input token list (component in a CF grammar)
literalCFPredef :: Token a => CFPredef a GFCFCat GFCFFun
literalCFPredef t = let t' = showTok t in case lookupLiteral t' of
  Ok (Cons c, lit) -> [(mkGFCFCat c, mkGFFun lit)]
  _ -> []

isLiteralTok :: Token a => a -> Bool
isLiteralTok = isLiteral . showTok

mkLitString s = Literal identString (beginLitString ++ s ++ endLitString)

