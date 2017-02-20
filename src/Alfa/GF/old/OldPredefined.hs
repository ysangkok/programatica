module OldPredefined where

import Operations
import OldGF
import Parsers

-- Copyright (c) Aarne Ranta 1999, under GNU General Public License (see GPL)
-- predefined categories and operations for GF. Aarne Ranta 6/2/1999 -- 21/9

type LinRule    = String -> [[String]]
type ParseRule  = [Parser String String]
type PredefType = ([Cat],Cat)            --- cannot ref. to other than predef !
type PredefDef  = OIDent -> ([OIDent] -> Term)

-- to add predefined categories or terms, it is enough to add items to one
-- or more of the following three lists

-- 7/2 we have : Int, String, Bool ; +, *, &, |, ~

predefCats :: [(OIDent,CatDef)]
predefCats = [(cat,([],[],[],1)) | cat <- catIds] where
 catIds = 
  [
   "Int",
   "String",
   "Bool"
  -- ADD YOUR OWN CATEGORY IDENTIFIERS HERE
  ]

predefRules :: [(OIDent, (PredefType, LinRule, ParseRule))]
predefRules = 
 [
  ("Int",
   (([], Cat "Int"),
    mkStringList,
    [satisfy (all (`elem` ['0'..'9']))])
  ),
  ("String",
   (([], Cat "String"),
    mkStringList,
    [longestOfSome (satisfy (/="}")) *** foldl1 (+++)])
  ),
  ("Bool",
   (([], Cat "Bool"),
    mkStringList,
    [literal "True" ||| literal "False"])
  ),
  ("arith",
   (([Cat "Int", Cat "Int"],Cat "Int"),   
    mkInfix,
    [succeed "", literal "+" ||| literal "*", succeed ""])
   ),
  ("compar",
   (([Cat "Int", Cat "Int"],Cat "Bool"),   
    mkInfix,
    [succeed "", literal "=" ||| literal "<" ||| literal ">", succeed ""])
   ),
  ("connect",
   (([Cat "Bool", Cat "Bool"],Cat "Bool"),   
    mkInfix,
    [succeed "", literal "&" ||| literal "|", succeed ""])
   ),
  ("negat",
   (([Cat "Bool"],Cat "Bool"),   
    mkPrefix,
    [literal "~", succeed ""])
   )
  -- ADD YOUR OWN RULES HERE
 ]

predefDefs :: [(OIDent, PredefDef)]
predefDefs = 
 [
  ("arith",
   (\f -> case f of
            "+" -> mkPredefInfixDef "Int" rdInt (+) rdInt
            "*" -> mkPredefInfixDef "Int" rdInt (*) rdInt
   )
  ),
  ("compar",
   (\f -> case f of
            "=" -> mkPredefInfixDef "Bool" rdInt (==) rdInt
            "<" -> mkPredefInfixDef "Bool" rdInt (<)  rdInt
            ">" -> mkPredefInfixDef "Bool" rdInt (>)  rdInt
   )
  ),
  ("connect",
   (\f -> case f of
            "&" -> mkPredefInfixDef "Bool" rdBool (&&)  rdBool
            "|" -> mkPredefInfixDef "Bool" rdBool (||)  rdBool
   )
  ),
  ("negat",
   (\f -> case f of
            "~" -> mkPredefPrefixDef "Bool" rdBool not
   )
  )
  -- ADD YOUR OWN DEFINITIONS HERE
 ]

--- ex. (compar{<}(Int{5},arith{*}(Int{2},Int{3})) : Int)  {{5} < {{2} * {3}}}

rdInt :: String -> Int
rdInt = read

rdBool :: String -> Bool
rdBool = read

mkPredefInfixDef :: (Show c) =>
  OIDent -> (String -> a) -> (a -> b -> c) -> (String -> b) -> [OIDent] -> Term
mkPredefInfixDef id read1 op read2 xx =
  Predef (id, show (op (read1 (xx!!0)) (read2 (xx!!1))))

mkPredefPrefixDef :: (Show b) =>
  OIDent -> (String -> a) -> (a -> b) -> [OIDent] -> Term
mkPredefPrefixDef id read1 op xx =
  Predef (id, show (op (read1 (xx!!0))))

mkStringList :: String -> [[String]]
mkStringList s = [words s]

mkInfix :: String -> [[String]]
mkInfix s = [[],[s],[]]

mkPrefix :: String -> [[String]]
mkPrefix s = [[s],[]]

