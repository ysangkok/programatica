module PluginPesca where

import PescaAlfa (tryProveAlfa, eMeta, parseFormula) ---- SC
import qualified AlfaPluginKit as A
import Char (isDigit)

type State = ()

tryProveExp :: State -> A.Exp -> String -> A.Parsed [A.Exp]
tryProveExp _ typ d 
  | not (null d) && all isDigit d = Right [tryProveAlfa (read d) eMeta typ]
  | otherwise = Right [tryProveAlfa 8 eMeta typ]

parseProposition :: State -> String -> A.Parsed [A.Exp]
parseProposition _ s = case parseFormula s of
  (f,[]):_ -> Right [f]
  _ -> Left ((0,0),"no parse")
