module Mixfix where
import List (intersperse)
import Char (ord)

mixfix' :: String -> [ShowS] -> ShowS
mixfix' ('_':cs) (p:ps) = p . mixfix' cs ps
-- mixfix' ('\\':c:cs) ps = showChar c . mixfix' cs ps
mixfix' (c:cs) ps = showChar c . mixfix' cs ps
mixfix' _ _ = id

mixfix :: String -> [ShowS] -> Int -> Int -> ShowS
mixfix cs ps p0 p = showParen (p > p0) (mixfix' cs ps)

showSpace = showChar ' '
showLR l r x = showString l . x . showString r
showQuotes = showLR "\"" "\""
showBraces = showLR "{" "}"
showParens = showLR "(" ")"
showAngles = showLR "\\<langle>" "\\<rangle>"
showSquares = showLR "[" "]"
showHBrackets = showLR "\\<guillemotleft>" "\\<guillemotright>"

showAll xs = foldr (.) id xs
showSep sep xs = showAll (intersperse sep xs)
showCommaSep = showSep (showString ", ")
showSpaceSep = showSep showSpace
showSemiSep = showSep (showString "; ")
showBarSep = showSep (showString " | ")

showTuple xs = showParens (showCommaSep xs)

showIndentLine = showLR "  " "\n"

encode_op :: String -> String
encode_op s = map encode_ch s

encode_ch c = case c of
  '&' -> 'a'
  '|' -> 'b'
  '^' -> 'c'
  '$' -> 'd'
  '=' -> 'e'
  '>' -> 'g'
  '#' -> 'h'
  '.' -> 'i'
  '<' -> 'l'
  '-' -> 'm'
  '!' -> 'n'
  '+' -> 'p'
  '\\' -> 'r'
  '/' -> 's'
  '*' -> 't'
  '%' -> 'v'
  '~' -> 'w'
  '?' -> 'x'
  '@' -> 'y'
  ':' -> 'z'

-- see ghc/compiler/basicTypes/OccName.lhs
