module Tokenize where

import Operations
import Parsers
import Char

-- Copyright (c) Aarne Ranta 1998--99, under GNU General Public License (see GPL)
-- predefined tokenizers for GF. AR 31/8/1998 -- 14/10/1999

-- this is still an awful mess on 23/8/2000 ---

data Tokenizer = Tokenizer String [String] deriving (Read,Show) 

testT f s = (t, untokens min t) 
              where t   = tokens min s
                    min = Tokenizer f []

tokens   :: Tokenizer -> String   -> [String]
untokens :: Tokenizer -> [String] -> String

allTokenizers = words "text code latex freecode mini chars" -- ADD TOKZER NAME

tokens (Tokenizer i k) =
  case i of
   "text"     -> tokensTable tokText k
   "code"     -> tokensTable tokCode k
   "latex"    -> tokensTable tokLatex ("$" : k)
   "freecode" -> (restoreStringBrack k) . (tokensTable tokCode k)
   "freetext" -> (restoreStringBrack k) . (tokensTable tokText k)
   "freemini" -> (restoreStringBrack k) . (tokensTable tokMini [])
   "chars"    -> map (:[]) . filter (/='\n') -- chars ignores newlines
   -- ADD YOUR OWN TOKENIZER HERE
   _          -> tokensTable tokMini []

untokens (Tokenizer i k) =
 case i of
  "text"     -> untokensTable tokText k
  "code"     -> untokensTable tokCode k
  "freecode" -> (untokensTable tokCode k) . (removeStringBrack k)
  "latex"    -> doLatex . (untokensTable tokText k)
  "chars"    -> concat
  -- ADD YOUR OWN TOKENIZER HERE
  _          -> untokensTable tokMini []

type TokenizerTable = ([Char], [Char],     [Char],     [Char])  
                     --junk,   majorpunct, interpunct, special

tokMini  = (" \n\t",     "",    "", ""      )   -- corresp. to words
tokText  = (" \n\t", ".!?", ",:;",  "")
tokLatex = (" \t",   ".!?", ",:;",  "")
tokCode  = (" \n\t", "",    "",    specialChar)

specialChar = "()[]+-,.:;|\\/?!~%#$&*@^\"'={}"

tokensTable :: TokenizerTable -> [String] -> String -> [String]
tokensTable tt@(ju,mp,ip,spc) toks s = filter noJunk (fst (head (pToks s))) where
 pToks =
  parses $
  longestOfMany (
          pLongestMatch toks                                        +||
          longestOfSome (satisfy (not . specChar)) *** maybeMinusc  |||
          satisfy specChar *** (\x -> [x]))
 noJunk (c:[]) = not (elem c ju) 
 noJunk _      = True
 specChar      = (\x -> elem x (ju ++ mp ++ ip ++ spc)) ---pd
 maybeMinusc   = if mp==[] then id else minusc [] []

untokensTable :: TokenizerTable -> [String] -> [String] -> String
untokensTable tt@(s:ju,mp,ip,spc) toks t = untok t where
  untok t = case t of 
              []                 -> ""
              [c]:l | elem c mp  -> c : s : majusc ju (untok l)
              [c]:l | elem c ip  -> c : s : untok l
              [c]:l | elem c spc -> c : spaceIfWord l ++ untok l
              w  :l              -> w ++ spaceIfIdent l ++ untok l
  spaceIfWord l = case l of 
                    []                  -> ""
                    [c]:_ | elem c mis  -> ""
                    _  :_               -> [s]
  spaceIfIdent l = case l of 
                     []                  -> ""
                     [c]:_ | elem c mi   -> ""
                     _  :_               -> [s]
  mis      = mi ++ spc
  mi       = mp ++ ip

majusc :: [Char] -> String -> String
majusc ju s = if (all junk s) then s else j ++ cap w where
 junk c     = elem c ju
 (j,w)      = span junk s
 cap (c:cs) = toUpper c : cs
 cap w      = w

minusc :: [Char] -> [String] -> String -> String
minusc ju toks s = if (all junk s) then s else j ++ treat w ++ t where
 junk c    = elem c ju
 (j,u)     = span junk s
 (w,t)     = span (not . junk) s
 treat v   = if elem v toks then v else ucap v
 ucap (c:cs) = toLower c : cs
 ucap w      = w

doLatex :: String -> String
doLatex []           = []
doLatex ('$':' ':s)  = doLatex ('$':s)
doLatex ('$':s)      = '$' : mathMode s
doLatex (c  :s)      = c   : doLatex s

mathMode :: String -> String
mathMode []          = [] -- should never happen
mathMode (' ':'$':s) = '$' : doLatex s
mathMode ('$':s)     = '$' : doLatex s
mathMode (c  :s)     = c   : mathMode s

composeTokenizers :: Tokenizer -> Tokenizer -> Tokenizer
composeTokenizers (Tokenizer _ ss) (Tokenizer t ss') = Tokenizer t (ss ++ ss')

sortTokenizer :: Tokenizer -> Tokenizer
sortTokenizer (Tokenizer t ss) = Tokenizer t (sortByLongest ss)

restoreStringBrack, removeStringBrack :: [String] -> [String] -> [String]
restoreStringBrack tokens tt = 
 case tt of
   t :tt' | isFreeIdent tokens t -> ("``"++t++"''") : restoreStringBrack tokens tt'
   t :tt'                        -> t : restoreStringBrack tokens tt'
   _                             -> tt
removeStringBrack tokens = map (mkFreeId tokens)

isFreeIdent toks s = s /= "" && not (elem s toks)

mkFreeId :: [String] -> String -> String
mkFreeId tokens str = case str of
  '`': '`' : s2 -> case reverse s2 of
     '\'':'\'':s | isFreeIdent tokens s' -> s' where s' = reverse s
     _ -> str
  _ -> str
