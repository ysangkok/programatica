module AlfaGF where

import Operations
import Grammar 
import IOGrammar (grammarFromFiles)
import PrGrammar (prRule)
import AgdaToGrammar --(a2aatheory,a2gG,adef2rule)
import A2GSyntax (GGf (lin, ggf), isConstrDef, Theory, gKind, GKind (..))
import G2ASyntax
import GSyntax (Gf)
import Chart
import Parsing
import PATheory
--import PAgda (pAExp) -- should be replaced by the standard parser ---
import UAbstract (Exp(..), DefB)
import Char(isDigit)
import Parsers()
import AuxParsing()
import Types()
--import AgdaEng
import AgdaSve

-- interface operations between Agda/Alfa in GF. AR 15/10/1999 -- 21/12

-- In the beginning of a session:
-- to read in an annotated Agdfa theory file, 
-- returning a GF grammar and a text with the new definitions.
-- This theory file may be as simple as
--
-- {--# Import agdaEng.gf #--}
--
-- importing the basic English grammar with no new constants
getATheory :: String -> IO ((Grammar,CfGrammar),[AlfaToken],String)
getATheory a =
 do s      <- readFile a
    let ii = importsA s
    (g0,_) <- grammarFromFiles grammar4Defs ii  -- using builtin agdaEng.gf
    let
       (g1,th) = 
           case pAATheory g0 s of
              ((x,y),_):_ -> (a2aatheory g0 y, x)
              _           -> ([],[])
       gr = a2gG g0 g1
       cf = grammar2cfgrammar gr
       tt = tokATheory gr (filter (not . isConstrDef) th)
       fi = unlines 
               (("Import" +++ prTList ", " ii +++ ";") : "Rules" : 
                [prRule gr r +++ ";" | r <- map adef2rule g1])
    return ((gr,cf),tt,fi)

-- to extend a grammar by a definition and annotation
extendATheory :: Grammar -> (DefB,Entry) -> Grammar
extendATheory gr0 da = a2gG gr0 [da]

-- to parse an annotation for a given definition
parseAnnot :: Grammar -> DefB -> String -> Entry
parseAnnot gr def s =
 case pAnnotation gr [def'] s' of
   ((_,entry),_):_ -> entry
   _               -> findEntry gr ([],[]) def' -- return the default entry
  where 
   def'@(fun,_) = a2gD def
   s' = "{-+" +++ fun +++ "-" +++ s +++ "+-}" ---should be done in PATheory

-- to print a given Alfa definition/expression as a token list and text
alfa2text :: (Gf a, GGf a) => Grammar -> a -> ([AlfaToken],String)
alfa2text gr a = (toks, prpr toks) where toks = prprA (lin gr a)

-- to parse a string as Alfa expression to replace a meta
text2alfa :: (Grammar,CfGrammar) -> Int -> String -> Exp
text2alfa grcf meta s = 
 case parseExpr grcf s of
   [e] -> e
   _   -> EMeta meta

-- for Alfa interaction by pointing, we need token lists
data AlfaToken = AString String | AMeta Int | ANewline | AIndent
  deriving (Read,Show,Eq)

-- to get Alfa token list from linearization result
prprA :: String -> [AlfaToken]
prprA str = prA str [] where
 prA ('#':'#':s) tt = AString (reverse tt) : ANewline : prA (dropWhile (==' ') s) []
 prA ('#':'%':s) tt = AString (reverse tt) : AIndent : prA s []
 prA ('[':'?':s) tt = AString (reverse tt) : pcA s []
 prA (c:s)       tt = prA s (c:tt)
 prA []          tt = [AString (reverse tt)]
 pcA ('?':']':s) tt = AMeta (read (reverse tt)) : prA s []
 pcA (c:s) tt | isDigit c = pcA s (c:tt)
 pcA (c:s) tt             = pcA s tt --- removes spaces and curly brackets
 pcA s _                  = [AString "UNTERMINATED METAVARIABLE"]

tokATheory :: Grammar -> ATheory -> [AlfaToken]
tokATheory g = prprA . (lin g) . Theory

-- simple pretty printer for Alfa token lists
prpr :: [AlfaToken] -> String
prpr (AString s : tt) = s   +++ prpr tt
prpr (AMeta n   : tt) = "[?" ++ show n ++ "?]" +++ prpr tt
prpr (ANewline  : tt) = "\n" ++ prpr tt
prpr (AIndent   : tt) = "  " ++ prpr tt
prpr []               = ""

-- to print the import list in grammar
importsA :: String -> [String]
importsA s = 
 case pAImport s of
   (f:ff,_):_ -> f:ff
   _          -> [{-"agdaEng.gf"-}] -- agdaEng.gf is bulltin, see above


