module GFAlfaLib(
  module GFAlfaLib,
  module A2GSyntax,
  module PGrammar,
  module Operations,
  module Parsers,
  -- module AuxParsing,
  module PTypes,
  module Grammar,
  Grammar.Grammar,
  module Types,
  module AgdaToGrammar,
  module CheckGrammar,
  module PrGrammar,
  module PrTypes,
  module IOGrammar,
  module G2ASyntax,
  --module Grammar,
  module AuxParsing
 )
 where
--import AlfaGF(prprA)
import A2GSyntax(decls2defs,GGf(..))
import qualified AgdaEng
import qualified AgdaSve
import PGrammar(pGrammar,pEntry)
import Operations((+++))
import Parsers
--import AuxParsing()
import PTypes()
import Grammar(Grammar)
import Types(typeForm,Symb(..))
import AgdaToGrammar(ATheory,a2gDD,a2gD,a2gG,a2aatheory,ADef,Annotation,defaultEntry)
import CheckGrammar(checkGrammar)
import PrGrammar(prGrammar,prAnnot)
import PrTypes(prTerm)
import IOGrammar(grammar2grammars)
import G2ASyntax(parseExpr,parseElem)
--import Grammar ()
import AuxParsing ()

import Char(isDigit,isSpace)
import Maybe(mapMaybe)
import Ix

import UAbstract(Decls)
--import Debug2(trace)

--trace x y = y

data Language = E | S deriving (Eq,Enum,Bounded,Ord,Ix,Show,Read)

langRange = (minBound,maxBound)
languages = range langRange::[Language]

langName E = "English"
langName S = "Svenska"

baseGrammars =
    [(E,grammarEng),(S,grammarSve)]
  where
    grammarEng = AgdaEng.grammar4Defs
    grammarSve = AgdaSve.grammar4Defs

-- From AlfaGF:

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

--

-- From PATheory:


-- an entire annotated theory: import list + Agda theory + annotations
pAATheory :: Grammar -> Decls -> [String] -> ([ADef], [Annotation])
pAATheory gr decls annots =
   (th',aa)
 where
   th' = concatMap a2gDD $ decls2defs decls
   th'' = map a2gD th'
   aa = mapMaybe pA annots

   pA annot =
     case pJ (pAnnotation gr th'') annot of
       ((a,s):_) -> (if (all isSpace s)
	             then id
		     else id{-trace ("skipped "++s)-}) $ Just a
       _ -> --trace ("No parse for "++annot)
	    Nothing

-- one single annotation, e.g. {-+ Abs - "we have a contradiction" +-}
--pAnnotation :: Grammar -> [GDef] -> Parser Char Annotation
pAnnotation gr defs =
  pIdent                                          .>. (\ident ->
  jL "-"                                          .>. (\_ ->
  maybe fails (pEntry . envt) (lookup ident defs) .>. (\entry ->
  succeed (ident,entry))))
   where 
    envt typ = (gr,[],[(x,(n,typeForm t)) | 
                          ((x,t),n) <- zip [(x,t) | (Symb x,t) <- xx] [0..]])
                                             where (xx,_,_) = typeForm typ

