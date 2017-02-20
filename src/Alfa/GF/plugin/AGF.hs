module AGF(
 AGFGrammar,Imports,State,
 initAGF,
 parseGrammar,importGrammar,
 languages,langName,
 linearize,parseExp,wordsOfLang
 ) where

import Operations
import Tokens
import Grammar
import IOGrammar
import PGrammar
import PrGrammar (Print)
import SymbolTable
import GFCFIdents
import Macros
import Update
import Parsers
import Chart
import EBNF
import Parsing
import CF
import Predefined
import Linearize
import GFtoCF

import List (nub)
import ListUtil(mapFst)

import AlfaToks

import PluginAlfa

--- import PescaAlfa (tryProveAlfa, eMeta) ---- SC


import qualified AlfaPluginKit as A

-- using Alfa as interface to GF. Thomas Hallgren & Aarne Ranta 27/3/2000 -- 1/4

type AGFPluginState = GFState Str Annotations
type PureState = GFPureState Str
type State = AGFPluginState
type AGFGrammar = (Imports,Grammar [Str])
stat s = (s,[])

type Annotat = String ---
type Annotations = [Annotat]

-- A function to create the initial state
initAGF :: IO State
initAGF = initGF'

-- A function to get a list of supported languages.
languages :: State -> [Language]
languages = allLanguages

langName = languageName

-- functions to get the grammar, CF grammar, and Exp parser of a language
grammarOfLang :: Language -> State -> GrammarST [Str]
grammarOfLang = grammarOfLanguage

cfOfLang :: Language -> State -> GFCF Str
cfOfLang = cfOfLanguage

wordsOfLang :: Language -> State -> [String]
wordsOfLang = wordsOfLanguage

parserOfLang :: Language -> State -> Cat -> WParser Str A.Exp
parserOfLang lang st cat = mapFst term2aexp . parserOfLanguage lang st cat

-- function that imports a grammar to Alfa
parseGrammar :: String -> (FilePath,String) -> A.Parsed (A.Module,AGFGrammar)
parseGrammar suff (name,src) =
  case parses (pSomeGrammar suff) src of
    (gr,""):_ -> Right (grammar2atheory gr,gr)
    (_,s):_   -> err ("grammar" +++ name +++ "has error in" +++ take 333 s)
    _         -> err ("no parse of grammar" +++ name)
  where
    err msg = Left (A.unknownSourcePos,msg)

    pSomeGrammar suff = case suff of
      "ebnf" -> (pEBNF *** (((,)[]) . ebnf2gf id)) .<. remComments
    ---  "cf" ->
    ---  "gfo" ->
      _ -> pGrammar .<. remComments

    grammar2atheory (importnames,gr@(Grammar (abs1,_))) =
	A.Module (imports++decls)
      where
	imports = [A.ImportDecl (A.Import path)|path<-importnames]
	decls = decl1 (map A.defA theory)
	   where decl1 [] = []
		 decl1 ds = [A.decl' ds]
	theory = abstract2atheory abs1

importGrammar :: State -> FilePath -> [String] -> AGFGrammar -> A.Parsed State
importGrammar stat@((abs0,cncs0), status) name langnames
                    (imports,gr@(Grammar (abs1,cnc1@(Concrete crs)))) = 
 let abs = if True{-status == []-} then updateAbstractST abs0 abs1 else abs0

     --lang = getLangName name
     lang:_ = map Language langnames -- !!!
     (_,cnc0) = grammarOfLang lang stat
     cnc = snd $ plainUpdOptGrammarST (abs,cnc0) gr
     (abs',cnc') = balanceGrammarST (abs,cnc)

     cf0 = cfOfLang lang stat 
     cf1 = case grammar2cf (abs',cnc') cnc1 of
             Ok c -> fst c           -- ignoring snd, the error message!
             _ -> emptyCF

     cf  = unionCF cf0 cf1

     cncs = if null crs then cncs0 else updLang cncs0 
              where updLang cc = 
                      case cc of
                        (l,c):cc' | l == lang -> (lang,(cnc',cf)):cc'
                        lc:cc'                -> lc : updLang cc'
                        _ -> [(lang,(cnc',cf))]
 in Right ((abs', cncs),[name]{-??-})
--- duplicates some of the code of importJustGrammar

-- Only the first import updates Abstract. Thereafter it is assumed
-- that the imported grammars have the same Abstract. They need not even
-- import it, but it makes no harm if they do.
-- TODO: error reporting if a grammar imported is not compatible with Abstract.

linearize :: State -> Language -> A.MetaEnv -> A.Syntax -> Maybe A.SyntaxText
linearize st lang env term =
  case term of
    A.ExpS e -> Just $ linAlfa gr env e
    _ -> Nothing
   where gr = grammarOfLang lang st

linAlfa :: GrammarST [Str] -> A.MetaEnv -> A.Exp -> A.SyntaxText
linAlfa gr@(abs,cnc) env t = case lint of
  Ok (s:_)  -> gfOutputToAlfaText $ showTok s
  Ok _  -> [] 
  Bad s -> gfOutputToAlfaText "???" -- $ prt gt --- showTok $ [Str s]
 where 
   gt = aexp2term env t
   lint = do
       let t' = updateTerm abs gt
       allLinearizes t' gr

-- A function to parse expressions
parseExp :: State -> Language -> A.Exp -> String -> A.Parsed [A.Exp]
--- parseExp st lang typ "SC" = Right [tryProveAlfa eMeta typ] ---- SC
parseExp st lang typ str = mkAParser (parserOfLang lang st cat) str where
  cat = aexp2cat typ

mkAParser :: (Token s, Eq c) => WParser s c -> String -> A.Parsed [c]
mkAParser pars s = case nub (wParseResults pars s') of
  [] -> Left ((0,0),"no parse")
  ts -> Right ts
 where
   s' = string2toklist s

atext2string :: AText -> String
atext2string = alfaTokens2String

