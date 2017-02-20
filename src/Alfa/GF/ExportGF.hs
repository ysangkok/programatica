module ExportGF where

import qualified Grammar as G
import qualified Macros as M
import qualified SymbolTable as S
import qualified Parsing as P
import qualified Linearize as L
import qualified IOGrammar as I
import qualified PrGrammar as R
import qualified PGrammar2 as A
import qualified Predefined as D
import qualified Tokens as T
import qualified Lookup as L
import qualified GFCFIdents as C
import qualified Update as U
import qualified CustomFormats as F
import qualified ComputeTerm as Co
import qualified Arch
import qualified CF
import qualified State

import Option
import Tokenize (tokens)
import TC () -- for hbc
import TypeCheck
import Paraphrases (mkParaphrases)
import GFtoXML (prXTrm)
import Operations
import LexPos() -- hmm
import List
import Arch (myStdGen,fudlogueWriteUni)
import Randomized (mkRandomTerms)
import Morphology

-- top-level functions from GF to use in Haskell programs. AR 2/9/2000

-- the most important functions

linearize :: Grammar -> Tree -> Str
parse     :: Grammar -> Str -> [Tree]
translate :: Grammar -> Grammar -> Str -> [Str]

-- some more functions

linearizeAll  :: Grammar -> Tree -> [Str]  -- give all forms
parseSloppy   :: Grammar -> Str  -> [Tree] -- tolerate grammatical errors
correctErrors :: Grammar -> Str  -> [Str]  -- suggest corrected forms

-- customized parsing, linearization, and translation

parseCustom     :: [Option] -> Cat -> Grammar -> Str  -> [Tree]
linearizeCustom :: [Option] -> Grammar -> Tree -> [Str]
translateCustom :: [Option] -> Cat -> Grammar -> Grammar -> Str -> [Str]
translateMany   :: [Option] -> Cat -> [Grammar] -> Str -> [Str]

-- type checking and other tree manipulations

typeCheck  :: Grammar -> Tree -> [Tree]  -- failure = []
compute    :: Grammar -> Tree -> Tree
paraphrase :: Grammar -> Tree -> [Tree]

-- random generation
generateRandom :: Grammar -> Cat -> Int -> IO [Tree]

-- homonyms = trees linearized in the same way (of a given category)
homonyms :: Grammar -> Cat -> Tree -> [Tree]

-- morphological analyser
morphology :: Grammar -> Str -> Str
morphology1  g = prMorphology . appMorpho (mkMorpho g)
morphology g = let m = mkMorpho2 (grammar g) in prMorphology . appMorpho2 m
prMorphology (s,ss) = s ++++ unlines ss

-- read a GF grammar from a file

getGrammar       :: FilePath -> IO Grammar
getGrammarMsg    :: FilePath -> IO Grammar -- print error messages on the way
getGrammarCustom :: [Option] -> FilePath -> IO Grammar

-- read and write trees

readTree :: String -> Tree
showTree :: Tree -> String

-- send translation into a fudget with unicode fonts (only for hbc)

showUnicode :: String -> IO ()

------------------------------------------------------------------------

-- basic datatypes

type Grammar  = ((Abstract,Concrete),CF)
type Abstract = S.AbstractST
type Concrete = S.ConcreteST [T.Str]
type CF       = C.GFCF T.Str

type Str  = String
type Tree = G.Trm
type Cat  = String

-----------------------------------------------------

-- definitions of functions

linearize g = head . linearizeCustom [firstLin] g
parse g = parseCustom defaultParseOpts (firstCat g) g
translate g1 g2 = map (linearize g2) . parse g1

linearizeAll = linearizeCustom [allLin]
parseSloppy g = parseCustom [forgiveParse,chartParse] (firstCat g) g
correctErrors g = concat . map (linearizeAll g) . parseSloppy g

parseCustom opts cat gr@(st@(abstr,cnc),cf) s =
  if elem dontParse opts then [readTree s] else 
  snd (op s)
    where
      lang = I.Language "this"
      stat = ((abstr, [(lang,(cnc,cf))]),())--- :: I.GFState [T.Str] ()
      cat' = mkRealCat gr cat
      tz   = I.tokenizerOfLanguage lang stat
      lit  = CF.predefOfCF cf . T.Str
      op   = P.mkOptParserOne opts (I.parserOfLanguage' opts lang stat cat') lit tz

linearizeCustom opts gr trm
  | elem dontLin opts     = [showTree trm]  -- just show the term 
  | elem showXML opts     = [prXTrm (abstract gr) trm]
  | elem distinctLin opts = nub lins
  | elem allLin opts      = lins
  | elem tableLin opts    = lins
  | otherwise             = take 1 lins
 where lins = I.justLinearizeTerm' opts (fst gr) trm

translateCustom opts cat g1 g2 = 
  concat . map (linearizeCustom opts g2) . parseCustom opts cat g1

translateMany opts cat gs s = 
  concat $ concat [[linearizeCustom opts g i | i <- ip] | g <- gs]
   where  
     ip =  concat $ map (\g -> parseCustom opts cat g s) gs

typeCheck = typeCheckTerm . abstract

compute g = Co.compute (abstract g) 

paraphrase g = mkParaphrases (abstract g)

generateRandom g cat n = do
  gen <- myStdGen n
  return $ mkRandomTerms gen 23 (abstract g) (mkRealCat g cat) -- 23 = max tries

homonyms gr cat = parseCustom [] cat gr . linearize gr

getGrammar = getGrammarCustom []
getGrammarMsg = getGrammarCustom [beVerbose]

getGrammarCustom opts file = do
  gr <- F.readGrammarFormatFile' [] verb file     
  (_,st0,cf) <- I.makeAllFormatsAdd verb comp D.predefGrammarST gr
  let st = if whole then st0 else U.minimizeGrammarST st0
  return (st,cf)
 where 
   verb  = elem beVerbose opts
   comp  = elem isCompiled opts 
   whole = elem wholeGrammar opts
-- [] = no excludes 
-- grammar format is decided from file suffix

readTree = A.pTrm
showTree = R.prt

showUnicode = Arch.fudlogueWriteUni id . Just

-- auxiliaries

firstCat :: Grammar -> Cat
firstCat g = head [cat | G.Ident (cat,(i,_)) <- L.allCats (abstract g), i>0]

abstract :: Grammar -> Abstract
abstract g@((a,_),_) = a

grammar :: Grammar -> (Abstract,Concrete)
grammar = fst

mkRealCat :: Grammar -> Cat -> G.Cat
mkRealCat g = U.updateIdent (abstract g) . M.zIdent

mkExportCat :: G.Cat -> Cat
mkExportCat = M.symid

mkExportGrammar :: Abstract -> Concrete -> CF -> Grammar
mkExportGrammar a c f = ((a,c),f)
