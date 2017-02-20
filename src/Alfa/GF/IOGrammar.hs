module IOGrammar where

import Operations
import ParserType(parses,newParseResults)
import CF
import PPrCF
import Tokens
import Grammar
import Macros
import Update
import Lookup
import ComputeTerm
import Predefined
import SymbolTable
import CFtoGrammar
import GFtoCF
import OldGF ()
import OldGFtoGrammar
import qualified OldPGF -- (getGrammarFromFile) 
import CheckGrammar
import PrGrammar
import PGrammar2(pImports,pGrammar,ParseToken) -- new parser
import LexGrammar(glexpos,remComments) -- new lexical analysis
import Linearize
import GFCFIdents
import Parsing

import List (nub, sort)
import Char (isSpace)

import Option
import EBNF
import Tokenize
import GrammarToLatex

-- functions for reading grammar files and testing them AR 30/1/2000 -- 28/4/2000

-- the environment used for parsing and linearization
type GFEnv a = (Grammar [a], GrammarST [a], GFCF a)
type Concr a = (Concrete [a], ConcreteST [a], GFCF a)

-- used in AGE
getGFEnvFromFile :: ParseToken a => String -> IO (GFEnv a)
getGFEnvFromFile file = do
  igr <- readGrammarFile False file
  makeAllFormats igr

-- read grammars in different formats

readStrGrammarFile :: String -> IO (Grammar [Str])
readStrGrammarFile = readGrammarFile True

readCompiledGrammarFile :: String -> IO (Grammar [Str])
readCompiledGrammarFile file =
 do s <- readFileIf file
    return (read s)

readGrammarFile :: ParseToken a => Bool -> String -> IO (Grammar a)
readGrammarFile v file = readGrammarFile' [] v file

readGrammarFile' :: ParseToken a => [String] -> Bool -> String -> IO (Grammar a)
readGrammarFile' excludes v file = do
   files <- getImports file
   ggtt  <- mapM readOne (filter (flip notElem excludes) (nub files))
   let (gg,tt) = unzip ggtt
   return $ if and tt then unionGrammars gg else emptyGrammar
  where
   getImports f = do
     p <- parseFileIf (parses pImports) f
     let imps = case p of
                  (ii,_):_ -> map (initFilePath f ++) ii
                  _ -> []
     ff <- mapM getImports imps
     return (concat ff ++ [f])
   readOne f = do
     p <- parseFileIf (newParseResults pGrammar) f
     let (((_,gr),msg),stat) = case p of
           Right ig  -> ((ig, "whole grammar" +++ f +++ "accepted"),True)
           Left msg  -> ((([],emptyGrammar), msg),False)
     putStrLnV v msg
     return (gr,stat)
   parseFileIf p f = (p . glexpos) `fmap` readFileIf f

readOldGFFile :: Bool -> String -> IO (Grammar [Str])
readOldGFFile v file =
 do (g,msg) <-  OldPGF.getGrammarFromFile file
    putStrLnV v msg
    let gr@(Grammar (th,conc)) = onGrammar g
        file' = file ++ ".gfn"
    putStr ("save new format in " +++ file' +++ "(y/n) ? ")
    s <- getLine
    (case s of
       y:_ -> writeFile file' (prt gr)
       _ -> return ())
    return gr

readCFFile :: Bool -> String -> IO (Grammar [Str])
readCFFile v file =
 do s <- readFileIf file
    let cf = ([rule | Just rule <- map getCFRule (lines s)], emptyCFPredef)
    putStrLnV v (prCF cf)
    let gr = (cf2grammar id cf)
    return gr

readEBNFFile :: Bool -> String -> IO (Grammar [Str])
readEBNFFile v file =
 do s <- readFileIf file
    let tokdef = case words (takeWhile (/='\n') s) of
          "--#":"tokenizer":ident:[] -> [DefTokenizer (zIdent ident)]
          _ -> [] 
        ebnf = case (parses pEBNF . remComments) s of
                (rr,_):_ -> rr
                _ -> []
        Grammar(abs,Concrete ldefs) = ebnf2gf id ebnf :: (Grammar [Str])
    return (Grammar(abs,Concrete (tokdef ++ ldefs)))

-- make different formats

makeAllFormats :: Token a => Grammar [a] -> IO (Grammar [a], GrammarST [a], GFCF a) 
makeAllFormats = makeAllFormatsAdd True False predefGrammarST

makeAllFormatsAdd :: Token a => Bool -> Bool -> 
 GrammarST [a] -> Grammar [a] -> IO (Grammar [a], GrammarST [a], GFCF a) 
makeAllFormatsAdd v compiled ground gr0 
  | isEmptyGrammar gr0 = do
    putStrLnV v "Nothing to do with empty grammar"
    return (emptyGrammar,ground,emptyCF)
  | otherwise = do
    putStrLnV v "Updating symbol table..."
    (st00,msg0) <- return $ (updateGrammarST ground gr0,"update OK.")
    let st0 = balanceGrammarST st00
    putStrLnV v msg0 
    let gr1 = st2grammar st0
    putStrLnV v "Compiling..."
    (st,msg1,optOk) <- return $ 
      if compiled then (st0,"trivial",True) else
        case optGrammarST ground st0 of
          Ok st -> (st, "compiling OK.", True)
          Bad s -> (st0, s ++++ "Warning: Compiling failed", False)
    putStrLnV v msg1
    if not optOk then return (emptyGrammar,emptyGrammarST,emptyCF) else do
      putStrLnV v "Type-checking..."
      putStrLnV v (if compiled then "skipped" else showCheckGrammar st gr1)
      putStrLnV v "Building parser..."
      let gr@(Grammar (_,conc)) = st2grammar st
      (cf,msg2)  <- case grammar2cf st conc of
                      Ok (c,[]) -> return (c, "parser OK.")
                      Ok (c,ss) -> return (c, ss)
                      Bad s     -> return (predefCF, s)
      putStrLnV v msg2
      return (gr1,st,cf)

putStrLnV v s = if v then putStrLn s else return ()

printAllFormats :: Token a => (Grammar [a], GrammarST [a], GFCF a) -> IO ()
printAllFormats (gr,st,cf) = 
 do let (gr',st',cf') = (prt gr, prGrammarST st, prCF cf)
    putStrLn gr' 
    putStrLn st'
    putStrLn cf'

-- parse and linearize

showParses :: (a -> String) -> [a] -> IO ()
showParses pr parses =
 do let p = case parses of
              [] -> "no parse"
              _ -> (unlines (map pr parses)) 
    putStrLn p

linearizeResults :: GrammarST [Str] -> Trm -> [String]
linearizeResults = justLinearizeTerm

linearizeTerm :: GrammarST [Str] -> Trm -> IO ()
linearizeTerm gr trm =
 do let ss = justLinearizeTerm gr trm
    putStrLn $ unlines ss

justLinearizeTerm :: GrammarST [Str] -> Trm -> [String]
justLinearizeTerm = justLinearizeTerm' []

justLinearizeTerm' :: [Option] -> GrammarST [Str] -> Trm -> [String]
justLinearizeTerm' opts gr@(abs,concr) trm =
 let tags  = oElem tableLin opts
     ltx   = oElem latexLin opts
---     morph = oElem morphoLin opts
     t = updateTerm abs trm
     tok = lookupTokenizer concr
     l = case allLinearizes' t gr of
           Ok ss -> if ltx then [mkLinLatex rs] else
                       if tags then 
                          map (uncurry (+++)) 
                               [(prTList " ; " p +++ ":",  s) | (p,s) <- rs]
                       else map snd rs
                        where 
                          rs =
                           [(p,untokens (Tokenizer (symid tok) []) (map showTok s)) 
                                                                      | (p,s) <- ss]
           Bad m -> ["ERROR" +++ m]
 in l

mkCodeTokens :: [String] -> String -> [String]
mkCodeTokens ww = tokens (Tokenizer "freecode" ww)

mkGFCFile f ty p = prelude +++++ def ++ "\n\n" where
 prelude =
  "module" +++ f' +++ "where" ++++
  "import Operations" ++++
  "import Grammar" ++++
  "import SymbolTable" +++++
  "-- this is a machine-generated GF grammar object"
 def =
  f'' +++ "::" +++ ty ++++
  f'' +++ "=" ++++
  unlines (map (indent 2) (cutLine 76 p))
 f' = takeWhile (/= '.') f
 f'' = "grammar" ++ f'

tokensOfCF :: Token a => GFCF a -> [a]
tokensOfCF (rr,_) = nub (concat (map wsr rr)) where
 wsr (_,(_,its)) = [t |  CFTerm r <- its, t <- wordsOfRegExp r] --- if RegEp had * ?

wordsOfCF :: Token a => GFCF a -> [String]
wordsOfCF = sort . (map showTok) . tokensOfCF

translates :: Token a => (String -> [Trm]) -> [GrammarST [a]] -> String -> String
translates parser grammars@((abs,_):_) str =
  let tt   = parser str
      tt'  = map (updateTerm abs) tt
      li g t = case allLinearizes t g of
                 Ok ss -> unlines (map showTok ss)
                 Bad s -> s
      showable = not . all isSpace
  in unlines (filter showable (map (\g -> unlines (map (li g) tt)) grammars))

-- previously in AGF, as special cases

newtype Language = Language String deriving Eq
languageName = prLanguage
prLanguage (Language l) = l
optLanguage = iOpt . prLanguage

getLangName filename = if notElem '.' filename then filename else langname where
 emanelif = reverse filename
 xiferp = tail (dropWhile (/='.') emanelif)
 langname = reverse (takeWhile (flip notElem "./") xiferp)

type GFState a c = (GFPureState a, c)
type GFPureState a = (AbstractST,[(Language,(ConcreteST [a], GFCF a))])
emptyPureState = (predefAbstractST,[])

class HasStatus a where 
  isInitial :: a -> Bool
  notInitial :: a -> a
instance HasStatus [a] where 
  isInitial l = length l == 0
  notInitial l = undefined : l

-- A function to create the initial state
initGF' :: IO (GFState a [c])
initGF' = return ((predefAbstractST,[]),[])

-- A function to get a list of supported languages.
allLanguages :: GFState a c -> [Language]
allLanguages = map fst . snd . fst

abstractOfState :: GFState a c -> AbstractST
abstractOfState ((abs,_),_) = abs

-- functions to get the grammar, CF grammar, and Exp parser of a language
grammarOfLanguage :: Language -> GFState a c -> GrammarST [a]
grammarOfLanguage lan ((abs,cncs),_) = case lookup lan cncs of
  Just (cnc,_) -> (abs,cnc)
  _ -> (abs,emptyConcreteST)

cfOfLanguage :: Token a => Language -> GFState a c -> GFCF a
cfOfLanguage lan ((abs,cncs),_) = case lookup lan cncs of
  Just (_,cf) -> cf
  _ -> ([],emptyCFPredef)

tokenizerOfLanguage :: Token a => Language -> GFState a c -> Tokenizer
tokenizerOfLanguage lan st@((abs,cncs),_) = case lookup lan cncs of
  Just (cnc,_) -> Tokenizer (symid (lookupTokenizer cnc)) ws
  _ -> Tokenizer "mini" []
 where
   ws = wordsOfLanguage lan st

wordsOfLanguage :: Token a => Language -> GFState a c -> [String]
wordsOfLanguage lan ((abs,cncs),_) = case lookup lan cncs of
  Just (_,cf) -> wordsOfCF cf --- put this directly into the state ?
  _ -> []

isWordOfLanguage :: Token a => Language -> GFState a c -> String -> Bool
isWordOfLanguage lan st w = let cf@(_,f) = cfOfLanguage lan st in
  elem w (wordsOfCF cf) ----- || not (null (f (readTok w)))

--parserOfLanguage :: Token a => Language -> GFState a c -> Cat -> WParser a Trm
parserOfLanguage l = parserOfLanguage' (optStrict_optChart) l where
  (optStrict_optChart) = []  --- no options = strict chart parser

parserOfLanguage' opts lang st cat = 
  let cf   = cfOfLanguage lang st
      gr = grammarOfLanguage lang st
      cat' = mkGFCFCat cat
  in parserMethod opts gr cf cat'

-- to be used after circularity test; added 24/9/2001
reduceCfOfLanguage :: Token a => Language -> GFState a c -> [Fun] -> GFState a c
reduceCfOfLanguage lan ((abs,cncs),x) funs = ((abs,cncs'),x) where
  cncs' = [(l,(g, red l cf)) | (l,(g,cf)) <- cncs] 
  red l cf = if l == lan then reduceCF funs cf else cf

importJustGrammar :: (Token a, HasStatus c) => 
    Bool -> Bool -> Bool -> GFState a c -> (String,Grammar [a]) -> IO (GFState a c)
importJustGrammar v c m ((abs0,cncs0), status) (name,gr@(Grammar (abs1,cnc1))) = do
  let st0 = (abs0, predefConcreteST)
  (gr1, st1@(abs,cnc), cf) <- makeAllFormatsAdd v c st0 gr
  let cnc'@(ids,_) = if m  then minimizeConcreteST cnc else cnc
      sz   = show (length (show cnc'))
      dpth = show (depthTree ids)
      lgth = show (length (tree2list ids))
  if v 
    then putStrLn 
          ("Concrete grammar size" +++ sz +++"length" +++ lgth +++"depth" +++ dpth)
    else return ()
  let lang = Language (getLangName name)
      cncs = case cnc1 of
               Concrete [] -> cncs0
               _ -> cncs0 ++ [(lang,(cnc',cf))]
  return ((abs, cncs), notInitial status)

