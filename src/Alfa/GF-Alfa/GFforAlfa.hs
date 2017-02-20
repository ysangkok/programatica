-- Using GF as interface to Alfa.
-- Thomas Hallgren & Aarne Ranta 18/3/2000 -- 3/4
module GFforAlfa(
   -- Exported types
   AlfaGFPluginState,P.Language,LangName,Errors,

   -- Plugin state init and queries:
   supportedLanguages,
   initGF,
   langName, languages, wordsOfLang,
   grammarOfLang, -- for AGE

   -- Loading and saving documents:
   importModule, loadModule, saveAnnotations,

   -- Accessing and manipulating annotations:
   currentAnnotation,  currentAnnotationCon,
   changeAnnotation,  changeAnnotationCon,
   badAnnots,

   -- Parsing and printing:
   linearize, parseExp,
 ) where

import AlfaAbs -- GF grammar for Alfa's abstract syntax
import AlfaToks

import qualified AlfaPluginKit as A

import AlfaToGrammar as AA ---
import qualified PATheory as P ---
import qualified A2GSyntax as AG ---
import qualified G2ASyntax as GA ---
import qualified UseGF as  G ---
import Parsers ((***),(.<.),parses,newParseResults) ----
import PGrammar2() -- TokenParser instances
--import LexGrammar(glexpos)
import LexPos()
import AIdent
import List
import ListUtil(mapFst)
import Char(isSpace)
import IO(try)
--import Maybe(fromJust)
import Debug2(trace,badtrace)

import ParseGFAlfa (grammar2cfReducedAlfa, parserGFAlfa)

-- The function that tells what languages are supported.
-- Abstract syntax is built-in, concrete syntax is loaded from grammar files.
supportedLanguages = words "Eng Fra Sve" :: [LangName]
alfaGrammarFileName lang = "alfa." ++ lang ++ ".gf" :: FilePath

type Language = P.Language 
type LangName = String
pLanguage = P.Language
langName = P.langName -- language code (abbreviated language name)
prLanguage (P.Language l) = l -- should be a user friendly name of the language

type AlfaGFPluginState = (PureState,Annotations)
type PureState = (AA.AbstractAST,[(Language,(AA.ConcreteAST, GFCFA))])
type State = AlfaGFPluginState

type Annotat = String ---
type Annotations = [((Language,AIdent),(Errors,Annotat))]
type Errors = [String]

-- List of pretty-printed names of identifiers with bad annotations:
badAnnots :: State -> [String]
badAnnots (st,annots) =
    nub [idFromGF n|((lang,n),(_:_,_))<-annots,lang `elem` langs]
  where langs = languages' st

-- A function to create the initial state
initGF :: [LangName] -> [FilePath] -> IO AlfaGFPluginState
initGF langs libDirs = do
    let (abs,fcncs0) = coreGrammars langs
    fcncs0' <- G.mapPairListM (findGrammarFile . snd)  fcncs0
    let fcncs = [(lg,file)|(lg,Just file)<-fcncs0']
    cncs0 <- G.mapPairListM (getGrammarSTFromFile . snd) fcncs
    let cncs1 = [(lan,st) | (lan,st) <- cncs0]
    cncs2 <- G.mapPairListM mkReducedCF cncs1
    putStrLn $ "read core grammars for " ++ 
               let ll = map (langName . fst) cncs2 in 
                 if null ll then "no languages" else unwords ll
    return ((abs,cncs2),[])
   where
     findGrammarFile grammarname = find filenames
      where
        filenames = nub (grammarname:[dir++"/"++grammarname|dir<-libDirs])
 
        find [] = do putStrLn ("Didn't find any of "++unwords filenames)
                     return Nothing
        find (f:fs) = 
          do r <- try (readFile f)
	     case r of
	       Right _ -> do putStrLn ("Found "++f)
			     return (Just f)
	       _ -> find fs

coreGrammars langs = 
  (grammarAlfaAbs,[(pLanguage lang, alfaGrammarFileName lang) | lang <- langs])

getGrammarSTFromFile :: String -> IO GrammarAST
getGrammarSTFromFile file = do
  putStrLn ("reading " ++ file)
  gr <- G.readGrammarFile' [alfaGrammarFileName "Abs"] True file
  return $ case (G.updOptGrammarST (grammarAlfaAbs, G.predefConcreteST)) gr of
             G.Ok t -> t
             _ -> G.predefGrammarST

mkReducedCF :: (Language,GrammarAST) -> IO (ConcreteAST, GFCFA)
mkReducedCF (_,st) = do
  let gr@(G.Grammar (_,conc)) = G.st2grammar st
  (cf,msg) <- case grammar2cfReducedAlfa st conc of
                    G.Ok (c,[]) -> return (c, "parser OK.")
                    G.Ok (c,ss) -> return (c, ss)
                    G.Bad s     -> return (G.predefCF, s)
---  putStrLn msg
  return (snd st,cf)

-- A function to get a list of currently active languages.
languages :: State -> [Language]
languages =  languages' . fst
languages' :: PureState -> [Language]
languages' = map fst . snd

-- functions to get the grammar, CF grammar, and Exp parser of a language
grammarOfLang :: Language -> PureState -> GrammarAST
grammarOfLang lan (abs,cncs) = case lookup lan cncs of
  Just (cnc,_) -> (abs,cnc)
  _ -> (abs,(G.NT,0))

cfOfLang :: Language -> PureState -> GFCFA
cfOfLang lan (_,cncs) = case lookup lan cncs of
  Just (_,cf) -> cf
  _ -> ([],const [])

wordsOfLang :: Language -> State -> [String]
wordsOfLang lan ((abs,cncs),_) = case lookup lan cncs of
  Just (_,cf) -> G.wordsOfCF cf --- put this directly into the state ?
  _ -> []

parserOfLang :: Language -> State -> G.WParser ATok A.Exp
parserOfLang lang (st@(abs,_),_) = mapFst mkExp . parserGFAlfa (cfOfLang lang st)
  where mkExp = GA.fgTrm . G.updateTerm abs 

-- loadModule: updates the state when a document is loaded into Alfa or when
--            new declarations are appended to the end of the current document.
-- importModule: updates the state when a document is imported into Alfa.
loadModule, importModule
  :: State -> A.Decls -> [(A.SourcePos,Annotat)] -> A.Parsed State
loadModule = loadOrImportModule (++)
importModule = loadOrImportModule const

loadOrImportModule ::
    (Annotations->Annotations->Annotations) ->
    State -> A.Decls -> [(A.SourcePos,Annotat)] -> A.Parsed State
loadOrImportModule combine (oldpure,oldannots) m aa =
  case loadModule' oldpure m aa of
    ((pure,newannots),msg) | all null msg ->
      Right (pure,combine oldannots newannots)
    (_,msg)  -> Left ((0,0), unlines msg)

loadModule' ::PureState -> A.Decls -> [(A.SourcePos,Annotat)] -> (State,Errors)
loadModule' pureSt0 decls annots0 = ((pureSt,annots),msgs)
  where
    abs1@(G.Abstract gdefs) = AA.alfaDecls2Abstract decls
    pureSt1 = extendAbstractGrammar pureSt0 abs1
    langs = languages' pureSt1

    userAnnots = map mkAnnot annots0
      where
	mkAnnot (pos,str) = (str,forWhat str) -- source positions are lost
	forWhat str =
	  {-take 1-}
	  [((lang,ident),trim a)|
	   (langname,a)<-lex' str,-- should ignore but still keep comments...
	   let lang = pLanguage langname,
	   (ident,_)<-parses pAIdent a]
	lex' s = {-take 1-}[r|r@(_:_,_)<-lex s]
		                      -- lex is hopefully deterministic

    defaultAnnots =
	[(lang,AA.defaultGFAlfaLin fun typ) |
         lang <- langs, 
         G.DefFun fun typ <- gdefs,
         (lang, G.symid fun) `notElem` userAnnotated]
       where
         userAnnotated = concatMap (map fst . snd) userAnnots

    (pureSt2,annots) = foldr addAnnot (pureSt1,[]) userAnnots
      where
        addAnnot (str,ann) state@(pureSt,annots) =
	  case ann of
	    ((lang,ident),ldef):_ ->
	      if lang `elem` langs
	      then
	        case parses pDefLin ( ldef) of
		  (a,""):_ -> fst (updateAnnotationId state lang ident ldef a)
		  _ -> trace ("syntax error ("++show (lang,ident)++": "++str) $
		       (pureSt,((lang,ident),(["syntax error"],ldef)):annots)
	      else (pureSt,((lang,ident),(["ignored"],ldef)):annots)
	    _ -> badtrace ("discarded: "++str) $
                 state -- all bad annotations are discarded !!

    (pureSt,msgss) = mapAccumL updatePureAnnotationId pureSt2 defaultAnnots
    msgs = concat msgss -- Errors indicate bugs in the system or the grammars...

    extendAbstractGrammar :: PureState -> G.Abstract -> PureState -- no errors?
    extendAbstractGrammar core@(abs0,cncs0) abs1 = (abs, cncs0)
      where
   -- Ugh. Here we supply empty concrete grammars and then discard the result...
	(abs,_)  = G.plainUpdOptGrammarST
		     (abs0,G.emptyConcreteST)
		     ((G.Grammar (abs1,G.Concrete []))::(G.Grammar AA.AToks))

saveAnnotations :: State -> [String]
saveAnnotations = sort . map prJustAnnotation . snd
  where
    prJustAnnotation ((l,_),(_,a)) = prLanguage l ++ " " ++ trim a

currentAnnotation = currentAnnotation' . consToGF
currentAnnotationCon = currentAnnotation' . constrToGF

currentAnnotation' :: AIdent -> State -> Language -> (Errors,Annotat)
currentAnnotation' i st@((abs,_),as) lang =
  case lookup (lang,i) as of
    Just eannot -> eannot
    _ -> case G.lookupFun i' abs of
           G.Ok typ -> ([],prAnnot (AA.defaultGFAlfaLin i' typ))
           G.Bad s  -> ([s],[])
          where i' = G.zIdent i

prAnnot :: G.LDef AA.AToks -> String
prAnnot (G.DefLin f xx t) = 
  idFromGF (G.prt f) G.+++ unwords (map G.prt xx) G.+++ "=" G.+++ G.prt t
prAnnot t = G.prt t

atext2string :: AText -> String
atext2string = alfaTokens2String

linearize :: State -> Language -> A.Syntax -> Maybe A.SyntaxText
linearize (st,_) lang term =
  case term of
    A.ExpS e -> linAlfa gr term e
    A.DefBS d -> linAlfa gr term d
    _ -> Nothing
  where
    gr = grammarOfLang lang st

    linAlfa :: AG.GGf t => GrammarAST -> A.Syntax -> t -> Maybe A.SyntaxText
    linAlfa gr@(abs,cnc) stx t = case lin t of
      G.Ok (s:_)  -> Just $ gfOutputToAlfaText $ G.showTok $ s
      G.Ok _  -> Just [] 
      G.Bad s -> trace ("lin error " ++ s) $
		 Just [A.PlainPara [A.TPlain ("GF: "++firstline s)],
		       A.PlainPara [A.TSyntax stx]]
		 --Just $ gfOutputToAlfaText $ G.showTok $ [G.Str t0]
     where lin t = do
	     let t' = G.updateTerm abs (AG.ggf abs t)
	     G.allLinearizes t' gr
	   t0 = G.prt $ AG.ggf abs t
	   firstline = concat . take 1 . lines

changeAnnotation :: State -> Language -> A.Var -> Annotat -> A.Parsed State
changeAnnotation s l = changeAnnotationId s l . consToGF

changeAnnotationCon :: State -> Language -> A.Con -> Annotat -> A.Parsed State
changeAnnotationCon s l = changeAnnotationId s l . constrToGF

changeAnnotationId :: State -> Language -> AIdent -> Annotat -> A.Parsed State
changeAnnotationId s l c a =
  case newParseResults pDefLin ( a) of
    Right t -> case updateAnnotationId s l c a t of
	         (upd,[]) -> Right upd
	         (_,msg)  -> Left ((0,0), unlines msg)
    Left err -> Left ((0,0),err)

updateAnnotationId :: 
  State -> Language -> AIdent -> Annotat -> AA.Annotation  -> (State,Errors)
updateAnnotationId st@(gr,aa) lang c a ldef = ((gr',aa'),msgs)
  where
    (gr',msgs) = updatePureAnnotationId gr (lang, ldef)
    aa' = ((lang,c),(msgs,a)) : filter ((/=(lang,c)).fst) aa

updatePureAnnotationId ::
  PureState -> (Language, AA.Annotation) -> (PureState,Errors)
updatePureAnnotationId st@(abs,cncs) (lang, ldef) = 
  let gr@(abs0,(cnc,mx)) = grammarOfLang lang st
      cf  = cfOfLang lang st
      (ldef',linfo') = G.updateLDef gr ldef
      msgs  = G.checkLDef gr ldef'
      cnc' = (G.updateTreeEq G.eqStrIdent linfo' cnc, mx) 
      gr'  = (abs0,cnc')
      cf'  = case grammar2cfReducedAlfa gr' (G.st2concrete cnc') of
               G.Ok (c,_) -> c
               _ -> cf
---      cf'   = G.tryUpdateCF gr' cf (G.zIdent c) --- would be optimal if worked...
      cncs' = G.updateLookupList (lang,(cnc',cf')) cncs
   in ((abs,cncs'),msgs)

-- A function to parse expressions. (Type Parsed as above.)
parseExp :: State -> Language -> String -> A.Parsed [A.Exp]
parseExp st lang = mkAParser (parserOfLang lang st)

mkAParser :: (G.Token s, Eq c) => G.WParser s c -> String -> A.Parsed [c]
mkAParser pars s = case nub (G.wParseResults pars s') of
  [] -> Left ((0,0),"no parse")
  ts -> Right ts
 where
   s' = G.string2toklist s


pDefLin = P.pDefLin .<. rmcomments

-- Annotation cleaning:

rmcomments = G.remComments
{- unlines . map rmcomment . lines
  where
    rmcomment ('-':'-':_) = ""
    rmcomment (c:cs) = c:rmcomment cs
    rmcomment "" = ""
-}

trim = reverse . trim1 . reverse . trim1
  where
    trim1 = dropWhile isSpace
