module Commands where

import Operations
import Parsers
import Tokens
import Grammar
import Macros
import PrGrammar 
import PGrammar(pTrm,pTerm)
import PGrammar2()
import PPrCF
import SymbolTable
import CF
import GFCFIdents
import Update
import IOGrammar
import CFProperties
import GFtoXML (prXAbstractST, prXTrm)
import OldGFtoGrammar (prOldTrm, pOldTrm)
import Randomized
import Tokenize
import HelpFile
import CustomCommands
import State
import GrammarToLatex
import GrammarToHaskell
import Option --- hiding (Opt)  -- not supported by hbc
import Arch (myStdGen)

import TeachYourself
import qualified ExportGF as E
import Parsing (tokens2form)

import System (system)
import Char (isDigit)
import IO

-- AR 18/4/2000

data CommandLine = OneC CommandOpt | PipeC CommandOpt CommandArg [CommandOpt]
type CommandOpt = (Command,[Option])

execLine :: Bool -> State Str -> CommandLine -> IO (State Str)
execLine _ st (OneC c@(_,opts)) = do
  (st',val) <- execC st c AUnit
  putStrLn $ prCommandArg st opts val
  return st'
execLine tr st (PipeC c@(_,opts) a cs) = do
  (st',val) <- execC st c a
  if tr then putStrLn (prCommandArg st opts val) else return ()
  execs st' cs val
 where
    execs _ _ (AError _) = return st
    execs st [] val = do 
      if tr then return () else putStrLn (prCommandArg st opts val)
      return st
    execs st (c:cs) val = execLine tr st (PipeC c val cs)

data CommandArg = 
  AError String | ATrm [Trm] | AStr [[Str]] | AString [String] | AUnit deriving Eq

prCommandArg :: State Str -> [Option] -> CommandArg -> String
prCommandArg st opts val = case val of
  AError s -> s
  AStr ss  -> unlines $ map showTok ss
  AString ss -> unlines ss
  ATrm tt  
    | oElem showHaskell opts -> unlines $ map show tt
    | oElem showOld opts     -> unlines $ map prOldTrm tt
    | oElem showXML opts     -> unlines $ map (prXTrm (abstractOfState st)) tt
    | otherwise -> unlines $ map prt tt
  AUnit    -> ""

data Command =
   CImport GrammarFile
 | CReadFile FilePath
 | CWriteFile FilePath
 | CAppendFile FilePath
 | CSpeakAloud String
 | CPutString
 | CTermCommand CommandId
 | CStringCommand CommandId
 | CParse GrammarId Cat
 | CLinearize GrammarId
 | CTranslate GrammarId GrammarId Cat
 | CMorphAnalyse GrammarId
 | CRandom Int Cat
 | CPrint GrammarId
 | CEmpty
 | CCFProperties Int GrammarId
 | CHelp
 | CVoid    -- empty command line
 | CUnknown -- no parse of command line
 | CQuit        -- execution of the last five is defined on GF top level
 | CPrintHistory
 | CExecHistory String
 | CTranslateSession GrammarId [GrammarId] Cat
 | CParseSession GrammarId Cat
 | CReduceParser GrammarId [Fun]
 | CEditSession
 | CDialogueSystem String Language String
 | CRandomQuestions Language Language Cat
 | CMorphoQuiz GrammarId Cat
 | CTranslationTraining Language Language Cat Int
 | CMorphologyTraining GrammarId Cat Int
 | CSystem

execC :: State Str -> (Command,[Option]) -> CommandArg -> IO (State Str, CommandArg)
execC st (comm, opts) arg = case (comm,arg) of
  (CTermCommand commid, ATrm tt) -> do
    let ab  = abstractOfState st
        tt' = concat $ map (termCommand ab commid . updateTerm ab) tt
    return (st, ATrm tt')
  (CStringCommand commid, AString ss) -> do
    let ss' = map (stringCommand commid) ss
    return (st, AString ss')
  (CImport file,AUnit) -> do
    let v = not (ooElem beSilent) -- -s = silent ; previously -v = verbose
        c = ooElem isCompiled
    gr  <- getGrammarFromFile opts file
    if ooElem showLatex
       then do writeFile (file ++ ".tex") (grammar2latex gr) 
               putStrLn ("Grammar written in" +++ file ++ ".tex")
       else return ()
    st' <- importJustGrammar v c (not (ooElem wholeGrammar)) st (file,gr)
    return (st',AUnit)

  (CReduceParser gr funs, AUnit) -> do
    let lang = getLanguage st gr
        st'  = reduceCfOfLanguage lang st funs
    return (st',AUnit)  --- can't write to file as AUnit


  (CReadFile file, AUnit) -> do
    s <- readFileIf file
    let ss  = if ooElem byWords then words s else 
	      if ooElem byLines then lines s else [s]
        val = if ooElem dontParse
                then (ATrm (concat (map (parseOptTrm opts) ss))) else AString ss
    return (st,val)
  (CWriteFile file, val) -> do
    writeFile file $ prCommandArg st opts val
    return (st,AUnit)
  (CAppendFile file, val) -> do
    appendFile file $ prCommandArg st opts val
    return (st,AUnit)
  (CSpeakAloud s, val) -> do
    system ("echo" +++ "\"" ++ prCommandArg st opts val ++ "\" | festival --tts" ++
            if null s then "" else " --language" +++ s)
    return (st,AUnit)
  (CPutString, AString ss) -> do
    return (st,arg)
  (CEmpty,AUnit) -> do
    return $ (emptyState,AUnit)
  (CParse gr cat, AStr ss) -> do
    tt <- mapM (parseWithOpts st opts gr cat . showTok) ss
    let tt' = concat tt
    if null tt' then putStrLn "No parse." else return ()
    return (st, ATrm tt')
  (CParse _ _, AString ss) -> 
    let a' = AStr (map readTok ss) in execC st (comm,opts) a'
  (CLinearize gr, ATrm tt) -> do
    let lang = getLanguage st gr
        grammar = grammarOfLanguage lang st
        ss = map (justLinearizeTerm' opts grammar)  tt
    return (st, AString (concat ss))
  (CTranslate gr1 gr2 cat, AStr ss) -> do
    tt <- mapM (parseWithOpts st opts gr1 cat . showTok) ss
    let tt' = concat tt
    if null tt' then putStrLn "No parse." else return ()
    let lang2 = getLanguage st gr2
        grammar = grammarOfLanguage lang2 st
    ss' <- return $ map (justLinearizeTerm grammar) tt'
    return (st, AString (concat ss'))
  (CTranslate _ _ _, AString ss) -> 
    let a' = AStr (map readTok ss) in execC st (comm,opts) a'
  (CMorphAnalyse gr, AStr ss) -> do
    let lang = getLanguage st gr
	grammar = grammarOfLanguage lang st
	cf = cfOfLanguage lang st
        mor = (if ooElem analMorpho 
	       then E.morphology else E.morphology1) (grammar,cf)
	tt = map (mor . showTok) ss
    if null tt then putStrLn "No parse." else return ()
    return (st, AString tt)
  (CMorphAnalyse _, AString ss) -> 
    let a' = AStr (map readTok ss) in execC st (comm,opts) a'
  (CRandom int cat, AUnit) -> do
    let abs = abstractOfState st
        cat' = updateIdent abs cat
    gen <- myStdGen int --- put a StdGen in the state
    let mx = 23 --- 23 = max ref steps
    let tt = take int $ mkRandomTerms gen mx abs cat' 
    if length tt < int then putStr ("max number of"+++ show mx +++ "tries exceeded")
                       else return ()
    return (st, ATrm tt)
  (CPrint gr, AUnit) -> do
    let lang = getLanguage st gr
        grammar = grammarOfLanguage lang st
        cf = cfOfLanguage lang st
        str 
          | ooElem showCF || ooElem (iOpt "c") -- option -c is obsolete
                               = prCF $ cf
          | ooElem showOptim   = prt $ st2grammar grammar
          | ooElem showAbstr   = prt $ st2abstract (abstractOfState st) 
          | ooElem showXML     = prXAbstractST (abstractOfState st)
          | ooElem showHaskell = show grammar
          | ooElem showLatex   = grammar2latex $ st2grammar grammar
          | ooElem showHsFile  = grammar2haskell $ st2grammar grammar
          | ooElem showWords   = unwords $ wordsOfLanguage lang st
          | ooElem showHsAbs   = 
                           mkGFCFile "Abs" "AbstractST" (show (abstractOfState st))
          | otherwise = prt $ st2grammar grammar ---
    return (st,AString [str])
  (CCFProperties int gr,AUnit) -> do
    let lang = getLanguage st gr
        cf = cfOfLanguage lang st
    showCFProperties cf int
    return (st,AUnit)  --- can't write to file as AUnit
  (CHelp, AUnit) -> do
    let str = txtHelpFile
        cust = txtCustomCommands
    return (st,AString [str, cust])
  (CQuit,AUnit) -> return (st,AUnit)
  (CTranslateSession _ _ _,AUnit) -> return (st,AUnit)
  (CParseSession _ _,AUnit) -> return (st,AUnit)
  (CEditSession, AUnit) -> return (st,AUnit)
  (CExecHistory _,AUnit) -> return (st,AUnit)
  (CPrintHistory,AUnit) -> return (st,AUnit) --- can't be saved with AUnit !
  (CRandomQuestions _ _ _,AUnit) -> return (st,AUnit)
  (CMorphoQuiz _ _, AUnit) -> return (st,AUnit)
  (CTranslationTraining ig og cat int,AUnit) -> do
     sts <- transTrainList st ig og cat int
     return $ (st, AString [s ++++ unlines ss | (s,ss) <- sts])
  (CMorphologyTraining gr cat int,AUnit) -> do
     sts <- morphoTrainList st gr cat int
     return $ (st, AString [unlines [s,p,t] | (s,p,t) <- sts])
  (CDialogueSystem _ _ _,AUnit) -> return (st,AUnit)
  (CSystem, AString ss) -> do
    mapM system ss
    return $ (st,AUnit)
  (CVoid,AUnit) -> return (st,AUnit)
  (CUnknown,AUnit) -> do putStrLn "unrecognized command" ; return (st,AUnit)
  _ -> do putStrLn "undefined or ill-typed command" ; return (st,AUnit)
 where
   ooElem = flip oElem opts

--parseWithOpts :: Token a => 
--            State a -> [Option] -> Maybe Language -> Ident -> String -> IO [Trm]
parseWithOpts st opts gr cat str = do
  let (ww,tt) = mkOptParser st opts gr cat str
  if null ww then return tt else do 
    putStrLn ("Unknown words in input:" +++ unwords ww)
    return []

parseOptTrm opts s
 | oElem showOld opts = take 1 (pOldTrm s)
 | otherwise = pTrm s : []

--

getGrammarFromFile :: [Option] -> String -> IO (Grammar [Str])
getGrammarFromFile opts file
 | oElem showEBNF opts || oElem (iOpt "e") opts -- option -e obsolete
                       = readEBNFFile v file
 | oElem showCF opts   || oElem (iOpt "c") opts -- option -c obsolete
                       = readCFFile v file
 | oElem showOld opts  || oElem (iOpt "o") opts -- option -o obsolete
                       = readOldGFFile v file
 | otherwise = readGrammarFile v file
     where v = not (oElem beSilent opts)


-- getting and parsing commands

getCommandLine :: [String] -> IO ((Bool,CommandLine), String)
getCommandLine hist = do
  putStr "\n> "
---  s <- getLine
  s <- catch getLine (\e -> if isEOFError e then return "q" else ioError e)
  let s' = if   s /= [] && all isDigit s
           then let n = read s in (if length hist > n then hist !! n else s) 
           else s 
  trco <- getParsedCommandLine s'
  return (trco,s')

getAndExecCommands :: State Str -> [String] -> IO (State Str)
getAndExecCommands env [] = return env
getAndExecCommands env (s:ss) = do
    (tr,cm) <- getParsedCommandLine s
    env'    <- execLine tr env cm
    getAndExecCommands env' ss

getParsedCommandLine s = 
  if all (==' ') s then return (False,(OneC (CVoid,[]))) else do
    let c = case parseResults pCommandLine s of 
              c:_ -> c
              _   -> (False,OneC (CUnknown,[]))
    return c

pCommandLine :: Parser Char (Bool,CommandLine)
pCommandLine = 
  (jL "trace" <<< True ||| succeed False) .>. (\tr ->
  pJ pCommand                             .>. (\c ->
  pJ pCommandArg                          .>. (\a ->
  ((jL "|" +.. pTList "|" pCommand) +|| succeed []) .>. (\cs ->
  succeed (if a == AUnit && null cs then (False,OneC c) else (tr,PipeC c a cs))))))

pCommand :: Parser Char (Command, [Option])
pCommand = 
  abbrlits "term" +.. nabbrlits "command" +.. pJ pOpts ... pJ pCommandId
    *** (\ (o,f) -> (CTermCommand f, o))
 |||
  abbrlits "string" +.. nabbrlits "operation" +.. pJ pOpts ... pJ pCommandId
    *** (\ (o,f) -> (CStringCommand f, o))
 |||
  nabbrlits "import" +.. pJ pOpts ... pJ pFileName 
    *** (\ (o,f) -> (CImport f, o))
 |||
  abbrlits "read" +.. nabbrlits "file" +.. pJ pOpts ... pJ pFileName 
    *** (\ (o,f) -> (CReadFile f, o))
 |||
  abbrlits "write" +.. nabbrlits "file" +.. pJ pOpts ... pJ pFileName 
    *** (\ (o,f) -> (CWriteFile f, o))
 |||
  abbrlits "append" +.. nabbrlits "file" +.. pJ pOpts ... pJ pFileName 
    *** (\ (o,f) -> (CAppendFile f, o))
 |||
  abbrlits "speak" +.. abbrlits "aloud" +.. pJ pOpts ... pJ pOptIdent
    *** (\ (o,s) -> (CSpeakAloud s, o))
 |||
  abbrlits "write" +.. abbrlits "string" +.. pJ pOpts 
    *** (\o -> (CPutString, o))
 |||
  abbrlits "linearize" +.. pJ pOpts ... pJ pGrammarId
    *** (\ (o,l1) -> (CLinearize l1, o))
 |||
  nabbrlits "parse" +.. pJ pOpts ... pJ pGrammarId ... pJ pCat
    *** (\ (o,(l1,cat)) -> (CParse l1 cat, o))
 |||
  nabbrlits "translate" +.. pJ pOpts ... pJ pLanguage ... pJ pLanguage ... pJ pCat
    *** (\ (o,(l1,(l2,cat))) -> (CTranslate l1 l2 cat, o))
 |||
  abbrlits "translate" +.. nabbrlits "session" +.. pJ pOpts ... 
  pJ (pLanguage ||| succeed Nothing) ... 
  pParenth (longestOfMany (pJ pLanguage)) ...
  pJ pCat
    *** (\ (o,(l1,(ls,cat))) -> (CTranslateSession l1 ls cat, o))
 |||
  abbrlits "morphological" +.. nabbrlits "analysis" +.. pJ pOpts ... 
  pJ pGrammarId
    *** (\ (o,l1) -> (CMorphAnalyse l1, o))
 |||
  abbrlits "random" +.. nabbrlits "questions" +.. pJ pOpts ... 
  pJ pGetLanguage ... pJ pGetLanguage ... pJ pCat
    *** (\ (o,(l1,(l2,cat))) -> (CRandomQuestions l1 l2 cat, o))
 |||
  abbrlits "morphological" +.. nabbrlits "questions" +.. pJ pOpts ... 
  pJ (pLanguage ||| succeed Nothing) ... pJ pCat
    *** (\ (o,(l1,cat)) -> (CMorphoQuiz l1 cat, o))
 |||
  abbrlits "translation" +.. nabbrlits "exercises" +.. pJ pOpts ... 
  pJ pGetLanguage ... pJ pGetLanguage ... pJ pCat ... pIntc
    *** (\ (o,(l1,(l2,(cat,int)))) -> (CTranslationTraining l1 l2 cat int, o))
 |||
  abbrlits "morphology" +.. nabbrlits "exercises" +.. pJ pOpts ... 
  pJ (pLanguage ||| succeed Nothing) ... pJ pCat ... pIntc
    *** (\ (o,(l1,(cat,int))) -> (CMorphologyTraining l1 cat int, o))
 |||
  abbrlits "dialogue" +.. nabbrlits "system" +.. pJ pOpts ... pJ pOptIdent ... 
  pJ pGetLanguage ... pJ pIdent
    *** (\ (o,(s,(l,cat))) -> (CDialogueSystem s l cat, o))
 |||
  abbrlits "parse" +.. nabbrlits "session" +.. pJ pOpts ... 
  pJ pGrammarId ... pJ pCat
    *** (\ (o,(l1,cat)) -> (CParseSession l1 cat, o))
 |||
  abbrlits "reduce" +.. nabbrlits "parser" +.. pJ pOpts ... 
  pJ pGrammarId ... longestOfSome (pJ pCat)
    *** (\ (o,(l1,funs)) -> (CReduceParser l1 funs, o))
 |||
  abbrlits "edit" +.. abbrlits "session" +.. pJ pOpts
    *** (\o -> (CEditSession, o))
 |||
  abbrlits "generate" +.. nabbrlits "random" +.. pJ pOpts ... 
  pJ pIntc ... pJ pCat
    *** (\ (o,(i,cat)) -> (CRandom i cat, o))
 |||
  abbrlits "print" +.. abbrlits "grammar" +.. pJ pOpts ... pJ pGrammarId
    *** (\ (o,l1) -> (CPrint l1, o))
 |||
  abbrlits "cf" +.. nabbrlits "properties" +.. pJ pOpts ... 
  pJ pIntc ... pJ pGrammarId
    *** (\ (o,(n,gr)) -> (CCFProperties n gr, o))
 |||
  abbrlits "print" +.. abbrlits "history" +.. pJ pOpts
    *** (\ o -> (CPrintHistory, o))
 |||
  abbrlits "execute" +.. nabbrlits "history" +.. pJ pOpts ... pJ pFileName 
    *** (\ (o,f) -> (CExecHistory f, o))
 |||
  abbrlits "empty" 
    <<< (CEmpty, []) 
 |||
  abbrlits "help" 
    <<< (CHelp, []) 
 |||
  abbrlits "quit"
    <<< (CQuit, []) 
 |||
  abbrlits "system" +.. abbrlits "call"
    *** (\ s -> (CSystem, []))

pCommandArg =
  pInputString *** (AString . (:[])) 
 |||
  pTerm *** (ATrm . (:[])) 
 +|| 
  succeed AUnit

pabbrlits (a:s) = literals (a:s) ||| literals [a]
abbrlits t = pJunk +.. pabbrlits t
nabbrlits t = pabbrlits t +.. literal ' ' -- forcing space

pGrammarId = jL "--" +.. pLanguage +|| succeed Nothing

pLanguage = pGetLanguage *** Just

pGetLanguage = pIdent *** Language

pInputString = pQuotedString

pInputTerm :: Parser Char Trm
pInputTerm = pTerm

pOptIdent = pIdent ||| succeed ""

pCat = pIdent *** zIdent

pCommandId = pIdent --- 


--- for some reason hbc wants me to repeat these derivations from State

instance HasStatus Bool where
 isInitial b = b
 notInitial _ = False
instance HasStatus b => HasStatus (b, a) where
 isInitial (b,_) = isInitial b
 notInitial (b,a) = (notInitial b,a)



