module Sessions where

import Operations
import Parsers
import Tokens
import Grammar
import SymbolTable
import State
import Commands
import IOGrammar
import PrGrammar
import EditSession (defOptsEnv)
import EditSessionL
import Option
import Parsing (remComm)
import Arch
import List (nub)

-- AR 20/4/2000

translateSession :: 
  State Str -> [Option] -> GrammarId -> [GrammarId] -> Cat -> IO () -> IO ()
translateSession st opts gr1 grs cat resume = do
    let parser = case gr1 of
           Nothing -> anyLangParser st opts cat
           _       -> mkOptParser st opts gr1 cat
        langs = if null grs then allLanguages st else map (getLanguage st) grs
        grammars = map (flip grammarOfLanguage st) langs
    translateLoop opts parser grammars resume

translateLoop opts parser grammars resume | elem (Opt "f") opts = do -- -f fudget
  let trans = translates (snd . parser) grammars
      font = optFont opts
  fudlogueWrite font trans Nothing
translateLoop opts parser grammars resume = do
  putStr "\ntrans> "
  s <- getLine
  treatLine s resume (\s' -> do
    let (ww,tt) = parser s'
    if null ww 
       then if null tt then putStrLn "Np parse."
            else mapM_ (\g -> mapM_ (linearizeTerm g) tt) grammars
       else putStrLn ("Unknown words in input:" +++ unwords ww)
    translateLoop opts parser grammars resume)

parseSession :: State Str -> [Option] -> GrammarId -> Cat -> IO () -> IO ()
parseSession st opts gr cat resume = do
    let parser = parseWithOpts st opts gr cat
    parseLoop parser resume

parseLoop parser resume = do
  putStr "\nparse> "
  s <- getLine
  treatLine s resume (\s' -> do
    tt <- parser s'
    putStrLn $ if null tt then "Np parse." else unlines (map prt tt)
    parseLoop parser resume)

treatLine s resume loop = case s of
  "." -> resume
  '<':file -> do s' <- readFileIf file ; loop (remComm s')
  _ -> loop s

editSession :: State Str -> [Option] -> IO () -> IO ()
editSession st opts resume 
  | oElem makeFudget opts = fudlogueEdit font (st,opts') resume
  | otherwise             = gfEdit st opts' resume
 where
   opts' = nub (filter (/=makeFudget) opts ++ defOptsEnv st)
   font  = optFont opts

optFont opts = if elem (Opt "asc") opts then myAscFont else myUniFont
