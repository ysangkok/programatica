module CustomFormats where

import Operations
import Grammar
import Tokens
import Macros
import Parsers
import PGrammar
import EBNF
import List
--- import DbParse

newtype GrammarOption = GOpt String deriving (Show,Eq)

pGrammarFormat :: GrammarOption -> Parser Char ([FilePath], Grammar [Str])
pGrammarFormat opt = case opt of
  GOpt "ebnf" -> (pEBNF ***  \x -> ([],ebnf2gf id x)) .<. remComments
---  GOpt "gfdb" -> (pGFDataBase ***  \x -> ([],db2gf x)) . remComments
-- ADD YOUR OWN GRAMMAR PARSER HERE
  _           -> pGrammar .<. remComments

readGrammarFormatFile' :: [FilePath] -> Bool -> String -> IO (Grammar [Str])
readGrammarFormatFile' excludes v file = do
   s <- readFileIf file
   files <- getImports file
   gg    <- mapM readOne (filter (flip notElem excludes) (nub files))
   return (unionGrammars gg)
  where
   getImports f = do
     s <- readFileIf f
     let o = getGrammarOption file
     let imps = case parses (pGrammarFormat o) s of
                  ((ii,_),_) : _ -> map (initFilePath f ++) ii
                  _ -> []
     ff <- mapM getImports imps
     return (nub (concat ff ++ [f]))
   readOne f = do
     let o = getGrammarOption f
     s <- readFileIf f
     let ((_,gr),msg) = case parses (pGrammarFormat o) s of
           (ig,[]):_ -> (ig, "whole grammar" +++ f +++ "accepted")
           (ig,rs):_ -> (ig, "grammar" +++ f +++ "rejected from" +++ take 111 rs)
           []        -> (([],emptyGrammar), "whole grammar" +++ f +++ "rejected")
     putStrLnV v msg
     return gr

getGrammarOption :: FilePath -> GrammarOption
getGrammarOption name 
  | elem '.' eman = GOpt $ reverse $ takeWhile (/='.') eman
  | otherwise = GOpt "gf"
 where eman = reverse name

putStrLnV v s = if v then putStrLn s else return ()
