module Main where --M

-- a copy of ../cparser with modifs --M

import ParAlfa --M
import LayoutAlfa hiding (Position) --M
import qualified LexAlfa --M
import PrintAlfa --M
import ErrM --M
import CPUTime --M


import System(getArgs)
import System(getArgs)
import FString(StrTable)
import Lex(Token, lx)
import Parse(Parser,simpleParse,sParse,parse)
import CParser(CParser,pLetDefs,finalP)
import PreStrings(preStrTable)
import CSyntax(CLetDef)
import BasicOps(importDefs,addDefs)
import Monads(Error(..),runSTM,liftESTM)
import Position(Position(..), noPosition, filePosition)
import ProofState
import ProofMonad(PCM,getMetaInfo,putStrTable,getStrTable)


-- pLetDefs :: CParser [CLetDef]
                              
main = do
  path <- getArgs
  mapM_ (parseFile pLetDefs) path
  alfa <- getCPUTime
  mapM_ parseBNFC path --M
  beta <- getCPUTime
  putStr "old parser:"
  putStrLn (show (alfa `div` 1000000000) ++ " msec")
  putStr "new parser:"
  putStrLn (show ((beta-alfa) `div` 1000000000) ++ " msec")

--M begin
parseBNFC = runFile 0 pModule

type ParseFun a = [LexAlfa.Token] -> Err a  
myLLexer = resolveLayout True . myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = if v > 1 then putStrLn s else return ()

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do  putStrLn "\nParse              Failed...\n"
                           putStrV v $ show ts
                           putStrLn s
           Ok  tree -> do putStrLn "\nParse Successful!"
                          putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
                          putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

--M end




type Tokens = [Token]

lexer :: Position -> StrTable -> String -> Tokens
lexer (Position s l c) strTable input = lx False s l c strTable input 

pParse :: CParser a -> Position -> String -> PCM a
pParse p pos input
    = do
        st <- getStrTable
        (result, st') <- liftESTM $ finalP p $ lexer pos st input 
        putStrTable st'
        return result

parseFile parser path = do 
                          putStr (path++": ")
                          input <- readFile path
                          printPCM $  pParse parser (Position path 0 0) input


----------------- Printing ------------------

showE :: Show a => Either a b -> String
showE (Right _) = "Success!"
showE (Left e) = "Fail: " ++ show e

instance  Show (Error a) where
   show (Done _) = "Success!"
   show (Err e) = "Fail: " ++ show e

printPCM :: PCM a -> IO ()
printPCM pcm = print $ runSTM  pcm ProofState.initState


{-
module BParser where

import CParser (pLetDefs) -- pLetDefs :: CParser [CLetDef]
import Parse (ParseResult (..))


pCParser :: FilePath -> IO ()
pCParser file = parseFile pLetDefs path


parseFile parser path = do 
                          putStr (path++": ")
                          input <- readFile path
                          r <- pParse parser (Position path 0 0) input

-}
