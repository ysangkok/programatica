module MyTestAgda where 

import LexAgda
import ParAgda
import SkelAgda
import PrintAgda
import AbsAgda
import AgdaLayout
import ErrM

type ParseFun a = [Token] -> Err a

myLLexer = resolveLayout True . myLexer

runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO()
runFile p f = putStr (f++": ") >> readFile f >>= run p

run :: (Print a, Show a) => ParseFun a -> String -> IO()
run p s = case (p (myLLexer s)) of
           Bad s    -> do  putStrLn "Parse Failed...\n"
                           putStrLn s
           Ok  tree -> do putStrLn "Parse Successful!"

