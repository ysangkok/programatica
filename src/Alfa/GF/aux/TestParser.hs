module TestParser where

import CF
import Operations
import Parsers
import PPrCF
import Tokens

import Koe -- the parser to be tested

--- test a context-free parser cfParser

cfParser :: (Token a, CFIdent c) => CF a c -> CFCat c -> Parser a (CFTree c)
cfParser = koeParser -- the parser imported

testCF :: String -> IO ()
testCF file = do
  s <- readFile file
  let cf = ([rule | Just rule <- map getCFRule $ lines s],const [])
  putStrLn $ prCF cf
  let cat = startCat cf
  let parser = unlines . map prCFTree . parseResults (cfParser cf cat) . 
                    map Str . words
  parseWith parser

parseWith p = do
  putStr "parse> "
  s <- getLine
  putStrLn $ p s
  parseWith p

