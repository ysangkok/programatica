module Parsing where

import Operations
import Tokens
import Grammar
import SymbolTable
import PrGrammar
import CF
import Macros
import Update
import Linearize
import GFCFIdents
import GFtoCF
import Option --- hiding (Opt) -- not supported by hbc
import Tokenize
import LexGrammar (remComments)

-- the cf parsing methods
import Chart -- or some other CF Parser
import Earley -- such as this one
import TopDown -- or this...
import TopDown2 -- or this...
import ParsekTopDown -- or this... 
import PebParser -- or this...

import Profile

import List (nub, nubBy)

-- AR 26/1/2000 -- 8/4 -- 28/1/2001

-- default: nonstrict parsing in empty metavariable environment
tokens2trms abs = tokens2trms' False (abs,emptyConcreteST)

-- returns the list of updated complete parses terms resulting from complete parses
-- flag True = correct wrt linearization

tokens2trms' :: 
  Token a => Bool-> GrammarST [a] -> WParser a GFCFTree -> [a] -> [Trm]
tokens2trms' = tokens2trms'' []

tokens2trms'' :: Token a => 
  [(Cat,Int)] -> Bool-> GrammarST [a] -> WParser a GFCFTree -> [a] -> [Trm]
tokens2trms'' metas False (abs,_) parser as =
  [updateTerm abs t | Ok t <- map (cf2trm' metas) (nub (wParseResults parser as))]
tokens2trms'' metas True gr@(abs,cnc) parser toks =  
  [t | t <- tokens2trms'' metas False gr parser toks, isLinOf t toks] where
    isLinOf t tt = case allLinearizes t gr of
                     Ok ss -> any (==tt) ss
                     Bad s -> False

-- to get morphological analysis
tokens2form :: 
  Token a => GrammarST [a] -> GFCF a -> GFCFCat -> [a] -> [String]
tokens2form gr cf cat toks =
  [prt t ++ "," +++ unwords ps | 
            t <- tokens2trms'' [] False gr parser toks, ps <- linAs t toks] where
    linAs t tt = [ps | Ok l <- [allLinearizes' t gr], (ps,s) <- l, s == tt]
    parser = chartParser cf cat

-- no postprocessing
rawParser :: WParser a GFCFTree -> [a] -> [Trm]
rawParser parser = map cf2trm0 . nub . wParseResults parser

-- postprocessing context-free parse trees

cf2trm :: GFCFTree -> Err Trm
cf2trm = postParse

cf2trm' _ = postParse -- ?

-- the first approach : so simple it would be if it were not for permutations etc.
cf2trm0 :: GFCFTree -> Trm
cf2trm0 (CFTree (fun, (_, trees))) = mkTerm ([], cffun2trm fun, map cf2trm0 trees)

cffun2trm :: GFCFFun -> Trm
cffun2trm (GFCFFun ((fun,_),_)) = fun


-- the CF parsing method

parserMethod :: (Ord a, Token a) => 
  [Option] -> GrammarST [a] -> GFCF a -> GFCFCat -> WParser a Trm
parserMethod opts gr cf cat = 
 let stri = not $ oelem forgiveParse
     parser 
        | oelem chartParse   = chartParser 
        | oelem topdownParse = topdownParser 
        | oelem parsekParse  = parsekParser 
        | oelem topdownPars2 = topdownParser2
        | oelem pebParse     = pebParser
     -- insert your own parser here !
        | otherwise  = earleyParser
     pars s 
        | oelem rawParse = map (\t -> (t,[])) $ rawParser (parser cf cat) s
        | otherwise = [(t,[]) | t <- tokens2trms' stri gr (parser cf cat) s]
     oelem o = oElem o opts
  in pars

-- mkOptParserOne :: 
--   State Str -> [Option] -> GrammarId -> Ident -> String -> ([String],[Trm]) 
mkOptParserOne opts parser liter tokzer str =
    let Tokenizer tokid ww = tokzer
        tokz
           | oElem literalParse opts =      -- unknown words as string literals 
               restoreStringBrack ww . tokens tokzer
           | otherwise = 
               tokens tokzer
        kword 
           | take 4 tokid == "free" =     --- hard-wired tokzer name !
               const True
           | oElem literalParse opts =
               const True
           | otherwise = 
               \x -> elem x ww || not (null (liter x))
        input0 = tokz (remComm str)   --- hard-wired comments !
        (uww,input) =
           if   oElem ignoreParse opts       -- forgive unknown words
           then ([], filter kword input0) 
           else (filter (not . kword) input0, input0)
        tt = wParseResults parser (strings2toklist input)
    in (uww, if oElem firstParse opts then take 1 tt else tt)

remComm :: String -> String
remComm = remComments -- line tails beginning with --

strings2toklist :: Token a => [String] -> a
strings2toklist s = foldr plusTok zeroTok (map readTok s)

string2toklist :: Token a => String -> a
string2toklist = strings2toklist . words
