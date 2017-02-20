module Standalone where

import System (getArgs)
import ExportGF
import Option --- hiding (Opt)  -- not supported by hbc

-- AR 2/9/2000

-- the standalone GF program: translator that reads two grammars
-- from files and translates from standard input to standard output

batchTranslate opts cat f1 f2 = do
  g1 <- getGrammarCustom opts f1
  g2 <- if f1==f2 then return g1 else getGrammarCustom opts f2
  ifUnicode opts (unlines . concat . ifLines opts (manip opts cat g1 g2))

ifUnicode opts op
 | oElem useUnicode opts = getContents >>= showUnicode . op
 | otherwise = interact op

ifLines opts op
 | oElem byWords opts = concat . map op . words
 | oElem byLines opts = concat . map op . lines
 | otherwise     = op 

manip opts cat g1 g2 = 
  if   oElem analMorpho opts 
  then let mor = morphology g1 in (\x -> [[x]]) . mor  
  else
    explain . map (linearizeCustom opts g2) . oper g1 . parseCustom opts cat g1
      where
        oper g
          | oElem optCompute opts = map (compute g)
          | oElem optCheck opts   = concat . map (typeCheck g)
          | oElem optParaphrase opts = concat . map (paraphrase g)
          | otherwise = id
        --- composing these would require we specify their order...
        explain [] = [["FAILURE"]]
        explain ss = ss

readOpts = concat . map readOpt where
  readOpt o = case o of
    "parse" -> [dontLin]
    "lin"   -> [dontParse]
    "translate" -> [] -- because gf translates by default
    _ -> [iOpt o]

usageMsg = 
  "usage: " ++ synopsisMsg

synopsisMsg = 
  "'gf' ('-H'heapsize)? (option* cat ingrammar outgrammar? | '+help')?"

helpMsg =
  "The program gf either opens an interactive GF session or\n" ++
  "translates from standard input into standard output. Synopsis:\n\n  " ++
  synopsisMsg ++ "\n\n" ++
  "The arguments are a category name and two GF grammar file names.\n" ++
  "If only one file name is given, the same grammar is used for in- and output.\n"++
  "The command line may contain one or more of the following options:\n\n" ++
  optionMsg

optionMsg = unlines (map prOne allOptions) ++ "\n"  
  where prOne (o,e) = "  +" ++ o ++ " = " ++ e

allOptions = [
  ("n",    "sloppy parsing: forgive parameter errors"),
  ("ign",  "free parsing: ignore unknown words"),
  ("td",   "top-down parsing"),
  ("read", "don't parse (read as GF term instead)"),
  ("all",  "all linearizations"),
  ("nub",  "all distinct linearizations"),
  ("show", "don't linearize (show tree instead)"),
  ("xml",  "show parse tree as XML object)"),
  ("v",    "be verbose (give information on grammars)"),
  ("lines","parse input line by line"),
  ("words","parse input word by word"),
  ("w",    "whole grammar (don't minimize by removing oper)"),
  ("table","show whole inflection table with morphological parameters"),
  ("uni",  "show unicode output in a separate window (hbc only)"),
  ("parse","show parse trees (same as +show)"),
  ("lin",  "linearize input term (same as +read)"),
  ("morpho", "perform morphological analysis on input"),
  ("translate", "translate (same as no options)"),
  ("compute", "compute the parse results into normal form"),
  ("check",   "type check the parse results"),
  ("para",    "generate paraphrases of the parse results")
 ]

