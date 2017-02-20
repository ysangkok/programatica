module Option where

import Parsers

-- all kinds of options

newtype Option = Opt String deriving Eq

iOpt  = Opt   -- to keep Option an abstract type

oElem :: Option -> [Option] -> Bool
oElem = elem  -- to transmit Eq

pOpts = longestOfMany (pJ pOption) where
  pOption = literal '-' +.. longestOfSome pAlphanum *** Opt

-- a suggestion for option names

-- parsing
strictParse  = iOpt "strict"
forgiveParse = iOpt "n"
ignoreParse  = iOpt "ign"
literalParse = iOpt "lit"
chartParse   = iOpt "chart"
earleyParse  = iOpt "ear"
topdownParse = iOpt "td"
topdownPars2 = iOpt "td2"
parsekParse  = iOpt "pp"
pebParse     = iOpt "peb"
rawParse     = iOpt "raw"
firstParse   = iOpt "1"
dontParse    = iOpt "read" -- parse as term instead of string
defaultParseOpts = [strictParse,chartParse]

-- grammar formats
showAbstr   = iOpt "abs"
showHaskell = iOpt "hs"
showHsAbs   = iOpt "hsa"
showHsFile  = iOpt "hsf"
showXML     = iOpt "xml"
showOld     = iOpt "old"
showLatex   = iOpt "latex"
showEBNF    = iOpt "ebnf"
showCF      = iOpt "cf"
showWords   = iOpt "ws"
showOptim   = iOpt "opt"
isCompiled  = iOpt "gfc"
defaultGrOpts = []

-- linearization
allLin      = iOpt "all"
firstLin    = iOpt "one"
distinctLin = iOpt "nub"
dontLin     = iOpt "show"
xmlLin      = showXML
latexLin    = showLatex
tableLin    = iOpt "table"
defaultLinOpts = [firstLin]

-- other
beVerbose    = iOpt "v"
beSilent     = iOpt "s"
wholeGrammar = iOpt "w" -- no minimization
makeFudget   = iOpt "f"
byLines      = iOpt "lines"
byWords      = iOpt "words"
analMorpho   = iOpt "morpho"

-- mainly for stand-alone
useUnicode    = iOpt "uni"
optCompute    = iOpt "compute"
optCheck      = iOpt "check"
optParaphrase = iOpt "para"

-- for edit session
allLangs = iOpt "All"
absView  = iOpt "Abs"
