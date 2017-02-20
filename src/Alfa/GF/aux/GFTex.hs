module Main where

import PGrammar (pGrammarStr, pLTermStr) --- the old grammar parser
import GrammarToLatex

import System
import List

-- convert tex files with verbatim GF into ones with typeset GF. AR 17/5/2001

-- if GF is a link to GF sources, execute the program by typing
--   runhugs -P.:GF: GFTex INFILE OUTFILE

main :: IO ()
main = do
  xx <- getArgs
  case xx of 
    f:f':_ -> do
      s <- readFile f
      let s' = mkGFTex (lines s)
      seq s' $ writeFile f' (unlines s')
    _ -> putStrLn helpMsg

-- main scanner
mkGFTex ss = case ss of
  s:ss2 | isPrefixOf begGF s  -> begQuot : mkGF  [] ss2
  s:ss2 | isPrefixOf begTGF s -> begMath : mkTGF [] ss2
  s:ss2                     -> s : mkGFTex ss2
  _                         -> ss

-- scanner of GF judgements
mkGF t ss = case ss of
  s:ss2 | isPrefixOf endVerb s -> gf2tex (unlines (reverse t)) :endQuot :mkGFTex ss2
  s:ss2                        -> mkGF (s:t) ss2
  _                            -> ss

-- scanner of GF terms
mkTGF t ss = case ss of
  s:ss2 | isPrefixOf endVerb s -> tgf2tex (unlines (reverse t)) :endMath :mkGFTex ss2
  s:ss2                        -> mkTGF (s:t) ss2
  _                            -> ss

-- parse a string as grammar and print it in Latex
gf2tex = gr2tex . snd . pGrammarStr

-- parse a string as term and print it in Latex
tgf2tex = trm2tex . pLTermStr

-- strings in Latex code
begGF   = "\\begGF"        -- sought
begTGF  = "\\begTGF"
endVerb = "\\end{verbatim}"

begMath = "\\[\\gfsize"  -- replacements
begQuot = "\\begin{quote}\\gfsize"
endMath = "\\normalsize\n\\]"
endQuot = "\\normalsize\n\\end{quote}"

helpMsg = 
 "\nThis program converts tex files into tex files.\n" ++
 "Lines between \\begGF and \\end{verbatim} are treated\n" ++ 
 "as sequences of GF judgements.\n" ++
 "Lines between \\begTGF and \\end{verbatim} are treated\n" ++ 
 "as GF terms.\n" ++
 "Two arguments are needed: infile and outfile.\n" ++
 "Look at the end of the source file to find the LaTeX macros that\n" ++
 "are needed in the infile (or at least in the outfile)."

-- macros you need in the Latex file; you can change their definitions
{-
\newcommand{\keyw}[1]{\mbox{{\bf #1 }}}   % keywords in bold
\newcommand{\gfsize}{\small}              % gf code is written in small
\newcommand{\syb}[1]{{\rm #1}}            % identifiers in romans
\newcommand{\str}[1]{{\em #1}}            % strings in italics (without quotes)
\newcommand{\kwtable}{\mbox{{\bf table}}} % the word table (sometimes nice to omit)
-}

