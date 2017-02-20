module Main where

import Operations ((++++))
import AlfaGF
import Types
import Grammar
import Chart
import PAgda (pAExp)
import PrTypes (prTerm)
import A2GSyntax (GGf(ggf, lin))
import System(getArgs)
import Parsers()
import AuxParsing()

-- to test the Agda/Alfa-GF interface. AR 15/10/1999 -- 17/12

main =
  do arg:_ <- getArgs
     testAG arg

testAG :: String -> IO ()
testAG a =
 do ((gr,cf),tt,fi) <- getATheory a
    putStrLn fi
    writeFile (a ++ ".gf") fi   
    putStr "\n\n"
    putStrLn (show tt)
    putStr "\n\n"
    putStrLn (prpr tt)
    testLoop (gr,cf)

testLoop :: (Grammar,CfGrammar) -> IO ()
testLoop grcf =
 do putStr "Exp> "
    s <- getLine
    case pAGCommand s of
      CQuit -> putStr "Bye"
      c     -> do { exec grcf c ; testLoop grcf }

data AGCommand = CQuit | CPExpr String | CPAlfa String | CPVoid

exec :: (Grammar,CfGrammar) -> AGCommand -> IO ()
exec env@(gr,cf) c = 
 case c of
   CPAlfa s -> 
     do let t = case pAExp s of
                  (x,_):_ -> prTerm (ggf gr x) ++++ prpr (prprA (lin gr x))
                  _       -> "no parse"
        putStrLn t
   CPExpr s -> 
     do let ee = parseC env (Cat "Expr",[],[],0) s
        putStrLn (unlines (map prTerm ee))
   _ -> return ()

pAGCommand :: String -> AGCommand
pAGCommand s =
 case s of
   'q':_ -> CQuit
   'p':' ':s -> CPExpr s
   _   -> CPAlfa s ---

