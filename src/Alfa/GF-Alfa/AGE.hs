module Main where

import UseGF
import qualified UAbstract as A

import AlfaToGrammar
import qualified PATheory as P
import PAgda
import A2GSyntax
import G2ASyntax
import Tokenize (Tokenizer (..), untokens)
import qualified GSyntax as G
import qualified GFforAlfa as F
import AlfaToks

import List
import System
-- AR 6/3/2000 -- 25/3

main = do
  langs <- getArgs
  env <- F.initGF langs []
  getGrammars env

getGrammars env = do
  putStr "give alfa file : "
  file <- getLine
  testAG env file
  getGrammars env

type AlfaGrammars = F.AlfaGFPluginState

testAG :: F.AlfaGFPluginState -> String -> IO ()
testAG core@((abs0,cncs0),_) myfile = do
  P.testPAATheory myfile ---
  src <- readFileIf myfile
  let (_,(theory, annots0)) = case P.pAATheory src of
         (p,_):_ -> p
         _ -> ([],([],[]))
      langs = map fst cncs0 
      st0 lan = F.grammarOfLang lan (fst core)
      abs1 = atheory2abstract theory ---
      annotss0 = [(lan, [ann | (l,ann) <- annots0, l==lan]) | lan <- langs] 
      annotss  = [(lan, as ++ addDefaultAnnots abs1 as) | (lan,as) <- annotss0]
      grs = [(lan,aatheory2grammar (theory,annots)) | (lan,annots) <- annotss]
  putStrLn $ foldr (+++++) "" [P.langName lan ++++ prt gr | (lan,gr) <- grs] --- 
  let abs  = fst (a2gG (abs0,(NT,0)) (theory,[]))
      cncs = [(lan, mkGr lan gr) | (lan,gr) <- grs] 
      mkGr lan gr      = let Grammar (_,cnc) = gr
                             cf  = ([],const []) ---F.cfOfLang lan core
                             gr' = plainUpdOptGrammarST (st0 lan) gr
                             cf' = case grammar2cf gr' cnc of
                                      Ok (c,_) -> (fst c ++ fst cf, snd cf)
                                      _    -> cf
                         in (snd gr',cf')
      sts  = (abs,cncs) 
      defs  = map (ggf abs) theory
  putStrLn (unlines (map prt defs))
  putStrLn (unlines 
             (map (\g -> unlines (map (linAlfa g) defs)) [(abs,cnc) | (_,(cnc,_)) <- cncs]))
---  mapM_ (\g -> mapM (linearizeTerm g) defs) [(abs,cnc) | (_,(cnc,_)) <- cncs]

---linAlfa :: AG.GGf t => GrammarAST -> t -> AText
linAlfa gr@(abs,cnc) t = case lin t of
  Ok ss -> unlines [untokens (Tokenizer (symid tok) []) (map showTok s) | s <- ss]
  Bad s -> s
 where 
  tok = lookupTokenizer cnc
  lin t = do 
    let t' = updateTerm abs t ---abs (ggf abs t)
    allLinearizes t' gr


