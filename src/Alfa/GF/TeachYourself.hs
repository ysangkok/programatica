module TeachYourself where

import Operations
import Tokens
import Grammar
import SymbolTable ()
import Parsing
import Linearize
import IOGrammar
import State (getLanguage)
import Randomized
import Random
import Update
import Option
import qualified ExportGF as E
import Arch (myStdGen)
import System

-- 10/5/2000

teachTranslation st ig og cat resume = do
  tts <- transTrainList st ig og cat infinity
  let qas = [ (q, mkAnswer as) | (q,as) <- tts]
  teachDialogue qas "Welcome to a GF Translation Quiz." resume
 where
   mkAnswer as s = if any (unwords (words s) ==) as 
                     then (1,"Yes.") else (0,"No," ++++ unlines as)

teachMorpho st gr cat resume = do
  tts <- morphoTrainList st gr cat infinity
  let qas = [ (basic ++ "," +++ par, mkAnswer infl) | (basic,par,infl) <- tts]
  teachDialogue qas "Welcome to a GF Morphology Quiz." resume
 where
   mkAnswer infl s = if unwords (words s) == infl 
                       then (1,"Yes.") else (0,"No," ++++ infl)

type QuestionsAndAnswers = [(String, String -> (Integer,String))]

teachDialogue :: QuestionsAndAnswers -> String -> IO () -> IO ()
teachDialogue qas welc resume = do
  putStrLn welc
  putStrLn genericTeachWelcome
  teach (0,0) qas
 where 
   teach _ [] = do putStrLn "Sorry, ran out of problems" ; resume
   teach (score,total) ((question,grade):quas) = do
    putStr ("\n" ++ question ++ "\n> ") 
    answer <- getLine
    if answer == "." then resume else do
      let (result, feedback) = grade answer
          score' = score + result 
          total' = total + 1
      putStr (feedback ++++ "Score" +++ show score' ++ "/" ++ show total')
      if (total' > 9 && fromInteger score' / fromInteger total' >= 0.75)
             then do putStr "\nCongratulations - you passed!\n" ; resume
             else teach (score',total') quas

transTrainList st ig og cat0 number = do
  ts <- E.generateRandom src cat number
  return $ map mkOne $ take number ts
 where
   mkOne t = (E.linearize src t, map (E.linearize targ) (E.homonyms src cat t))
   (src,targ) = (getEGrammarOfLanguage st ig, getEGrammarOfLanguage st og)
   cat = E.mkExportCat cat0

morphoTrainList st gr cat0 int = do
  ts  <- E.generateRandom src cat int
  let pars = length $ E.linearizeCustom [tableLin, allLin] src $ head ts ---
  gen <- myStdGen int
  let is = randomRs (0,pars-1) gen
  return $ map mkOne $ take int (zip ts is)
 where 
   mkOne (t,i) = (basic, par, inflected)
     where
       tt = E.linearizeCustom [tableLin, allLin] src t
       basic = snd $ analyse $ head tt  --- should be done in Linearize, not Export
       (par,inflected) = analyse (tt !! i)
       analyse s = let (s1,s2) = span (/= ':') s in (s1, drop 2 s2)
   src = getEGrammarOfLanguage st (getLanguage st gr)
   cat = E.mkExportCat cat0


genericTeachWelcome = 
 "The quiz is over when you have done at least 10 examples" ++++
 "with at least 75 % success." +++++
 "You can interrupt the quiz by entering a line consisting of a dot ('.').\n"

infinity :: Int
infinity = 123 ---

--- this is awful ; due to discrepancy between State and ExportGF
getEGrammarOfLanguage st lang =
  let grammar@(a,c) = grammarOfLanguage lang st
      cf = cfOfLanguage lang st
  in E.mkExportGrammar a c cf

