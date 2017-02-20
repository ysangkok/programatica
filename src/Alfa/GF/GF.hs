module Main where

import Operations
import Tokens (Str)
import IOGrammar ()
import SymbolTable ()
import Commands
import System (getArgs)
import Sessions (translateSession, parseSession,editSession)
import Randomized
import TeachYourself
import Dialogue (dialogueSession)
import State
import Standalone
import ExportGF ()
import Arch


-- AR 19/4/2000

main = gfMain

type CEnv = State Str

type CHEnv = (CEnv,[String],Integer) -- CEnv with history and CPU time

emptyCHEnv :: CHEnv
emptyCHEnv = (emptyState, [], 0)

gfMain :: IO ()
gfMain = do
  xx <- getArgs
  if elem "+help" xx then putStrLn (helpMsg ++ authorMsg) else 
    if xx == [] then gfSession else do
      let (opts,args0) = ([o | '+':o <- xx],[x | x <- xx, head x /='+'])
      if length args0 < 2 
         then putStrLn usageMsg 
         else let args = init args0 ++ repeat (last args0) in
              batchTranslate (readOpts opts) (args !!0) (args !! 1) (args !! 2)
--- TODO: certain options make some of the arguments needless

gfSession :: IO ()
gfSession = do
  putStr welcomeMsg
  gfInteract emptyCHEnv

gfInteract :: CHEnv -> IO ()
gfInteract henv@(env,hist,cpu) = do
 ((tr,c),line) <- getCommandLine hist
 env'   <- execLine tr env c
 cpu'   <- prCPU cpu
 (case c of 
    OneC (CQuit,_) -> putStr (let ct = cpu' `div` 1000000000 in
                               if ct == 0 then "\n\n" else
                               "\nYour GF session consumed " ++ show ct ++ 
                               " msec of CPU time.\n\n")
    OneC (CPrintHistory,_)  -> let prhist = prHistory hist in
                      do putStr ('\n':prhist)
                         gfInteract (env', line:hist, cpu')
    OneC (CExecHistory h,_) -> 
                      do putStrLn ""
                         ss    <- readFileIf h
                         env'' <- getAndExecCommands env (lines ss)
                         cpu'' <- prCPU cpu'
                         gfInteract (env'', line:hist, cpu'')
    OneC (CTranslateSession g gs cat,os) -> 
        translateSession env os g gs cat (gfInteract (env, line:hist, cpu'))
    OneC (CParseSession g cat,os) -> 
        parseSession env os g cat (gfInteract (env, line:hist, cpu'))
    OneC (CEditSession,os) -> 
        editSession env os (gfInteract (env, line:hist, cpu'))
    OneC (CRandomQuestions ig og cat,os) ->
        teachTranslation env ig og cat (gfInteract (env, line:hist, cpu'))
    OneC (CMorphoQuiz ig cat,os) -> do
        teachMorpho env ig cat (gfInteract (env, line:hist, cpu'))
    OneC (CDialogueSystem sp g cat,os) -> 
        dialogueSession env sp g cat (gfInteract (env, line:hist, cpu'))
    _              -> gfInteract (env', line:hist, cpu'))
  
gfBatch :: String -> IO ()
gfBatch file = 
 do s <- readFile file 
    e <- getAndExecCommands emptyState (lines s)
---    gfInteract (e,[],0)
    putStr "\nGoodbye.\n"

prHistory hist = unlines (reverse (filter (/="") hist))

welcomeMsg = 
  "Welcome to " ++ authorMsg ++++ welcomeArch ++ "\n\nType 'h' for help."

authorMsg = 
  "Grammatical Framework, test version 0.976, September 24, 2001.\n" ++ 
  "Copyright (c) Aarne Ranta 1998-2001, under GNU General Public License (GPL).\n"++
  "Bug reports to aarne@cs.chalmers.se"

