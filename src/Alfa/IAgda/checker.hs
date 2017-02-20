-- | Top level Agda module for batch checking
module Main where
import Commands        (addDefsCommand,solveAll1)
import CSyntax         (MetaVar)
import Monads          (Error(..),StateM(..))
import ProofMonad      (PCM, getUninstantiated)
import ProofState      (State, initState)
import LoadPrelude     (loadPrelude)
import BasicOps        (includePrelude)
import Position        (Position(Position))

import System          (getArgs)
import System.Exit     (ExitCode(..),exitWith,exitFailure)

main :: IO ()
main = do
    files <- getArgs
    case files of
        [] -> putStr "Usage: checker file ..." >> exitFailure
        _  -> do
                inputs <- mapM readFile files
                startState <- mayLoadPrelude ProofState.initState  
                checkFiles (zip files inputs) startState
                exitWith ExitSuccess

-- includePrelude :: PCM () = StateM State ()
mayLoadPrelude :: State -> IO State
mayLoadPrelude s | loadPrelude = case funSTM includePrelude s of
  Done (_a,s') -> return s'
  Err  err     -> putStr ("Error loading prelude "++ show err) >> exitFailure
                 | otherwise   = return s
          
type FileContent = String

checkFiles :: [(FilePath, FileContent)] -> State -> IO State
checkFiles []                  st = return st
checkFiles ((path,input):rest) st = do
    putStr (path++": ")
    st' <- ioPCM (checkString path input) st 
    checkFiles rest st'

-- checkPath :: FilePath -> IO ([MetaVar], [(MetaVar, String, [MetaVar])])
checkPath path
 = do
     contents <- readFile path
     startState <- mayLoadPrelude ProofState.initState  
     let Done (res, s') =  funSTM (checkString path contents>>getUninstantiated) startState
     print res

     
     

checkString :: FilePath -> String -> PCM ([MetaVar], [(MetaVar, String, [MetaVar])])
checkString path input = addDefsCommand (Position path 1 0) input

{-

041006: PJ trying to understand what checkString returns 
ghci -cpp -O -package lang checker.hs
let pcm = checkString "q.agda" "a::Set= ?" 
let res = funSTM pcm initState
let Done (a1,s) = res

fst a1 is [0]  (that is, I guess, there is one MetaVar in the file, with number 0)

let pcm = checkString "q.agda" "a::Set= ?\nb::Set= ?" 
let res = funSTM pcm initState
let Done (a2,s) = res

fst a2 is [0,1]  (there are two MetaVars in the file, numbered 0 and 1)

In both cases snd ai is [].

-}



-- | ioPCM checks for remaining meta variables, transforms the
--   error constructor into exitFailure or passes on the changed
--   state
ioPCM :: PCM ([a], t) -> State -> IO State
ioPCM pcm s = case funSTM (pcm>>solveAll1>>getUninstantiated) s of
   (Done ([],s')) -> success       >> return s'
   (Done (mvs,s)) -> openproof mvs >> exitFailure
   (Err e)             -> failure   e   >> exitFailure 
  where success       = putStrLn "Success!" 
        openproof mvs = putStrLn ("Not closed: " ++ show (length mvs) ++
                                  " meta variable(s) remain")
        failure   e   = putStrLn ("Fail: " ++ show e)

instance  Show (Error a) where
   show (Done _) = "Success!"
   show (Err e)  = "Fail: " ++ show e


{-
checkFiles :: [FilePath] -> [String] -> PCM ()
checkFiles [] _ = return ()
checkFiles (path:paths) (input:inputs) =  
              -- putStr (path++": ")
              checkString path input >> checkFiles paths inputs
-}

{- Now about PCM:

type PCM a  = StateM State a 
Monads.runSTM :: StateM s a -> s -> Error a
ProofState.initState :: State

hence runSTM pcm initState :: Error a where stm :: PCM a
-- Destructor: funSTM :: StateM s a -> s -> Error(a,s)
-- funSTM :: StateM s a -> s -> Error(a,s)

liftESTM :: Error a -> StateM s a
liftESTM e :: PCM a if e :: Error a 


newtype IM a = IM ([State] -> IO(Error(a,([State]))))
-}

