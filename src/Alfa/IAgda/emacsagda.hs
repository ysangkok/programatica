{- 
#include "config.h" 
-}
-- | Top level Agda module for using the emacs front-end
module Main(main) where
import Parse_com       (Command(..), getCommand, isQuit)
import Commands  -- semantic actions for the most commands
import InteractionMonad(IM, runIM, undoToIM, liftIOIM, liftEIM,
                        accessPCMIM, liftPCMIM, readPCMIM, nrOfState,
                        handle, raise, when)
import ProofMonad      (PCM, toggleTraceFl)
import ProofState      (initState)
import LoadPrelude     (loadPrelude)
import BasicOps        (includePrelude)
import MetaVars        (MetaVar)
import Position        (Position(Position), noPosition)
import Error           (eMsg, EMsg, ErrMsg(ENoFile), prEMsg)
import Version         (version, compiled)
#ifdef PLUGINS
import Plugin          (PluginCall(..))
import PluginTable     (pluginAction)
#endif
#ifdef DYNAMIC_PLUGINS
import DynamicPlugin
#endif
import IO              (Handle, hGetLine, stdin, hSetBuffering, 
                        stdout, BufferMode(NoBuffering))
import System.IO.Error(IOError,catch,isEOFError)
--import CITrans
--import FString(StrTable)
--import List(intersperse)
--import Utilities
--import CITrans()
--import PPrint(PDetail (..),ppDebug,ppReadable)
--import AgdaTrace
--import Monads(traceM)
 
versionCommand :: String
versionCommand = infoCommand "* Agda version *" (version++"\n\n"++compiled)

-- errorBuffer = "*error*"
-- environmentBuffer = "* Environment *"

constraintBuffer, contextBuffer, goalBuffer, resultBuffer,
  suggestionsBuffer, terminationBuffer, typeBuffer :: String

constraintBuffer  = "* Constraints *"
contextBuffer     = "* Context  *"
goalBuffer        = "* Goals *"
resultBuffer      = "* Result *"
suggestionsBuffer = "* Suggestions *"
terminationBuffer = "* Termination *"
typeBuffer        = "* Type *"

mkString :: String -> String
mkString s = show s

inPar :: String -> String
inPar s = " (" ++ s ++ ")"

mkEmacsCommand :: String -> String
mkEmacsCommand s = "--- " ++ s ++ " +++\n"

mkCommand :: String -> String -> String
mkCommand s1 s2 = mkEmacsCommand (inPar (mkString s1 ++ inPar s2))

infoCommand,
  visitCommand  :: String -> String -> String
infoGoalCommand :: String -> String -> String -> String
updateReplaceCommand,
  updateCommand :: String -> String -> String -> String

errorCommand    :: String -> String
newstateCommand :: Int    -> String

errorCommand s                = mkCommand "error" (mkString s)
infoCommand s1 s2             = mkCommand "info" (mkString s1 ++ " " ++ mkString s2)
infoGoalCommand s1 s2 s3      = mkCommand "infogoal" (unwords (map mkString [s1,s2,s3]))
newstateCommand n             = mkEmacsCommand (inPar (mkString "tr#" ++ " " ++ show n ))
updateCommand s1 s2 s3        = mkCommand "update" (unwords [s1,s2,s3])
updateReplaceCommand s1 s2 s3 = mkCommand "updateReplace" (unwords [s1,mkString s2,s3])
visitCommand s1 s2            = mkCommand "visit" (mkString s1 ++ s2)

--messageCommand s              = mkEmacsCommand (inPar (mkString "message"++mkString s))
--versionCommand                = mkCommand "version" (mkString version)

mkMetaList :: [MetaVar] -> String
mkMetaList mvs = inPar (unwords (map show mvs))

promptForInput :: IM ()
promptForInput = liftIOIM (putStr ">> ")

tryReadFile :: String -> IM (String)
tryReadFile f = liftIOIM (readFile f)
                  `handle` (\_ -> raise $ eMsg noPosition (ENoFile f))

mkError :: EMsg -> String
mkError e = errorCommand (prEMsg e)

{-
readStdInput :: IM String
readStdInput = do ss <- liftIOIM readLines
                  return (unlines ss)    

readLines = do s <- getLine
               if null s 
                 then readLines
                 else if head s == '.' 
                        then return []
                        else do ss <- readLines
                                return (s:ss)
-}


tryReadCommand :: Handle -> IM Command
tryReadCommand h = do let isEmpty :: String -> Bool
                          isEmpty s = (filter ( /= ' ') s) == []
                      inp <- liftIOIM (hGetLine' h)
                      if isEmpty inp
                         then return NoCommand
                         else liftEIM (getCommand inp)
                   `handle`  \e -> do liftIOIM (putStrLn (mkError e))
                                      return NoCommand

-- make EOF work like quit (mainly to prevent loop)
hGetLine' h = (hGetLine h) `catch` \e->return "quit"

-- Read lines on standard input until a correct command is read 
-- and then return this command
readCommand :: IM (Command)
readCommand  = do promptForInput
                  com <- tryReadCommand stdin 
                  if com == NoCommand then readCommand else return com
   `handle`  \e -> do liftIOIM (putStrLn (mkError e))
                      readCommand

outputTAllMeta :: IM ()
outputTAllMeta = do s <- readPCMIM printTypeAllMeta
                    liftIOIM (putStr (infoCommand goalBuffer s))


outputTypeMeta :: Int -> MetaVar -> IM ()

outputTypeMeta i m = do s <- readPCMIM (printTypeMeta False i m)
                        liftIOIM (putStr (infoCommand typeBuffer s))   



outputConstraints :: IM ()
outputConstraints = do s <- readPCMIM printConstraints
                       liftIOIM (putStr (infoCommand constraintBuffer s))


outputMeta :: MetaVar -> [MetaVar] -> Bool -> IM()
outputMeta m ms b = liftIOIM (putStr (updateCommand (show m) (show b) (mkMetaList ms)))



outputMetaReplace :: MetaVar -> String -> [MetaVar] -> IM()
outputMetaReplace m s ms = liftIOIM (putStr (updateReplaceCommand (show m) s (mkMetaList ms)))


outputVisit :: String -> [MetaVar] -> IM ()
outputVisit f mvs = liftIOIM (putStr (visitCommand f (mkMetaList mvs)))



outputState :: IM ()
outputState = do n <- nrOfState
                 liftIOIM (putStr (newstateCommand n))

type Filename = String
type Contents = String

readSDefs :: Filename -> Contents -> IM ([MetaVar],[(MetaVar,String,[MetaVar])])
readSDefs f s   = liftPCMIM (addDefsCommand (Position f 1 0)  s)

importSDefs :: Filename -> Contents -> IM ()
importSDefs f s = liftPCMIM (importDefsCommand (Position f 1 0)  s)

termReport :: Filename -> Contents -> PCM String
termReport f s  = do m <- termDefsCommand (Position f 1 0)  s
                     case m of
                       Just e  -> return (prEMsg e)
                       Nothing -> return "Buffer is correct"


updateActionIM :: IM [(MetaVar,String,[MetaVar])] -> IM ()
updateActionIM action = do 
        outputState
        msmss <- action
        mapM_ (\(m,e,ms) -> outputMetaReplace m e ms) msmss
        outputTAllMeta
        outputConstraints


    `handle` \e -> do liftIOIM(putStrLn (mkError e))
                      return ()


updateAction :: PCM [(MetaVar,String,[MetaVar])] -> IM ()
updateAction action = updateActionIM (accessPCMIM action)

infoAction :: String -> PCM String -> IM ()
infoAction buffer action = do 
    s     <- liftPCMIM action
    let ss = infoCommand buffer s
    liftIOIM (putStr ss)

metaInfoAction :: String -> MetaVar -> PCM String -> IM ()
metaInfoAction buffer m action = do 
    s     <- liftPCMIM action
    let ss = infoGoalCommand (show m) buffer s
    liftIOIM (putStr ss)

execCommand :: Command -> IM ()
execCommand c = (case c of
    Load f              -> do contents <- tryReadFile f
                              updateAction (addDefsCommand (Position f 1 0) contents 
                                            >> return [])
    LoadB f b           -> updateActionIM $ do
                              contents <- tryReadFile b
                              (ms, mes) <- readSDefs f contents
                              outputVisit f ms
                              return mes
    Import f            -> do outputState
                              contents <- tryReadFile f
                              importSDefs f contents
    ImportB f b         -> do outputState
                              contents <- tryReadFile b
                              importSDefs f contents
    TermB f b           -> do --outputState
                              contents <- tryReadFile b
                              infoAction terminationBuffer (termReport f contents)
    Give pos m e        -> updateActionIM $ do 
                              (ms,b,mes) <- accessPCMIM $ giveCommand pos m e
                              outputMeta m ms b
                              return mes
    Refine pos m e      -> updateAction $ refineCommand pos m e
    RefineEx pos m e    -> updateAction $ refineExCommand pos m e
    RefinePrj pos m xs  -> updateAction $ refinePrjCommand pos m xs
    Case pos m n        -> updateAction $ caseCommand pos m n
    Abstract pos m xs   -> updateAction $ abstractCommand pos m xs
    Let pos m xs        -> updateAction $ letCommand pos m xs
    Intro pos m         -> updateAction $ introCommand pos m
    Intros pos m        -> do s <- accessPCMIM (introsCommand pos m) 
                              liftIOIM (putStr s)
    Solve               -> updateAction solveAll1
    SolveC t n          -> updateAction $ solveCNumber t n
    Suggest             -> infoAction suggestionsBuffer (suggestCommand >>= 
                                                         return.unlines)
    UnfoldC n s         -> do --outputState
                              accessPCMIM (unfoldCNumber n s)
                              outputConstraints
    Compute f m s       -> metaInfoAction resultBuffer m $ computeCommand f (Just m) s
    ComputeS f m s      -> metaInfoAction resultBuffer m $ computeSCommand f (Just m) s
    ComputeTL f s       -> infoAction resultBuffer $ computeCommand f Nothing s
    ComputeSTL f s      -> infoAction resultBuffer $ computeSCommand f Nothing s
    NfCount f m n s     -> metaInfoAction resultBuffer m $ nfCountCommand f m n s
    Unfold f m s        -> metaInfoAction resultBuffer m $ unfold1Command f m s
    ContUnfold _f m n   -> metaInfoAction resultBuffer m $ contUnfoldNCommand n
    PrintContext m      -> infoAction contextBuffer $ printContext m
    PrintConstraints    -> outputConstraints
    PrintTypeMeta i m   -> outputTypeMeta i m
    PrintAllMeta        -> outputTAllMeta
    TypeOfExp pos i m e -> infoAction typeBuffer $ printTypeExp pos i m e
    New                 -> do undoToIM 0
                              accessPCMIM new 
    TermCounter i       -> liftPCMIM (termCountCommand i)
    Quit                -> return ()
    Auto pos m          -> updateAction$ autoCommand pos m
    Undo i              -> undoToIM i
    ToggleTrace         -> liftPCMIM toggleTraceFl
    NoCommand           -> return ()
    PrintEnv            -> error ("emacsagda:execCommand: unknown "++show c)
-- Plugin commands
#ifdef PLUGINS
    TopPlugin n f a     -> pluginAction n (TopByNum f a)
    GoalPlugin n f m    -> pluginAction n (GoalByNum f m)
    PosPlugin n f pos m    -> pluginAction n (PosByNum f pos m)
#endif
#ifdef DYNAMIC_PLUGINS
    RunPlugin pos m pi e -> 
	     do outputState
		(s, ms, pm) <- runPluginCommand pos m pi e
		outputMetaReplace m s ms 
		outputTAllMeta
		outputConstraints
		outputPluginMessage pm
#endif
   )
                  `handle` \e -> do liftIOIM(putStrLn (mkError e))
                                    return ()

loop :: IM ()
loop  = do com <- readCommand
           execCommand com
           if isQuit com then return () 
                         else loop

inits :: IM ()
inits = do when loadPrelude $ liftPCMIM includePrelude
           loop

main :: IO () 
main  = do hSetBuffering stdout NoBuffering
           putStr versionCommand
           runIM inits initState
           return ()
