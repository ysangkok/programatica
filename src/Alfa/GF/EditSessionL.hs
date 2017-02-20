module EditSessionL where

import Operations
import Grammar ()
import CustomCommands ()
import EditSession
import Editing
import Option
import Macros (zIdent)
import PGrammar2 (pMeta)

-- a line-based syntax editor editor, alternative to EditSessionF
-- using the same abstract command language EditSession

gfEdit :: EditEnv -> [Option] -> IO () -> IO ()
gfEdit st opts resume = editLoop st (addOptions opts emptyEState) resume

editLoop st env resume = do
  putStrLn ""
  putStr "edit> "
  s <- getLine
  let c = pECommand (words s)
  (case c of
    ECQuit -> resume
    ECHelp -> do
      putStrLn editHelpMsg
      editLoop st env resume
    _ -> do
      let env' = execECommand st c env
      putStrLn $ prEditEnv st env'
      putStrLn $ prRefineMenu st env'
      editLoop st env' resume)

pECommand ws = case ws of
  "n":c:_ -> ECNew (zIdent c)
  "w":i:_ | isInt i -> ECWrapAct (read i)
  "h":_   -> ECHelp
  "q":_   -> ECQuit
  "u":_   -> ECUndo
  "p":ww  -> ECParse [] (unwords ww)
  "c":ww  -> ECAddConstraints (unwords ww)
  "g":_   -> ECGenerateRandom
  "r":i:_ | isInt i -> ECRefineAct (read i)
---  "l":i:_ | isInt i -> ECChange (read i) Nothing
  "o":s:_ -> ECOptions (iOpt s)
  "a":i:_ -> ECActivate (pMeta i)
  "s":i:_ | isInt i -> ECChoose (read i)
  "t":f:_ -> ECTermCommand f
  _ -> ECHelp

prRefineMenu :: EditEnv -> EState -> String
prRefineMenu env st = unlines (map snd (mkRefineMenu True env st))

editHelpMsg =
  "GF editing session commands:" ++++
  "  n Cat      = create new goal of type Cat" ++++
  "  o Opt      = toggle option" ++++
  "  q          = quit the editor (= resume gf session)" ++++
  "  r Int      = perform refinement Int" ++++
  "  s Int      = select alternative Int" ++++
  "  a Meta     = activate subgoal Meta" ++++
---  "  l Int      = delete subterm Int" ++++
  "  w Int      = perform wrapping Int" ++++
  "  g          = generate random object for active subgoal" ++++
  "  t Command  = perform Command on term" ++++
  "               (standardly available: compute, paraphrase)" ++++
  "  c Constr   = add constraint Constr" ++++
  "  u          = undo (= go one step back in the history)" ++++
  "  h          = help (= display this message)"


