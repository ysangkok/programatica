module InfoPlugin where
import AgdaPluginKit
import PPrint

infoPlugin :: Plugin
infoPlugin = ("Info", "provides information",
                infoDispatch)

infoDispatch :: PluginDispatch
infoDispatch (GoalByNum 1 m) = doInContext m printContext
infoDispatch (PosByNum 1 pos m) = doInContext m printContext

infoDispatch _ =  printAction "Info plugin: no such function"

printContext :: [ArgsBind] -> IM()
printContext binds = printAction $ infoCommand "* Info Plugin *" ( ppAll binds) 

mkEmacsCommand :: String -> String
mkEmacsCommand s = "--- " ++ s ++ " +++\n"

mkCommand :: String -> String -> String
mkCommand s1 s2 = mkEmacsCommand (inPar (mkString s1 ++ inPar s2))

infoCommand s1 s2             = mkCommand "info" (mkString s1 ++ " " ++ mkString s2)

mkString :: String -> String
mkString s = show s

inPar :: String -> String
inPar s = " (" ++ s ++ ")"
