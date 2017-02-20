{- 
#include "config.h" 
-}
module Parse_com where 

--import CSyntax
import MetaVars
import FString (StrTable)
import Monads 
--import Lex (lexStart,Token (..),LexItem (..),prLexItem)
--import FString(emptyStrTable)
import Error(EMsg , ErrMsg (..))
import List(nub)
import Char
import AgdaTrace
import Position
#ifdef DYNAMIC_PLUGINS
import DynamicPlugin(PluginInfo(..))
#endif

data Command = Load String  -- not used in emacs-interface 
             | Import String  -- not used in emacs-interface
             | LoadB String String
             | ImportB String String
             | TermB String String
           --  | AddDef CLetDef
             | Give Position MetaVar String
             | Refine Position MetaVar String
             | RefineEx Position MetaVar String
             | RefinePrj Position MetaVar String
             | Case Position MetaVar String
             | Intro Position MetaVar
             | Abstract Position MetaVar String
             | Let Position MetaVar String
--           | IntroS MetaVar
--           | RefineC MetaVar CConName
             | SolveC Int Int
             | Suggest
             | UnfoldC Int Int 
             | Solve
             | Intros Position MetaVar  -- ???
             | Compute Position MetaVar String
             | ComputeS Position MetaVar String
             | ComputeTL Position String
             | ComputeSTL Position String
             | NfCount Position MetaVar Int String
             | Unfold Position MetaVar String
             | ContUnfold Position MetaVar Int
             | PrintConstraints
             | PrintTypeMeta Int MetaVar
             | PrintContext MetaVar
             | PrintAllMeta
             | TypeOfExp Position Int MetaVar String
             | PrintEnv 
             | New 
             | Undo Int
             | Quit
             | ToggleTrace
             | TermCounter Int
--             | Batch
--             | DShowval Name   -- For debugging
             | Auto Position MetaVar
             | NoCommand
             | TopPlugin String Int String 
             | GoalPlugin String Int MetaVar 
	     | PosPlugin String Int Position MetaVar
#ifdef DYNAMIC_PLUGINS
             -- | for dynamic plugin
             | RunPlugin Position MetaVar PluginInfo String 
#endif
            deriving (Eq,Show)

isQuit Quit = True
isQuit _    = False


removeDQ,removeDQ' :: String -> String
removeDQ ('\"':s) = removeDQ' s -- "
removeDQ s        = s
removeDQ' "\""    = []
removeDQ' (c:cs)  = c:removeDQ' cs


getWord :: String -> (String,String)
getWord s = case dropWhile isSpace s of
                  "" -> ([],[])
                  s' -> break isSpace s'

getWordN :: Int -> String -> ([String],String)
getWordN 0 s = ([],s)
getWordN n s = let (s',r) = getWord s
                   (s2,r') = getWordN (n-1) r
               in (s':s2,r')

getWordLine :: String -> String
getWordLine  s = let (w,r) = break (\x -> x == '~') s 
                 in if r == [] then w
                               else getWord' w (tail r)
     where getWord' s [] = s
           getWord' s s' = let (w1,r) = break (\x -> x == '~') s'
                           in if r == [] then s++"\n"++w1
                              else getWord' (s++"\n"++w1) (tail r)


mkPosition :: String -> String -> String -> Position
mkPosition f l c = Position (removeDQ f) (read l) (read c - 1)

getCommand :: String -> Error Command
getCommand s = let (c,r) = getWord s in case c of
    "load"            -> let (f,_) = getWord r
                         in return (LoadB (removeDQ f) (removeDQ f))
    "LOAD"            -> let ([a1,a2],_) = getWordN 2 r
                         in return (LoadB(removeDQ a1) (removeDQ a2))
    "import"          -> let (f,_) = getWord r
                         in return (Import (removeDQ f))
    "IMPORT"          -> let ([a1,a2],_) = getWordN 2 r
                         in return (ImportB (removeDQ a1) (removeDQ a2))
    "TERM"            -> let ([a1,a2],_) = getWordN 2 r
                         in return (TermB (removeDQ a1) (removeDQ a2))
    "new"             -> return New
    "undo"            -> let (i,_) = getWord r
                         in if all isDigit i
                              then return (Undo (read i))
                              else internalError "Parse_com: expected a number: "
    "quit"            -> return Quit
    "solve"           -> return Solve
    "solveC"          -> let ([t,c],_) = getWordN 2 r
                         in return (SolveC (read t) (read c))
    "suggest"         -> return Suggest
    "unfoldC"         -> let ([c,s],_) = getWordN 2 r
                         in return (UnfoldC (read c) (read s))
    "printEnv"        -> return PrintEnv
    "showConstraints" -> return PrintConstraints
    "mkCase"          -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (Case pos (read m) e)
    "refine"          -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (Refine pos (read m) e)
    "refineEx"        -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (RefineEx pos (read m) e)
    "refinePrj"       -> let ([f,l,c,m],r') = getWordN 4 r
                             pos = mkPosition f l c
                         in return $ RefinePrj pos (read m) r'
    "give"            -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine  (tail r')
                             pos = mkPosition f l c
                         in return (Give pos (read m) e)
    "intro"           -> let ([f,l,c,m],_) = getWordN 4 r
                             pos = mkPosition f l c
                         in  return (Intro pos  (read m))
    "intros"          -> let ([f,l,c,m],_) = getWordN 4 r
                             pos = mkPosition f l c
                         in  return (Intros pos  (read m))
    "auto"            -> let ([f,l,c,m],_) = getWordN 4 r
                             pos = mkPosition f l c
                         in  return (Auto pos  (read m))
    "abstract"        -> let ([f,l,c,m],r') = getWordN 4 r
                             pos = mkPosition f l c
                         in return $ Abstract pos (read m) r'
    "let"             -> let ([f,l,c,m],r') = getWordN 4 r
                             pos = mkPosition f l c
                         in return $ Let pos (read m) r'
    "compute"         -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (Compute pos (read m) e)
    "computeS"        -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (ComputeS pos (read m) e)
    "computeTL"         -> let ([f,l,c],r') = getWordN 3 r
                               e   = getWordLine (tail r')
                               pos = mkPosition f l c
                         in return (ComputeTL pos e)
    "computeSTL"        -> let ([f,l,c],r') = getWordN 3 r
                               e   = getWordLine (tail r')
                               pos = mkPosition f l c
                         in return (ComputeSTL pos e)
    "unfold"          -> let ([f,l,c,m],r') = getWordN 4 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (Unfold pos (read m) e)
    "continue"        -> let ([f,l,c,m,n],r') = getWordN 5 r
                             pos = mkPosition f l c
                         in return (ContUnfold pos (read m) (read n))
    "nfC"             -> let ([f,l,c,m,n],r') = getWordN 5 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return $ NfCount pos (read m) (read n) e
    "printg"          -> let ([flag,f,l,c,m],_) = getWordN 5 r
                         in if flag == "toiota"
                              then return (PrintTypeMeta 1 (read m))
                              else return (PrintTypeMeta 0 (read m))
    "printGAll"       -> return PrintAllMeta
    "typeOfExpression"-> let ([flag,f,l,c,m],r') = getWordN 5 r
                             e   = getWordLine (tail r')
                             pos = mkPosition f l c
                         in return (TypeOfExp pos (read flag) (read m)  e)  
    "printmc"         -> let ([f,l,c,m],_) = getWordN 4 r
                         in return (PrintContext  (read m))
    "setTerm"         -> let (n,_) = getWord r
                         in return (TermCounter  (read n))
    "toggleTrace"     -> return ToggleTrace
    "topPlugin"       -> let([p,f], a) = getWordN 2 r
                         in return (TopPlugin p (read f) a)
    "goalPlugin"       -> let([p,f,m], _) = getWordN 3 r
                         in return (GoalPlugin p (read f) (read m))
    "posPlugin"       -> let([pn,fun,f,l,c,m], _) = getWordN 6 r
			    pos = mkPosition f l c
                         in return (PosPlugin pn (read fun) pos (read m))
#ifdef DYNAMIC_PLUGINS
    "runPlugin"       -> let ([f,l,c,m],r') = getWordN 4 r
		             (pi, r'') = getRunPluginInfo (tail r')
		             pos = mkPosition f l c
		             e = getWordLine (tail r'')
                         in return $ RunPlugin pos (read m) pi e
                          where 
                           getRunPluginInfo :: String -> (PluginInfo, String)
                           getRunPluginInfo s = 
	                    let ([p, f, c, n], r) = getWordN 4 s
		                argc = (read n) :: Int
		                (o, r') = getWordN argc r
                                pi = PluginInfo {
                                       pluginName = p,
                                       pluginPath = f,
                                       pluginCommand = c,
                                       pluginArgs = o
                                     }
		            in (pi, r')
#endif
    s                 -> internalError ("Parse_com: unknown command: " ++ s)
