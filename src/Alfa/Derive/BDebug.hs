module BDebug where
import Trace(trace)
import UAbstract
--import ProofEngine(Env)
import AlfaPluginKit
import AlfSyntax

------------------------------ trace ------------------------------
mtrace :: String -> a -> a
--mtrace s x = const x (unsafePerformIO (print s))
mtrace = trace

notrace::  String -> a -> a
notrace s x = x

mtrace2 :: String -> String -> a -> a
mtrace2 s1 s2 x = mtrace (s1++s2) x

notrace2 :: String -> String -> a -> a
notrace2 s1 s2 x =  x

mtracen :: [String] -> a -> a
mtracen ss x = mtrace (unwords ss) x

------------------------------- show -------------------------------
-- TODO: recode show* in the ``shows'' style

showTry:: Exp -> Exp -> String
showTry exp typ = "Trying: "++showExp exp++"::" ++ showExp typ

showExp :: Exp -> String
showExp e = case e of
 	       EMeta m -> '?':show m
	       ESort (Sort s) -> s
	       EVar (Var s) -> s
	       EApp e0 e1 -> showExp e0 ++ showAExp e1
	       ECon (Con s) -> s ++ "@"
	       EPi (Var s:- t) e' -> "("++s++"::"++(showExp t)++")->"
                                       ++showAExp e'
--	       EProj e (Label s) -> (showExp e)++"."++s
	       EProj e (Label s) -> (showExp e)++"."++s
               ECase e _ -> "case " ++ (showExp e)++ " of..."
               EAnnot _ e' -> showExp e'
	       _ -> show e

showAExp (EVar (Var s)) = " "++s
showAExp e = "("++showExp e++")"

showExps :: [Exp] -> String  
showExps = unlines . (map showExp)

join          :: String -> [String] -> String
join _ []       =  ""
join sep ws       =  foldr1 (\w s -> w ++ sep ++ s) ws

showExpsH :: [Exp] -> String  
showExpsH = (join ", ") . (map showExp)

showEnvElem :: (Var,Exp) -> String
showEnvElem (Var s,t) = s++"::"++(showExp t)

showEnv :: Env -> String  -- type Env = [(Var,Exp)] 
showEnv = unlines . (map showEnvElem)

showDefB :: DefB -> String
showDefB (Value (Var s,(Ctx _ binds,e1,e2))) 
  = s++"(...)="++showExp e1++"::"++showExp e2
showDefB d = show d

showMaybeDefB :: (Maybe DefB) -> String
showMaybeDefB Nothing = "Nothing"
showMaybeDefB (Just d) = showDefB d


showRunRes :: (Either Error (Exp,State))  -> String
showRunRes foo  = case foo of
                       Left _ -> "*error*"
                       Right (rexp, rstate) -> showExp rexp

--instance Show [Syntax] where
--	show = unlines . (map show)

showSyns :: [Syntax] -> String
showSyns syns = unlines $ map show syns
