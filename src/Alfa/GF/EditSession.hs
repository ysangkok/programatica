module EditSession where

import Operations
import Parsers (parseResults)
import Tokens
import Grammar
import SymbolTable
import Update (updateIdent, updateTerm, updateTermInContext)
import Macros
import State
import IOGrammar
import PrGrammar
import PGrammar2 (pConstraints, pTrm')
import Refine
import CustomCommands
import LocalUndo
import List (intersperse)
import Random (StdGen, mkStdGen)
import Randomized (mkRandomTerms)
import TC (Constraints)
import BottomUp
import Option
import Editing
import Char (isDigit)

-- AR 2000 -- 6/2/2001
-- command language for editing, either line-based or fudget

type EditEnv   = State Str
type EditState = EState

abstractOfEnv = abstractOfState . gStateOfEnv
gStateOfEnv   = id

defOptsEnv = (absView:) . map optLanguage . allLanguages . gStateOfEnv

data ECommand =
 -- action commands
   ECNew Cat
 | ECUndo
 | ECRefine (MetaSymb,(Trm,Type))
 | ECRefineAct Int
 | ECParse [Option] String
 | ECChoose Int
 | ECActivate MetaSymb
 | ECChange Int Int
 | ECLocalWrap Int (Fun,[Int])
 | ECAddConstraints String
 | ECMenu MetaSymb
 | ECTermCommand CommandId
 | ECWrap ((Fun,Type),[Int]) -- bottom-up "refinement"
 | ECWrapAct Int
 | ECGenerateRandom
 | ECOptions Option
 -- commands on outer environment
 | ECSave FilePath
 | ECHelp
 | ECQuit
 | ECVoid
  deriving Eq

execECommand :: EditEnv -> ECommand -> Action
execECommand env c = case c of

  ECNew cat -> eNew abstr cat
  ECUndo -> eUndo
  ECRefine mt -> eRefineRef abstr mt
  ECRefineAct int -> eRefineAct abstr int
  ECParse opts s -> tryParseRefine env opts s
  ECChoose i -> eChoose i
  ECAddConstraints s -> eAddConstrs abstr (mkConstraints abstr s)
  ECTermCommand commid -> eTransform abstr commid
  ECGenerateRandom -> tryRandomRefine env
  ECActivate m -> eActivate m
  ECChange n _ -> eLocalUndo abstr n
  ECLocalWrap n fti -> eLocalWrap abstr n fti
  ECWrap fti -> eWrap abstr fti
  ECWrapAct int -> eWrapAct abstr int
  ECOptions opt -> toggleOption opt
  _ -> eDummy

 where abstr = abstractOfEnv env

tryParseRefine :: EditEnv -> [Option] -> String -> Action
tryParseRefine env opts s st 
  | eStatus st == EWork = eAmbigRefine abst mts st
  | otherwise           = eMessage "no parse refine if no metas" st
 where
   mts  = [(meta, updateTermInContext abst cont t) | t <- ts]
   (meta,cont) = errVal undefined $ do -- safe in EWork!
            ti <- termInfoOfState st
            m  <- firstMetaOfTI ti
            c  <- contextOfMeta ti m
            return (m, map fst c)
   cat  = catOfMeta meta
   ts   = pTrm' s ++ snd (anyLangParser (gStateOfEnv env) opts cat s)
   abst = abstractOfEnv env

tryRandomRefine :: EditEnv -> Action
tryRandomRefine env st
  | eStatus st == EWork = eAmbigRefine abstr mts st
  | otherwise           = eMessage "no random refine if no metas" st
 where
   meta  = riskFirstMetaOfState st
   cat   = catOfMeta meta
   stgen = mkStdGen (lengthEState st * (length (messageOfState st) + 11)) --- 
   mx    = 23 ---
   trms  = take mx $ mkRandomTerms stgen mx abstr cat
   mts   = [(meta,trm) | trm <- take 1 trms]
   abstr = abstractOfEnv env

mkConstraints abs s = 
  [(updateTerm abs x,updateTerm abs y) | (x,y) <- pConstraints s ]

prEditEnv :: EditEnv -> EState -> String
prEditEnv env st = prMsg (messageOfState st) ++ case eStatus st of 
  EInitial -> 
    "SELECT NEW GOAL TO START"
  EEmpty -> 
    "NOTHING TO WORK ON"
  e | elem e [EWork, EImpossible] -> 
    let Ok ti = termInfoOfState st in 
      prTermInfoEnv env (optionsOfState st) ti
  e | elem e [EComplete, EAmbiguous] ->
    let tis = termInfosOfState st in 
      unlines (intersperse (replicate 50 '=') 
        (map (prTermInfoEnv env (optionsOfState st)) tis))
 where prMsg m = if null m then "" else unlines m ++++ replicate 50 '*' ++ "\n\n"

prTermInfoEnv env opts ti = 
      unlines (intersperse hyphens
                ((if ooElem absView then (prTermInfo ti :) else id)
                 [unlines (tkOpts (linearizeResults g (termOfTI ti))) 
                                                           | g <- grammars]))
  where
    hyphens = replicate 50 '-'
    grammars = map (flip grammarOfLanguage gst) (filtOpt (allLanguages gst))
    gst = gStateOfEnv env
    filtOpt langs = filter (\ (Language s) -> oElem (iOpt s) opts) langs
    ooElem o      = oElem o opts
    tkOpts 
      | oElem allLin opts = id
      | otherwise = take 1

mkRefineMenu :: Bool -> EditEnv -> EState -> [(ECommand, String)]
mkRefineMenu nums env st = case eStatus st of
  EInitial -> 
    [(ECVoid, "SELECT New CATEGORY TO START")]
  EComplete -> 
    [(ECVoid, "THE EXPRESSION IS COMPLETE")]
  EImpossible -> 
    [(ECVoid, "IMPOSSIBLE STATE")]
  EAmbiguous ->
    let tis = termInfosOfState st in
      (ECVoid, "Select alternative") : (ECVoid, "") :
      [(ECChoose i, ifnums i ++ prt (termOfTI ti)) | (i,ti) <- zip [0..] tis]
  EWork ->
    let Ok ti   = termInfoOfState st 
        Ok meta = firstMetaOfTI ti
        Ok typ  = typeOfMeta ti meta
        Ok cont  = contextOfMeta ti meta
        refs    = zip [0..] (activeRefinements abstr st)
    in
     (ECVoid, "Active:"  +++ prt meta) :
     (ECVoid, "Type:"    +++ prt typ) :
     (if null cont then [] else [(ECVoid, "Context:" +++ prContext cont)]) ++
     [(ECVoid, "")] ++
     [(ECRefine (meta,(c,t)), ifnums i ++ prt c +++ ":" +++ prt t) | 
                                                       (i,(c,t)) <- refs] ++
     [(ECVoid,""), (ECVoid,"YOU CAN ALSO TRY PARSING")]
  _  -> [(ECVoid, "NO REFINEMENTS IN THE EMPTY STATE")]
 where 
   abstr = abstractOfEnv env
   ifnums i = if nums then show i ++ ". " else ""

env2Change :: EditEnv -> EditState -> [(Int -> ECommand, String)]
env2Change env st = case eStatus st of
  EWork     ->  [(fChOrRem k f, prTr n (xx,f)) | 
                     (k,(n,(xx,f))) <- zip [0..] (errList (mkTermTree abs term))]
  EComplete ->  [(fChOrRem k f, prTr n (xx,f)) | 
                     (k,(n,(xx,f))) <- zip [0..] (errList (mkTermTree abs term))]
  _ ->          [(const ECVoid, "NO CHANGE POSSIBLE NOW")]
 where
     term = errVal (Meta (mkFreshMeta [] (zIdent "??"))) $ termOfState st
     fChOrRem n f = case f of 
         Meta m             -> (\_ -> ECActivate m)
         Closure _ (Meta m) -> (\_ -> ECActivate m)
         _ -> ECChange n
     isMeta f = case f of {Meta _ -> True ; Closure _ (Meta _) -> True ; _ -> False}
     prTr n (xx,f) = (if isMeta f then "? " else "   ") +++ repl n "|  " +++
                     prt (mkAbs xx f)
     repl n s = if n == 0 then "" else s ++ repl (n-1) s
     abs = abstractOfEnv env

env2FWrap :: Bool -> EditEnv -> EditState -> [(ECommand,String)]
env2FWrap nums env st = case eStatus st of
  EWork     -> mkWraps
  EComplete -> mkWraps
  _  -> []
 where
   mkWraps =
      (ECVoid, "Apply function to term") : (ECVoid, "") :
      [(ECWrap w, ifnums i ++ prt f +++ prT ps t) | (i,w@((f,t),ps)) <- wraps]
   prT ps t = case typeForm t of
      Ok (cont,cat,_) -> unwords (map prOne (zip [1..] cont)) +++ ":" +++ prt cat
                             where prOne (i,_) = if elem i ps then "t" else "_" 
      Bad s -> s
   wraps = zip [0..] $ activeWraps (abstractOfEnv env) st
   ifnums i = if nums then show i ++ ". " else ""

-- temporary implementation of local wrap, 9/4/2001
parseLocalWrap :: String -> ECommand
parseLocalWrap s = case words s of
  n:f:is | isInt n && all isInt is -> ECLocalWrap (read n) (zIdent f,map read is)
  _ -> ECVoid

isInt s = not (null s) && all isDigit s
