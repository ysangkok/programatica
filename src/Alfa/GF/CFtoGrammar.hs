module CFtoGrammar where

import Grammar
import CF
import Macros
import List (nub)

-- 26/1/2000 -- 18/4

-- typically we put f = Str :: String -> Str
cf2grammar :: (a -> b) -> CF a String String -> Grammar [b]
cf2grammar f (rules,_) = Grammar (Abstract abs, Concrete conc) where
 abs = cats ++ funs
 conc = lintypes ++ lins
 cats = [DefCat cat [] | cat <- nub (concat (map cf2cat rules))]
 lintypes = [DefLintype cat (mkRecType linLabel [TypeStr]) | DefCat cat _ <- cats]
 (funs,lins) = unzip (map (cf2rule f) rules)

cf2cat :: CFRule a String String -> [Ident]
cf2cat (_,(cat, items)) = 
  zIdent cat : [zIdent c | CFNonterm c <- items]

cf2rule :: (a -> b) -> CFRule a String String -> (Def, LDef [b])
cf2rule f (fun,(cat, items)) = (def,ldef) where
 def = DefFun (zIdent fun) (mkProd (args', Cons (zIdent cat), []))
 args0 = zip (map (mkIdent "x") [0..]) items
 args = [(v, Var (zIdent cat)) | (v, CFNonterm cat) <- args0]
 args' = [(zIdent "_", Var (zIdent cat)) | (_, CFNonterm cat) <- args0]
 ldef = DefLin (zIdent fun) (map fst args) 
               (mkRecord linLabel [foldconcat (map mkIt args0)])
 mkIt (v, CFNonterm cat) = Project (Var v) (linLabel 0)
 mkIt (_, CFTerm (RegAlts [a])) = Tok [f a]
 mkIt _ = Tok [] --- regexp not recognized in input CF ; use EBNF for this
 foldconcat [] = Tok []
 foldconcat tt = foldr1 Concat tt

