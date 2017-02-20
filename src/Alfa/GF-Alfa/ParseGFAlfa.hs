module ParseGFAlfa where

import Operations
import Parsers
import CF
import GFtoCF
import GFCFIdents
import Parsing
--import Earley
import TopDown2
import Tokens
import Grammar
import SymbolTable
import Macros
--import AIdent
--- import AlfaToGrammar
type AbstractAST = AbstractST
type ConcreteAST = ConcreteST AToks
type GrammarAST  = GrammarST AToks
type GFCFA   = GFCF ATok
type AToks   = [ATok]
type ATok    = Str

-- customized parsing for GF-Alfa. 
-- Hopefully needless in the future. Hopelessly needful now.
-- AR 24/10/2000

parserGFAlfa cf =
    --{-
    \ s ->
    take 2 [(mkExp t',s') | (t,s') <- topdownParser2 cf catExp s,
                             s' == [], 
                             Ok t'  <- [cf2trm t]]
    --}
    --first (complete (earleyParser cf catExp) *?* (ok . cf2trm))
  where
    ok (Ok t) = Just t
    ok _ = Nothing
    catExp  = "Exp"
    mkExp t = case termForm t of
      Ok (xx,Cons ecf,aa) | ecf1 == expcons -> 
        App (Cons (zIdent expcons)) 
            (mkTerm (xx,(Cons (zIdent ecf2)), map mkExp aa))
       where (ecf1,ecf2) = (x, {-idFromGF-} (tail y))   --- not the proper place !
                  where (x,y) = splitAt 7 $ symid ecf
      _ -> composSafeOp mkExp t
    expcons = "ExpCons"

grammar2cfReducedAlfa :: GrammarAST -> Concrete AToks -> Err (GFCFA,String)
grammar2cfReducedAlfa st g = do
   ((cf0,_),msg) <- grammar2cfReduced isSmallRule st g
   let
     vars  = cfVarGFAlfa
     rules = map mkExpConsCF cf0
   return ((rules,vars),msg)
  where
    isSmallRule f = case symid f of
      'A':'A':_ -> True
      'C':'C':_ -> True
      h -> elem h $ words "ExpVar MkVar emptySet emptySig emptyStruct Set Type"

mkExpConsCF :: GFCFRule ATok -> GFCFRule ATok
mkExpConsCF r@(GFCFFun ((Cons f,b), p), (c, its)) = case c of
  "Cons" -> (GFCFFun ((f',b),p), ("Exp", its))
                 where f' = Cons (zIdent ("ExpCons/" ++ symid f))
  _ -> r


cfVarGFAlfa t@(Str s) = case s of
  '$':cs@(_:_) | last cs == '$' -> 
    [(
      "String", 
      GFCFFun ((Literal (zIdent "String") (init cs), []),[])
    )]
  _ -> []

