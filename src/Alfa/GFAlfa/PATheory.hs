module PATheory where

import Parsers
import Types (typeForm, Symb(..))
import Grammar
import PGrammar (pEntry)
import AgdaToGrammar
import A2GSyntax (Theory)
import PAgda (pATheory, removeAgdaComments) --- should come from the standard parser
import Operations ()
import PTypes()

-- parser for annotated Agda theories. AR 12/10/1999 -- 24/11

-- an entire annotated theory: import list + Agda theory + annotations
pAATheory :: Grammar -> Parser Char (ATheory,(ATheory, [Annotation]))
pAATheory gr = pA . removeAgdaComments where
 pA =
  pJ pAImport                                  .>. (\_ ->
  pJ pATheory                                  .>. (\ (Theory th) -> 
  let th' = concat (map a2gDD th) in
   (lits "\n\n" ..+ pJunk ||| jL ";")            .>. (\_ ->
   longestOfMany (pAnnotation gr (map a2gD th')) .>. (\aa -> 
   succeed (th,(th',aa))))))

-- one single annotation, e.g. {-+ Abs - "we have a contradiction" +-}
pAnnotation :: Grammar -> [GDef] -> Parser Char Annotation
pAnnotation gr defs =
  jL "{-+"                                        .>. (\_ ->
  pIdent                                          .>. (\ident ->
  jL "-"                                          .>. (\_ ->
  maybe fails (pEntry . envt) (lookup ident defs) .>. (\entry ->
  jL "+-}"                                        .>. (\_ ->
  succeed (ident,entry))))))
   where 
    envt typ = (gr,[],[(x,(n,typeForm t)) | 
                          ((x,t),n) <- zip [(x,t) | (Symb x,t) <- xx] [0..]])
                                             where (xx,_,_) = typeForm typ

-- an import list, e.g. {-# Import agdaEng.gf, Satslogik.agda.gf #-}
pAImport :: Parser Char [String]
pAImport = 
 jL "{--#" +.. jL "Import" +.. pTList "," pFileName ..+ jL "#--}" ||| succeed []

