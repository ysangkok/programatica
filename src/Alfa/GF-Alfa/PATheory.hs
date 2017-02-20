module PATheory where

import Ix

import Operations
import Parsers
import Tokens
import Grammar
import Macros (symid, zIdent)
import PGrammar 
import A2GSyntax
import AlfaToGrammar
import PAgda (pATheory, removeAgdaComments) --- should come from the standard parser
import AIdent


-- parser for annotated Agda theories. AR 12/10/1999 -- 24/11 -- 2/3/2000

testPAATheory :: String -> IO ()
testPAATheory file = do
  src <- readFileIf file
  let t = case parses pAATheory src of
            ((imps,(th,annots)),rest):_ -> 
                  sl imps ++++ sl th ++++ sl annots +++++ rest
            _ -> "No parse"
  putStrLn t
 where sl = show . length

type AImportList = [(Language,String)] -- concrete grammar file names

-- an entire annotated theory: import list + Agda theory + annotations
pAATheory :: Parser Char (AImportList,(ATheory,[(Language,Annotation)]))
pAATheory = pA .<. removeAgdaComments where
 pA =
  pJ pAImportList                              .>. (\imps ->
  pATheory                                     .>. (\ (Theory th) ->
  (lits "\n\n" ..+ pJunk ||| jL ";")           .>. (\_ ->
  longestOfMany (pJ pAnnotation)               .>. (\aa -> 
  succeed (imps,(th,aa))))))

-- one single annotation, 
-- e.g. {-# E Succ n = mkPN ("the" ++ "successor" ++ "of" ++ n.lin ! pn) #-}
pAnnotation :: Parser Char (Language, Annotation)
pAnnotation =
  jL "{-#"                                        .>. (\_ ->
  jL "GF"                                         .>. (\_ ->
  pJustAnnotation                                 .>. (\annot ->
  jL "#-}"                                        .>. (\_ ->
  succeed annot))))

pJustAnnotation :: Parser Char (Language, Annotation)
pJustAnnotation =
  pJ pLanguageId                          .>. (\lang ->
  pJ pDefLin                              .>. (\annot ->
  succeed (lang,annot)))

pDefLin :: Parser Char Annotation
pDefLin = pAIdent ... longestOfMany (pJ pVIdent) ... jL "=" +..  pTerm 
                        *** (\ (f, (xx, t)) -> DefLin (zIdent f) xx t)

pLanguageId :: Parser Char Language
pLanguageId = pIdent *** Language

-- an import list, e.g. {-# Import E = alfaEng.gf, F = alfaFra.gf  #-}
pAImportList :: Parser Char AImportList
pAImportList = 
  jL "{-#" +.. jL "Import" +.. pTList "," pImp ..+ jL "#-}" ||| succeed [] where
    pImp = pLanguageId ... jL "=" +.. pFileName

newtype Language = Language String deriving (Read, Show, Eq)

langName :: Language -> String
langName (Language s) = s ---
