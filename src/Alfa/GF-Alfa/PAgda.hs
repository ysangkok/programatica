module PAgda where

import qualified UAbstract as A
import Operations
import Parsers
import A2GSyntax 

-- ad hoc parser for Agda. AR 12/10/1999 -- 31/3/2000
-- should be replaced by the standard one

pATheory :: Parser Char Theory
pATheory = pJ (pTList ";" pADef +|| succeed []) *** Theory

pADef :: Parser Char A.DefB
pADef = 
  pAIdent                             .>. (\fun ->
  pJ pAContext                        .>. (\cont ->
  jL "::"                             .>. (\_ ->
  pAExp                               .>. (\typ ->
  jL "="                              .>. (\_ ->
  pAExp                               .>. (\def ->
  succeed (A.Value (A.Var fun,(cont,def,typ)))))))))
 |||
  jL "data"                           .>. (\_ ->
  pAIdent                             .>. (\fun ->
  pJ pAContext                        .>. (\cont ->
  jL "="                              .>. (\_ ->
  pJ (pTList "|" pAConstructor)       .>. (\cc -> 
  succeed (A.Data (A.Var fun,(cont,cc))))))))
 |||
  jL "package"  .>. (\_ ->
  pJ pAIdent    .>. (\name ->
  pJ pAContext  .>. (\cont ->
  jL "where"    .>. (\_ ->
  jL "{"        .>. (\_ ->
  pADecls       .>. (\decls  ->
  jL "}"        .>. (\_  ->
  succeed (A.Package (A.Var name, (cont, A.PackageDef decls))))))))))
 |||
  jL "open"           .>. (\_ ->
  pJ pAIdent          .>. (\name ->
  jL "use"            .>. (\_ ->
  pTList "," pAIdent  .>. (\ops ->
  succeed (A.Open (A.EVar (A.Var name),[A.OpenArg [] (A.Var x) Nothing Nothing | x<- ops]))))))
 |||
  pAIdent  .>. (\x -> 
  jL "="   .>. (\_ ->    
  pAExp    .>. (\e ->
  succeed (A.Binding (A.Var x, e)))))

pADecls :: Parser Char A.Decls
pADecls = pATheory *** 
            (\ (Theory th) -> [A.decl' [A.defA d | d <- th]])

pAContext :: Parser Char A.Context
pAContext = longestOfMany (pJ (pParenth pADecl)) *** A.ctx

pADecl :: Parser Char (A.Var, A.Exp)
pADecl = (pAIdent *** A.Var) ... jL "::" +.. pAExp 

pALabelling :: Parser Char A.SigPart
pALabelling = (pAIdent *** A.Label) ... jL "::" +.. pAExp *** uncurry A.SigField

pAExp :: Parser Char A.Exp
pAExp = pAExpp 0

pAExpp :: Int -> Parser Char A.Exp
pAExpp 3 = 
  lits "?" +.. pIntc *** A.EMeta
 |||
  pASort      *** A.ESort
 |||
  pAIdent     *** A.EVar . A.Var
 |||
  pParenth (pAExpp 0)
pAExpp 2 =
  pAExpp 3 .... 
    (longestOfSome (pJ (pAExpp 3)) *** (\aa e -> A.app e aa)
      |||
     jL "." +.. pAIdent *** (\l e -> A.EProj e (A.Label l)))
   *** (\ (e,h) -> h e) 
 |||
  pAIdent ... jL "@_" +.. longestOfMany (pJ (pAExpp 3)) 
    *** (\ (c,aa) -> A.eCon (A.Con c) aa)
 |||
  jL "case" +.. pAExp ... jL "of" +.. 
  jL "{" +.. longestOfMany (pJ pABranch) ..+ jL "}" 
    *** (\ (e,bb) -> A.ECase e bb)
 |||
  jL "data" +.. pJ (pTList "|" pAConstructor ||| succeed [])
    *** A.ESum
 |||
  jL "sig" +.. jL "{" +..  pJ (pTList ";" pALabelling ||| succeed []) ..+ jL "}"
    *** A.ESig
 |||
  jL "struct" +.. jL "{" +.. pADecls ..+ jL "}"
    *** A.EStr
 |||
  pAExpp 3
pAExpp 1 =
  jL "\\" +.. pParenth pADecl ... jL "->" +.. pAExpp 1 
    *** (\ ((x,a),b) -> A.EAbs (x A.:- a) b)
 |||
  pParenth pADecl ... jL "->" +.. pAExpp 1 
    *** (\ ((x,a),b) -> A.EPi (x A.:- a) b)
 |||
  jL "let" +.. jL "{" +.. pADecls ... jL "}" +.. jL "in" +.. pAExpp 1
    *** (\ (dd,e) -> A.ELet dd e)
 |||
  pAExpp 2 ...
  (jL "->" +.. pAExpp 1 *** (\b a -> A.EPi (A.Var "_" A.:- a) b)
    |||
   succeed id)
   *** uncurry (flip ($))
pAExpp 0 =
  pAExpp 1

pABranch :: Parser Char A.Branch
pABranch = 
  pParenth (pAIdent .... longestOfMany (pJ pAIdent *** A.Var)) ...
  jL "->" +.. pAExp ..+ jL ";" *** (\ ((c,vv),e) ->  A.Branch (A.Con c,(vv,e)))

pAConstructor :: Parser Char A.Constructor
pAConstructor = 
 (pAIdent *** A.Con) .... pAContext .... succeed [] --- empty EqRestr ?
 *** A.Constr

pAIdent :: Parser Char String
pAIdent = pVarIdent ["data", "sig", "struct", "case", "Set", 
                     "Type", "package", "use", "where", "open"]

pASort :: Parser Char A.Sort
pASort = pLongestIdent ["Set", "Type", "Prop"] *** A.Sort --- Prop may be too much

pAImport :: Parser Char [String]
pAImport = 
 jL "{--#" +.. jL "Import" +.. pTList "," pFileName ..+ jL "#--}" ||| succeed []

-- comment removal, works for {- comment -} ; NB spaces after and before -'s
removeAgdaComments :: String -> String
removeAgdaComments s =
 case s of
   '{':'-':c:t | not (specComm c) -> getAgdaComment t
   c:t                            -> c : removeAgdaComments t
   []                             -> []
  where
   getAgdaComment s =
    case s of
      c:'-':'}':t | not (specComm c) -> removeAgdaComments t
      c:t                            -> getAgdaComment t
      []                             -> []
   specComm c = c `elem` "#"

