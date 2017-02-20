module GFtoXML where

import Operations
import XML
import Predefined
import Tokens
import Grammar
import Macros
import Lookup
import SymbolTable
import PrGrammar

-- translating GF grammars and trees into a fragment of XML. AR 5/1/1999 -- 5/6
-- Copyright (c) Aarne Ranta 1999, under GNU General Public License (see GPL)

prXAbstractST :: AbstractST -> String
prXAbstractST = unlines . prDTDlocal . grammar2dtd

prXTrm :: AbstractST -> Trm -> String
prXTrm abstr = unlines . prElementX . term2elemx abstr

gf2xml :: (AbstractST, Trm) -> (DTD,ElemX)
gf2xml (gr,trm) = (grammar2dtd gr, term2elemx gr trm)

grammar2dtd :: AbstractST -> DTD
grammar2dtd abstract = 
 concat [gf2rulex abstract (cat, rulesOf cat) | (cat, IdentCat _) <- defs] ++ vars
  where
   rulesOf cat = [(fun,typ) | (fun,IdentFun typ) <- defs, 
                              Ok c <- [valCat typ],  eqStrIdent c cat]
   vars = [ElemX    (TagX t) EmptyX   | t <- ["AbsX", "VarX"]] ++
          [AttlistX (TagX t) ["symb"] | t <- ["AbsX", "VarX"]]
   defs = tree2list (fst abstract)

gf2rulex :: AbstractST -> (Ident,[(Ident,Type)]) -> [RuleX]
gf2rulex abstract (elm, rules) = 
 ElemX (TagX (prt elm)) (allArgsOf rules) : 
 [ElemX (TagX (prt fun)) EmptyX | (fun,_) <- rules]
  where
   allArgsOf []    = ArgSeqX (foldl1 AltX vars)
   allArgsOf rules = ArgSeqX (foldl1 AltX (map seqOf rules ++ vars))
   seqOf (fun,typ) = foldl SeqX (AtomX (TagX (prt fun))) (map argOf cont)
                       where 
                         cont = case typeForm typ of
                            Ok (co,_,_) -> co
                            _ -> []
   argOf (_,t) = case typeForm t of
                   Ok (co@(_:_),c,_) -> ProdX (length co) (TagX (prt c))
                   _ -> AtomX (TagX (prt t))
   vars = if isUsedBound then [AtomX (TagX "VarX")] else []
   isUsedBound = True
{-
   not (null [fun | (fun,IdentFun typ) <- defs, _ <- hypIn typ])
   hypIn (Prod _ a b) = argIn a || hypIn b
   hypIn _ = False --- except for type macros ! ; normalize type first...
   argIn (Prod _ (Cons a) b) = eqStrIdent a elm || argIn b
   argIn _ = False
-}

term2elemx :: AbstractST -> Trm -> ElemX
term2elemx gr term = AbsX (map prt xx) (body (trm,args)) where
 (xx,trm,args) = case termForm term of
   Ok tf -> tf
   _ -> ([],term,[])
 body (t,a) = case (t,a) of
   (Literal c s,[]) -> PredefX (prt c, s)
   (Cons f,args) -> case analyseFun f of
      Ok (cats,cat) -> 
        AppX (TagX cat) (AppX (TagX (prt f)) [] : map tt2t (zip cats args))
      _  -> MetaX (TagX "String", "xml-generation fails for" +++ prt trm)
   (Meta (MetaSymb (c,i)),_) -> MetaX (TagX (prt c), show i)
   _  -> MetaX (TagX "String", "xml-generation fails for" +++ prt trm)
 analyseFun f = do
   typ <- lookupFun f gr
   (hyps,cat,_) <- typeForm typ
   cats <- mapM (valCat . snd) hyps
   return (map prt cats, prt cat)
 tt2t (c,tr) = case tr of
   Var x -> VarX (TagX c, prt x)
   _ -> term2elemx gr tr

