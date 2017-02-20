module BottomUp where

import Operations
import Grammar
import SymbolTable
import Macros
import Update
import Lookup
import Refine
import ComputeTerm

import TC () -- hbc (but not hugs) requires this for an implicit type synonym

-- functions on given type, with possible place position combinations

functionsOnType :: AbstractST -> Type -> [((Fun,Type),[Int])]
functionsOnType st ty =
  [((f,typ),ps) | (f, IdentFun typ) <- infos, 
                  Ok (cont,_,_) <- [typeForm typ],
                  ps@(_:_) <- possibleFor ty cont]
 where
   infos = tree2list (fst st)  --- not efficient to do every time
   possibleFor ty cont = 
     parts [i | (t,i) <- zip (map snd cont) [1..], unifiableTypes st ty t]
   parts [] = [[]]
   parts (x:xs) = map (x:) pxs ++ pxs where pxs = parts xs

unifiableTypes st x y = consistentTerms (compute st x) (compute st y) 
--- does not go under abstraction

-- wrap a given term in such a function

mkWrappedTerm :: [MetaSymb] -> Trm -> ((Fun,[Int]),Cont) -> Err Trm
mkWrappedTerm oldmetas trm ((fun,places),args) = do
  wrapt0    <- mkIncomplTrm [] args (Cons fun)   -- [] means extern bound variables
  let wrapt  = refreshMetasInTrm oldmetas wrapt0
  (zz,f,xx) <- termForm wrapt
  let xx'    = map mkBody (zip [1..] xx)
  return $ mkTerm (zz,f,xx')
 where
   mkBody (i,x) = if elem i places then trm else x

wrapTermInfoWithFun :: AbstractST -> TermInfo -> ((Fun,Type),[Int]) -> Err TermInfo
wrapTermInfoWithFun abs ti ((fun,typ),ps) = do
  (args,cat,_) <- typeForm typ
  cont         <- lookupCat cat abs
  let (tr,ty,co,qs,cs) = infoOfTI ti
  let metas     = metaSymbsOfTI ti
  typ          <- mkIncomplTrm [] cont (Cons cat)
  trm          <- mkWrappedTerm metas tr ((fun,ps),args)
  checkTermInfo abs $ mkTermInfo trm typ co qs cs

