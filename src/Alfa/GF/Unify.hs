module Unify where

import Operations
import Tokens ()
import Grammar
import ComputeTerm
import SymbolTable
import Macros
import PrGrammar
import Predefined

-- (c) Petri Mäenpää & Aarne Ranta, 1998--2001

type Unifier = [(MetaSymb, Trm)]
type Constrs = [(Trm, Trm)] 

unifyAll :: Constrs -> Unifier -> (Unifier,Constrs)
unifyAll [] g = (g, [])
unifyAll ((a@(s, t)) : l) g =
  let (g1, c) = unifyAll l g
  in case unify s t g1 of
       Ok g2  -> (g2, c)
       _      -> (g1, a : c)

unify :: Trm -> Trm -> Unifier -> Err Unifier
unify e1 e2 g = 
 case (e1, e2) of 
  (Meta s, t) -> do
     tg <- subst_all g t
     let sg = maybe e1 id (lookup s g)
     if (sg == Meta s) then extend g s tg else unify sg tg g 
  (t, Meta s) -> unify e2 e1 g
  (Cons a, Cons b) | (a == b)      -> return g
  (Literal _ _, Literal _ _) | e1 == e2  -> return g
  (Var x, Var y) | (x == y)        -> return g 
  (Abs x b, Abs y c)               -> do let c' = substTerm [x] [(y,Var x)] c 
                                         unify b c' g
  (App c a, App d b)            -> case unify c d g of 
                                     Ok g1 -> unify a b g1 
                                     _       -> prtBad "fail unify" e1
  _                             -> prtBad "fail unify" e1

extend :: Unifier -> MetaSymb -> Trm -> Err Unifier
extend g s t | (t == Meta s) = return g  
             | occCheck s t  = prtBad "occurs check" t
             | True          = return ((s, t) : g) 

subst_all :: Unifier -> Trm -> Err Trm
subst_all s u =
 case (s,u) of
  ([], t)  -> return t
  (a : l, t) -> do
     t' <- (subst_all l t) --- successive substs - why ?
     return $ substMetas [a] t'

substMetas :: [(MetaSymb,Trm)] -> Trm -> Trm
substMetas subst trm = case trm of
   Meta x -> case lookup x subst of
     Just t -> t
     _ -> trm
   _ -> composSafeOp (substMetas subst) trm

occCheck :: MetaSymb -> Trm -> Bool
occCheck s u = case u of
    Meta v  -> s == v
    App c a -> occCheck s c || occCheck s a
    Abs x b -> occCheck s b
    _       -> False

