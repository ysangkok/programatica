module Paraphrases (mkParaphrases) where

import Operations
import Grammar
import Lookup (allDefs)
import Macros (mkApp, eqStrIdent)
import SymbolTable
import ComputeTerm
import List (nub)

-- paraphrases of GF terms. AR 6/10/1998 -- 24/9/1999 -- 5/7/2000
-- Copyright (c) Aarne Ranta 1998--99, under GNU General Public License (see GPL)
-- thus from the old GF. Incomplete and inefficient...

mkParaphrases :: AbstractST -> Trm -> [Trm]
mkParaphrases st = nub . paraphrases (allDefs st)

paraphrases :: [Definition] -> Trm -> [Trm]
paraphrases th t =
  t :
  paraImmed th t ++
  paraMatch th t ++
  case t of
    App c a -> [App d b | d <- paraphrases th c, b <- paraphrases th a]
    Abs x b -> [Abs x d | d <- paraphrases th b]
    c       -> []

paraImmed :: [Definition] -> Trm -> [Trm]
paraImmed defs t = 
  [Cons f | (PattCons f [], u) <- defs, t == u] ++ --- eqTerm
  case t of
    Cons c -> [u | (PattCons f [], u) <- defs, eqStrIdent f c]
    _      -> []

paraMatch :: [Definition] -> Trm -> [Trm]
paraMatch th@defs t = 
 [mkApp (Cons f) xx | (PattCons f zz, u) <- defs, 
                      let (fs,sn) = fullApp u, fs == h, length sn == length zz] ++
 case findAMatch defs t of
   Ok (g,b) -> [substTerm [] g b]
   _        -> []
  where 
    (h,xx)    = fullApp t
    fullApp c = case c of 
                  App f a -> (f', a' ++ [a]) where (f',a') = fullApp f
                  c       -> (c,[])

