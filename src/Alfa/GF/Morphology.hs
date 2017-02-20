module Morphology where

import Operations
import Tokens
import Grammar
import Macros
import Parsing
import GFCFIdents
import PrGrammar
import SymbolTable
import Linearize
import Lookup
import List (sortBy)

-- construct a morphological analyser from a GF grammar. AR 11/4/2001

type Morpho = String -> (String,[String])

appMorpho :: Morpho -> String -> (String,[String])
appMorpho = ($)

mkMorpho :: (GrammarST [Str], GFCF Str) -> Morpho
mkMorpho (gr@(abstr,_), cf) str = 
  (str,concat [map (c +++) (tokf c str) | c <- allcats])
  where
    tokf c s   = tokens2form gr cf c [Str s]
    allcats    = map mkGFCFCat $ allCats abstr
    allItems c = [(f,c) | (f,t) <- funsForCat c abstr, isLex t] 
    isLex t    = case typeForm t of
       Ok ([],_,_) -> True
       _ -> False

---

type Morpho2 = BinTree (String,[String])

appMorpho2 :: Morpho2 -> String -> (String,[String])
appMorpho2 m s = case lookupTree id s m of
  Ok vs -> (s,vs)
  _ -> (s,[])

mkMorpho2 :: Token a => GrammarST a -> Morpho2 
mkMorpho2 gr@(abstr,_) = mkMorphoTree $ concat $ map mkOne allItems
  where
    mkOne (fun,cat) = 
      [(showTok a, prt fun +++"+"++ prt cat +++"+"++ concat ps) | 
                  Ok pas <- [allLinearizes' (Cons fun) gr], (ps,a) <- pas] 
    allItems = [(f,c) | c <- allCats abstr, (f,t) <- funsForCat c abstr, isLex t] 
    isLex t = case typeForm t of
      Ok ([],_,_) -> True
      _ -> False

mkMorphoTree :: Ord a => [(a,b)] -> BinTree (a,[b])
mkMorphoTree = sorted2tree . sortAssocs

sortAssocs :: Ord a => [(a,b)] -> [(a,[b])]
sortAssocs = arrange . sortBy (\ (x,_) (y,_) -> compare x y) where
  arrange ((x,v):xvs) = arr x [v] xvs
  arrange [] = []
  arr y vs xs = case xs of
    (x,v):xvs -> if x==y then arr y (v:vs) xvs else (y,vs) : arr x [v] xvs
    _ -> [(y,vs)]
    

