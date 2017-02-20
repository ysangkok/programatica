module Earley where

import Operations
import CF
import Tokens
import Grammar ---

-- AR 28/4/2000 from ML code of Thomas Hallgren (1989)

earleyParser :: (Token a, CFIdent c, CFIdent f) => 
  CF a c f -> CFCat c -> WParser a (CFTree c f)
earleyParser cf cat tt = [(parseTree2tree t,[]) | t <- pts, compat t cat] where
 pts = case earley cf cat tt of
         e:_ -> concat $ map getETrees e
         _ -> []
 getETrees (_,tt,_,_) = tt
 compat (Node ((cat',_),_)) cat = compatCF cat' cat

parseTree2tree (Node ((cat,fun),trees)) = 
  CFTree (fun,(cat, map parseTree2tree trees))

type EState a c f = 
      ((c,f), 
       [ParseTree c f],       -- (* parse trees being built (reversed) *)
       [CFItem a c],   -- (* symbols right of dot *)
       Int)

data ParseTree c f = Node ((c,f), [ParseTree c f]) --- | Leaf a 

type EStateset a c f = [EState a c f]

earley :: (Token a, CFIdent c, CFIdent f) => 
  CF a c f -> CFCat c -> [a] -> [EStateset a c f]
earley cf@(rr,lit) cat symbols = step [s0] 0 (symbols ++ [termDollar]) where

 -- mark recognized literals first
 lrr = concat (map (predefRules lit) symbols)
 cf' = (lrr ++ rr, lit)

 catDollar = mkCFIdent 0
 funDollar = mkCFIdent 0
 termDollar = readTok "0"
 s0 = [((funDollar,catDollar), [], [CFNonterm cat, mkCFTerm termDollar],-1)]

 --- step :: [EStateset a c] -> Int -> [a] -> [EStateset a c]
 step sets i [] = sets
 step ([] : sets) _ _ = [] : sets
 step (s : sets) i (c : cs) = step (si1 : si : sets) (i+1) cs where

  (si,si1) = operate [] s

  --- operate :: [CFCat c] -> EStateset a c -> (EStateset a c,EStateset a c)
  operate _ [] = ([],[])
  operate used ((state@(nt,l,r,j)) : states) = 
    afst (state :) 
      (case r of
	 [] -> operate used (states ++ complete nt (reverse l) j)
	 CFNonterm a : _  -> 
            if   elem a used
	    then operate used states
	    else operate (a : used) (states ++ predict a)
	 CFTerm a : _  -> let (si,si1) = operate used states
			    in (si,if  satRegExp a c
			           then scan state : si1
				   else si1))

  predict c = map statef (rulesFor cf' c)
  statef (fun,(cat,rhs)) = ((cat,fun), [], rhs, i)
  scan (a,l,CFTerm s : ss, i) = (a, l, ss ,i)
  --- scan (a,l,CFTerm s : ss, i) = (a, Leaf s : l, ss ,i) to collect terminals

  needs a (_,_,CFNonterm b:_,_)= compatCF a b
  needs _ _ = False

  complete (a,f) children j =
	      let movedot (b,l,_:ss,j) = (b, Node ((a,f),children):l, ss, j)
	      in if j<0
		 then []
                 else (map movedot . filter (needs a) . index (i-j)) sets
  index n l = if length l < n || n < 1 then [] else l !! (n-1) --- AR ??

  afst f (x,y)=(f x,y)

