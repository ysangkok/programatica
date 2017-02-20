module TopDown2 (topdownParser2) where
 
import CF
--import Operations
import Parsers
import Tokens (Token)

import List(nub)
import Maybe(fromMaybe)
import HO(apFst)

--import Trace(trace)
--tr s x = trace (s++": "++show x) x
--fails' s = trace s (fails s)

-- A simple top-down parser that allows left recursion /TH 2000-11-22

--topdownParser2 :: (Token a, CFIdent c) => CF a c -> CFCat c -> Parser a (CFTree c)
topdownParser2 cf@(rr,lit) cat =
    --trace ("topdownParser2 "++show cat) $
    parses (remainingInput .>. \ tt -> pCfTree1 (rr' tt) cat)
  where
    rr' tt = numberRules (concat (map (predefRules lit) tt) ++ rr) -- instert a nub?

{-
-- Simple version, without left factorisation
pCfTree0 nrules = pCat
  where
    pCat = lookupCat (fails "<TopDown2a>") pcats
    pcats = [(cat,pCat' cat)|cat<-cfCats nrules] -- memoisation

    pCat' = foldl (|||) (fails "<TopDown2b>") . map pOneRule . rulesFor' nrules
    pOneRule (i,(fun,(c,its))) =
      rule i (pCFItems its *** (\xx -> CFTree (fun,(c,xx))))
    pCFItems its =
      case its of
	[]              -> succeed []
	CFTerm reg  :tt -> pTerm reg +.. pCFItems tt
	CFNonterm c :tt -> pCat c ... pCFItems tt *** uncurry (:)
--}

-- Better version, *with* left factorisation
pCfTree1 nrules = pCat
  where
    pCat = lookupCat (fails "<TopDown2a>") pcats
    pcats = [(cat,pCat' cat)|cat<-cfCats nrules] -- memoisation

    pCat' = pRules . rulesFor' nrules
    pRules = pRest []
    pRest rprefix [] = fails "<TopDown2b>"
    pRest rprefix rules@((i,(fun,(c,its))):rules') =
      case its of
        [] ->  pRest rprefix rules'
	       ||| succeed (CFTree (fun,(c,reverse rprefix)))
	CFTerm reg:_ ->
	  case mapPartition (sameTerm reg) rules of
	    (same,other) ->
	      pTerm reg +.. pRest rprefix same
	      ||| pRest rprefix other
	CFNonterm c:_ ->
	  case mapPartition (sameNonterm c) rules of
	    (same,other) ->
	      rule' i (pCat c .>. \ tc -> pRest (tc:rprefix) same)
	      ||| pRest rprefix other

    sameTerm reg (i,(fun,(c,CFTerm reg':its))) | reg'==reg =
      Just (nomark,(fun,(c,its)))
    sameTerm _ _ = Nothing

    sameNonterm c (i,(fun,(rc,CFNonterm c':its))) | compatCF c' c =
      Just (nomark,(fun,(rc,its)))
    sameNonterm _ _ = Nothing

---- Assign unique numbers to the rules of the grammar
numberRules =  zip [(1::Int)..]

-- mark rules (0 means no need to mark)
nomark = 0::Int
rule' 0 = id
rule' n = rule n

---- Operations on grammars with numbered rules:
rulesFor' rules c = [r | r@(_,(_,(c',_))) <- rules, compatCF c' c]

-- List of categories in a grammar:
cfCats :: Eq c => [(a, (b, (c, d)))] -> [c]
cfCats = nub . map (fst . snd . snd)
         -- nubBy compatCF ?

---

lookupCat def pcats cat =
  case [p |(c,p)<-pcats,compatCF c cat] of
    [] -> def
    p:_ -> p

---

pTerm :: Token a => RegExp a -> Parser a a
pTerm = satisfy . satRegExp

---
--mapPartition :: (a->Maybe b) -> [a] -> ([b],[a])
mapPartition f [] = ([],[])
mapPartition f (x:xs) =
    case f x of
      Nothing -> (ys,x:xs')
      Just y -> (y:ys,xs')
  where
    (ys,xs') = mapPartition f xs
