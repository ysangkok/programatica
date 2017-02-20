module TopDown where

import Operations
import Parsers
import CF
import Tokens
import Grammar

-- AR 28/4/2000
-- resursive descent combinator parser. Code modified from old GF.

--topdownParser :: (Token a, CFIdent c) => CF a c -> CFCat c -> WParser a (CFTree c)
topdownParser cf@(rr,lit) cat =
   parses (remainingInput .>. \ tt -> pCfTree (cf' tt) cat)
 where
   cf' tt = (concat (map (predefRules lit) tt) ++ rr, lit)

pCfTree :: (Token a, CFIdent c, CFIdent f) => CF a c f -> c -> Parser a (CFTree c f)
pCfTree env@(_,predef) cat =
   if   leftRecs net cat == []
   then pNet     env net *?* (makeCfTree env) 
   else pClosure env net cat  
     where 
       net = buildNet (selectRules env cat)

-- leftRecs :: (Token a, CFIdent c) => Net (ParsInfo a c) -> c -> [Int]
leftRecs net@(_,start,trans) cat = 
  [m | (n, NS c, m) <- trans, n == start, compatCF c cat]

--pNet :: (Token a, CFIdent c) => 
--  CF a c -> Net (ParsInfo a c) -> Parser a [Either (CFTree c) (c,c)]
pNet env net@(fin,start,trans) = foldr (|||) status paths where
  status = if elem start fin then succeed [] else fails "<pNet>"
  paths  = [pSimple env pi (pNet env (fin,nx,trans)) | (k,pi,nx) <- trans, k==start]

-- pClosure ::  (Token a, CFIdent c) => 
--               CF a c -> Net (ParsInfo a c) -> c -> Parser a (CFTree c)
pClosure env net@(fin,start,trans) cat = 
 (pNet env (fin,start,goodpaths) *?* makeCfTree env) .>. closure afterC
  where
   goodpaths  = [(n,c,m) | (n, c, m) <- trans, n /= start || distIt c cat]
   distIt (NS c) c' = not (compatCF c c')
   distIt _ _ = True
   afterC x   = pNet env (fin,fromC,trans) .>.
                       (\y -> succeed (Left x : y) *?* makeCfTree env)
   fromC      = case leftRecs net cat of m:_ -> m
                                         []  -> error "no left recursion"

{-pSimple :: (LexPos a,Token a, CFIdent c) => 
    CF a c -> ParsInfo a c -> Parser a [Either (CFTree c) (c,c)]
                           -> Parser a [Either (CFTree c) (c,c)]-}
pSimple env@(_,cf) pi parser =
 case pi of
   NS cat -> pCfTree env cat  .>. (\x -> parser .>. (\y -> succeed (Left x:y)))
   TS str -> literOptTok str  .>. (\x -> parser .>. (\y -> succeed y))
   FS catfun ->                                            succeed [Right catfun]

--literOptTok :: Token a => RegExp a -> Parser a a
literOptTok (RegAlts [a]) | isZeroTok a  = succeed a
literOptTok s  = satisfy (satRegExp s)

-- makeCfTree :: (Token a, CFIdent c) => 
--  CF a c -> [Either (CFTree c) (c,c)] -> Maybe (CFTree c)
makeCfTree cf result = Just (CFTree (funct, (cat, args)))
  where
   (cat,funct)  = case last result of Right f -> f
   args         = [x | Left x <- result]

data ParsInfo a c f = NS c | TS (RegExp a) | FS (c,f) deriving (Eq,Show)

-- collect the parsing info patterns for each category
-- selectRules :: (Token a, CFIdent c) => CF a c -> c -> [[ParsInfo a c]]
selectRules cf cat =
 [map pinf its ++ [FS (cat,fun)] | (fun,(_,its)) <- rulesFor cf cat] 
  where
   pinf (CFNonterm c) = NS c
   pinf (CFTerm s) = TS s



-- auxiliaries

--- import Tries as T
--- type Net a = T.T a ()
--- 
--- acceptByNet :: Eq a => Net a -> [a] -> Bool
--- acceptByNet = not null . snd . T.acc
--- 
--- buildNet :: Eq a => [[a]] -> Net a
--- buildNet ws = T.build [ (w,[()]) | w <- ws]

type Net a  =
  ([Int],         -- set of final states 
   Int,           -- initial state
   [(Int,a,Int)]) -- transitions between states

acceptByNet :: Eq a => Net a -> [a] -> Bool
acceptByNet net@(fsts,st0,arcs) list = accNet st0 list where 
 accNet st l =
  case l of
    []  -> st `elem` fsts
    x:y -> case lookup (st,x) transs of
               Just st' -> accNet st' y
               _        -> False
 transs = [((st,x),st') | (st,x,st') <- arcs]

buildNet :: Eq a => [[a]] -> Net a
buildNet [] = ([],0,[])
buildNet (t:l) = addtonet t (buildNet l)

addtonet :: Eq a => [a] -> Net a -> Net a
addtonet t (fin,o,trs) =
 atn t o (fin,trs)
  where
   atn t n (fin',trs') =
    case t of 
      []    -> if elem n fin then (fin,o,trs) else (n:fin,o,trs)
      (c:l) -> case possible c n trs' of []    -> continue t n (fin,o,trs)
                                         (m:_) -> atn l m (fin',(red n trs'))
   possible c n trs  = [m       | (k,d,m) <- trs, d==c, k == n]
   red n trs         = [(k,d,m) | (k,d,m) <- trs,       k /= n]
   continue t n (fin,o,trs) = 
     (fin',o,trs')
       where s0 = maximum (0:[s | (_,_,s) <- trs])
             l0 = length t
             fin' = (s0 + l0) : fin
             trs' = [(u,c,v) | 
                    (u,(c,v)) <- zip (n:[s0+1..]) (zip t [s0+1..])] ++ trs

