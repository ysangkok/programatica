module Chart where

import Operations
import CF

-- AR 15/12/1999 -- 22/3/2000 -- 26/1/2001

-- chart parser ---- some signatures hidden because hbc gives "bad type reorder"

----- chartParser :: 
-----  (Eq a,Eq f,CFIdent c) => CF a c f -> CFCat c -> WParser a (CFTree c f)
chartParser cf cat ss =
 [(dotrule2tree dr, drop j ss) | 
       (0, j, dr@(_,(cat',(_,[])))) <- mkChart cf ss, compatCF cat' cat]

type Chart a c f   = [Arc a c f]
type Arc a c f     = (Int, Int, DotRule a c f) 
type DotRule a c f = (CFFun f, (CFCat c, ([CFTree c f], [CFItem a c])))

----- dotRule :: [Maybe (CFTree c f)] ->  CFRule a c f -> DotRule a c f
dotRule trees (fun,(cat,cats)) = (fun,(cat,(trees', drop (length trees) cats)))
 where trees' = [t | Just t <- trees]

----- dotrule2tree :: DotRule a c f -> CFTree c f
dotrule2tree (fun,(cat,(found,_))) = CFTree (fun, (cat,found)) --- ignoring sought

-- the parsing method: the bottom up method a` la Kilbury

mkChart :: (Eq a, Eq c, Eq f, CFIdent c) => CF a c f -> [a] -> Chart a c f
mkChart cf aa = tryAll [predict cf, combine cf, combineLit cf aa0] aa1 where
 aa1 = search cf aa0
 aa0 = mkToks aa

tryAll :: (Eq a,Eq c,Eq f,CFIdent c) => 
  [Chart a c f -> Chart a c f] -> Chart a c f-> Chart a c f
tryAll methods chart =
 let chart' = tryOnce methods chart
 in  if length chart' == length chart then chart else tryAll methods chart'
  where
   tryOnce [] ch = ch
   tryOnce (m:mm) ch =
     let ch1 = m ch
     in tryOnce mm (ch ++ ch1)

-- the first three are not specific to BUK
search :: (Eq a,Eq c,Eq f,CFIdent c) => CF a c f -> Toks a -> Chart a c f
search (cf,plit) toks = lastArcs ++ concat (map addRight toks) where 
  addRight (i,a) = 
      [(i,i+1, dotRule [Nothing] rule) | rule <- cf, forItem a rule] ++
      [(i,i+1, dotRule [Nothing] rule) | rule <- plit' a ]
   ++ [(i,i,   e) | e <- emptyRhss] ---
  emptyRhss = [(fun,(cat,([],[]))) | (fun,(cat,[])) <- cf] ---
  lastArcs = [(mx,mx, e) | e <- emptyRhss] --- []
  mx = length toks ---
  plit' = predefRules plit
--- an attempt to recognize empty productions, which seems to work...

combine ::  (Eq a,Eq c,Eq f,CFIdent c) => CF a c f -> Chart a c f -> Chart a c f
combine (cf,_) chart = 
  [arc       | (i, j, (fun, (cat, (found, CFNonterm x : xx)))) <- chart,
               (j',k, dr@(fun',(cat',(found',[]))))  <- chart,
               j'   == j,
               compatCF cat' x,
               let arc = (i,k,(fun,(cat,(found ++ [dotrule2tree dr], xx)))),
               not (elem arc chart)]

combineLit :: (Eq a,Eq c,Eq f,CFIdent c) => 
             CF a c f -> Toks a -> Chart a c f -> Chart a c f
combineLit (cf,_) toks chart = 
  [arc       | (i, j, (fun, (cat, (found, CFTerm a : xx)))) <- chart,
               (j',t)  <- toks,
               j' == j,
               satRegExp a t,
               let arc = (i,j+1,(fun,(cat,(found, xx)))),
               not (elem arc chart)]

predict ::  (Eq a,Eq c,Eq f,CFIdent c) => CF a c f -> Chart a c f -> Chart a c f
predict (cf,_) chart = concat (map tryPredict chart) where
 tryPredict (i,j,dr@(fun,(cat,(found, [])))) =
   [arc | rule@(fun',(cat', CFNonterm cat0:its)) <- cf,
          compatCF cat0 cat,
          let arc = (i,j,dotRule [Just (dotrule2tree dr)] rule),
          not (elem arc chart) ]
 tryPredict _ = []

