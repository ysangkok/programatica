module LLParsOps1
  (DParser,parseDP,emptyDP,symbolDP,anySymbolDP,altDP,seqDP,nonEmptyDP)
   where
import Data.List(union,intersect)
import Data.Maybe(isJust)

-- Deterministic parsing combinators, using ideas described by Swierstra,
-- http://www.cs.ruu.nl/people/doaitse/
-- Also in Springer Verlag, series LNCS, volume 1129.

data DParser i o
  = P { empty::Bool, -- Does the parser accepts the empty set?
        first::First i,  -- The "first set" of the parser
	parse:: ParserFun i o }

type Input i = [i]
type Follow i = [i]
type First i = [i]
type ParserFun i o = Input i->Follow i->Either (Error i) (o,Input i)
type Error i = ([i],[i]) -- expected, got

parseDP (P {parse}) is = parse is []

emptyPF x is fs = Right (x,is)

anySymbolPF is' is fs =
    case is of
      [] -> err
      i:is -> if i `elem` is'
	      then Right (i,is)
	      else err
  where err = Left (is',is)

emptyDP x = P True [] (emptyPF x)
anySymbolDP is = P False is (anySymbolPF is)
symbolDP i = anySymbolDP [i]

P e1 f1 p1 `altDP` P e2 f2 p2 = P altE altF altP
  where
    altE = e1||e2
    altF = if null ambiguity
	   then f1++f2
	   else error (unlines ["Grammar is not LL(1): ",
			        show ambiguity,show f1,show f2])
      where ambiguity = intersect f1 f2

    altP is fs =
	case is of
	  [] ->
	     if e1 then p1 is fs
	     else if e2 then p2 is fs
	     else err
	  i:_->
	     if i `elem` f1 then p1 is fs
	     else if i `elem` f2 then p2 is fs
	     else if e1 && i `elem` fs then p1 is fs
	     else if e2 && i `elem` fs then p2 is fs
	     else err
      where
	err = Left (combine altE altF fs,is) -- hmm

P e1 f1 p1 `seqDP` ~(P e2 f2 p2) = P seqE seqF seqP
  where
    seqE = e1&&e2
    seqF = combine e1 f1 f2
    seqP is fs =
     do (x1,is') <- p1 is (combine e2 f2 fs)
        (x2,is'') <- p2 is' fs
	return (x1 x2,is'')

combine e xs ys = xs `union` (if e then ys else [])

-- To be able to use the convenience of the "do" notation in seqDP:
instance Monad (Either e) where
  return = Right
  xe >>= ye =
	case xe of
	  Left e -> Left e
	  Right x -> ye x

nonEmptyDP s ~(P e f p) =
  P (if e then error ("Emtpy parser in "++s) else e)
    f p
