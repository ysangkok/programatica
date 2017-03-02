module LLParsOps
  (DParser,parseDP,emptyDP,symbolDP,anySymbolDP,altDP,seqDP,nonEmptyDP)
   where
import Data.List(union,intersect)
import HO(apFst)
import qualified OrdMap as T -- selected table implementation
--import qualified SymbolMap as T -- selected table implementation
import Debug.Trace(trace)

-- The OrdMap solutions seems slightly faster than SymbolMap

type Table i a = T.OrdMap i a -- selected table implementation
--type Table i a = T.SymbolMap i a -- selected table implementation

-- Deterministic parsing combinators, using ideas described by Swierstra,
-- http://www.cs.ruu.nl/people/doaitse/
-- Also in Springer Verlag, series LNCS, volume 1129.

data DParser i o
  = P { empty::Empty o, -- Result, if the parser accepts the empty set?
	table::Table i (ParserFun i o)
      }

data Empty a
  = Nonempty
  | Empty a
--  | Insert (a,[String])

isEmpty (Empty _) = True
isEmpty _ = False

first = T.indices . table

type Input i = [i]
type Follow i = [i] -- the follow set == tokens that can be accepted next
type NoSkip i = [[i]] -- tokens that can be accepted sometime in the future
type First i = [i]
--type ParserFun i o = Input i->Follow i->Either (Error i) (o,Input i)
type ParserFun i o = Input i->Follow i->Errors i->(Maybe o,Input i,Errors i)
--type Error i = ([i],[i]) -- expected, got
type Errors i = [Error i]
type Error i = (Input i,First i,Maybe String) -- got, expected, action

parseDP p is = choose (empty p) (table p) is [] []

choose pE pT is fs es =
    case is of
      [] ->
	case (pE,T.toList pT) of
	   (Nonempty,[(i,pf)]) ->
		trace ("Inserted "++show i++" at eof") $
		pf (i:is) fs es
	   _ -> empty
      i:is' ->
	case T.lookup i pT of
 	  Just pf -> dbgmsg "accepted input" $ pf is fs es
	  Nothing -> --if (i `elem`) `any` fs
		     if i `elem` fs
		     then dbgmsg "using empty rule" $
		     	  empty
		     else msg ("deleting "++show i) $
			  choose pE pT is' fs es
		     	  
  where
    empty =
      case pE of
	Empty x -> (Just x,is,es)
--	Insert (x,msg) -> trace (unlines msg) $ Right (x,is) -- !!
	Nonempty -> err
    err = (Nothing,is,(is,T.indices pT,Nothing):es)
    dbgmsg action = id -- msg fs actiona
    msg action (o,is',es) =
	(o,is',(is,T.indices pT,Just action):es)
{-
	trace (unlines ["Input: "++show i,
			"First set: "++show (T.indices pT),
			"Follow or noskip set: "++show fs,
			"Action: "++action])
-}

symbolPF (i:is) fs es = (Just i,is,es)


emptyDP x = P (Empty x) T.empty
anySymbolDP is = P Nonempty (T.fromList [(i,symbolPF)|i<-is])
symbolDP i = anySymbolDP [i]

-- Error recovery:
--p `errDP` x =  p `altDP` insertDP x
--  where insertDP x = P (Insert x) T.empty

P e1 t1 `altDP` P e2 t2 = P altE altT
  where
    altE =
	case (e1,e2) of
	  (Nonempty, Nonempty) -> Nonempty
	  (Empty _,  Empty _) -> ambiguous
--	  (Insert _, Insert _) -> ambiguous
	  (_       , Empty y ) -> Empty y
	  (Empty x,  _       ) -> Empty x
--	  (Insert x, Nonempty) -> Insert x
--	  (Nonempty, Insert y) -> Insert y
      where ambiguous =
		error (unlines ["Ambiguous empty parsers:",
				show f1,show f2])
    altT = if null ambiguity
	   then T.union t1 t2
	   else error (unlines ["Grammar is not LL(1) (in alternative): ",
			        show ambiguity,show f1,show f2])

    ambiguity = intersect f1 f2
    f1 = T.indices t1
    f2 = T.indices t2

P e1 t1 `seqDP` ~(P e2 t2) = P seqE seqT
  where
    seqE = case (e1,e2) of
	     (Nonempty,_) -> Nonempty
	     (_,Nonempty) -> Nonempty
	     (Empty x,Empty y) -> Empty (x y)
--	     (Empty x,Insert (y,ymsg)) -> Insert (x y,ymsg)
--	     (Insert (x,xmsg),Empty y) -> Insert (x y,xmsg)
--	     (Insert (x,xmsg),Insert (y,ymsg)) -> Insert (x y,xmsg++ymsg)

    seqT =
	case e1 of
	  Empty f -> seqT2 f `T.union` seqT1
		 -- Entries from t1 have priority over entries from t2
	  _ -> seqT1

    seqT1 = fmap seqPF t1
      where seqPF p1 is fs es0 = (r,is'',es2)
	       where
		 r = do x <- optx; y <- opty; return (x y)
		 (optx,is',es1) =
			p1 is (combine (isEmpty e2) (T.indices t2) fs) es0
			--p1 is (T.indices t2: fs)
		 (opty,is'',es2) = choose e2 t2 is' fs es1

    seqT2 f =
	if null ambiguity
	then fmap pf t2
	else {-trace (unlines ["Grammar is not LL(1) (in sequence): ",
			      show ambiguity,show f1,show f2]) $-}
	     fmap pf t2
      where
        pf p is fs es = f' (p is fs es)
	  where f' (o,is,es) =
			       (fmap f o,is,es)
	ambiguity = intersect f1 f2
	f1 = T.indices t1
	f2 = T.indices t2

combine e xs ys = xs `union` (if e then ys else [])

{-
-- To be able to use the convenience of the "do" notation in seqDP:
instance Monad (Either e) where
  return = Right
  xe >>= ye =
	case xe of
	  Left e -> Left e
	  Right x -> ye x

instance Functor (Either e) where
  fmap f (Left e) = Left e
  fmap f (Right x) = Right (f x)
-}

nonEmptyDP s ~(P e t) =
  P (if isEmpty e then error ("Emtpy parser in "++s) else e)
    t
