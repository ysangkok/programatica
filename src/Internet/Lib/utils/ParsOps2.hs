module ParsOps2 where
import Utils2

{-
#ifndef __HASKELL98__
#define fmap map
#endif
-}

infixl 3 `ap`, `chk`, `cap`, `bind`
infixl 2 `orelse`

-- "Parser t a" is a parser that parses a sequence of "t":s into an "a".
newtype Parser t a = P ([t]->(Either String (a,[t]),[t]))
  -- input->(error_msg or (result,remaining input),high water mark)

parse (P p) ts = case p ts of
		   (Right (x,[]),_) -> Right x
		   (Right (x,garb),hwm) -> Left ("trailing garbage",hwm) -- !!
		   (Left msg,hwm) -> Left (msg,hwm)

tstparse (P p) ts = p ts

un (P p) = p

unit :: a->Parser t a
unit x = P (\ts->(Right (x,ts),ts))

failP msg = P (\ts->(Left msg,ts))

bind :: Parser t a -> (a -> Parser t b) -> Parser t b
bind (P ap) atobp=
	P (\ts->case ap ts of
		  (Right (a,ts'),hwm1) -> 
		     --un (atobp a) ts'
		     --{-
		     case un (atobp a) ts' of
		       (r,hwm2) -> (r,shortest hwm1 hwm2) -- hmm
		       
		     --}
		  (Left msg,hwm) -> (Left msg,hwm))

mapP :: (a->b) -> Parser t a -> Parser t b
mapP f (P ap) = P (\ts -> case ap ts of
			    (Right (a,ts'),hwm) -> (Right (f a,ts'),hwm)
			    (Left msg,hwm) -> (Left msg,hwm))

ap :: Parser t (a->b)->Parser t a->Parser t b
ap abp ap = abp `bind` (`mapP` ap)

chk :: Parser t b->Parser t c->Parser t b
chk bp cp = const `mapP` bp `ap` cp

cap bp cp = unit id `chk` bp `ap` cp

orelse :: Parser t a -> Parser t a -> Parser t a
orelse (P a1p) (P a2p) =
  P (\ts->case a1p ts of
	    e1@(Left _,hwm1) ->
	      case a2p ts of
	        (a@(Right _),hwm2) -> (a,shortest hwm1 hwm2)
		e2@(Left _,hwm2) ->
		  if hwm1 `shorter` hwm2 then e1 else e2
	    a1 -> a1)
	    -- a1p is assumed to parse a longer segment than a2p

shortest ts1 ts2 = if shorter ts1 ts2 then ts1 else ts2
shorter ts1 ts2 = length ts1 < length ts2

token :: Parser t t
token = P tokenp
  where tokenp (t:ts) = (Right (t,ts),ts)
        tokenp []     = (Left "unexpected end of file",[])

tokens n = P tokensp
  where tokensp ts =
          case splitAt n ts of
	    (ts1,ts2) -> if length ts1==n
	                 then (Right (ts1,ts2),ts2)
			 else (Left ("wanted "++show n++" bytes, got only "++show (length ts1)),[])

peek :: Parser t t
peek = P peekp
  where peekp ts@(t:_) = (Right (t,ts),ts)
	peekp []       = (Left "unexpected end of file",[])

{-
eof :: Parser t ()
eof = P eofp
  where eofp [] = (Right ((),[]),[])
        eofp ts = (Left "end of file expected",ts)
-}

theRest :: Parser t [t]
theRest = P therest
  where therest ts = (Right (ts,[]),[])

scan :: (t->Bool) -> Parser t t
scan = scan' "unexpected token"
scan' msg p = P scanp
	where scanp (t:ts) | p t = (Right (t,ts),ts)
	      scanp ts = (Left msg,ts)

lit' t = scan (t==)
lit t = scan' ("expected "++show t)  (t==)

lits [] = unit []
lits (t:ts) = unit (:) `ap` lit t `ap` lits ts

many p = some p `orelse` unit []

some p = unit (:) `ap` p `ap` many p

optional x p = p `orelse` unit x

maybeP p = Just `mapP` p `orelse` unit Nothing

somesep sep p = unit (:) `ap` p `ap` many (sep `cap` p)
manysep sep p = optional [] (somesep sep p)

cut = id -- ??

filterP :: Parser t a -> (a->Bool) -> Parser t a
ap `filterP` p = ap `bind` \ a -> if p a then unit a else failP "filterP"

repeatP 0 p = unit []
repeatP n p = (:) `mapP` p `ap` repeatP (n-1) p

eitherP lP rP = mapP Left lP `orelse` mapP Right rP

----
instance Monad (Parser t) where
  (>>=) = bind
  return = unit

instance Applicative (Parser t) where
  pure  = return
  (<*>) = ap

instance Functor (Parser t) where
  fmap = mapP
