module SymbolMap(SymbolMap,indices,lookup,empty,union,toList,fromList) where
import LexSymbol
import Data.Array hiding (indices)
import Prelude hiding (lookup)
import HO(apSnd)
--import Trace(trace)

newtype SymbolMap s a = S (Array Int (Maybe (s,a)))


indices (S a) =
  if snd (bounds a)<0 then [] -- because of hbc empty array bug workaround
  else [s | Just (s,_) <- elems a]

toList (S a) = [(s,x) | Just (s, x) <- elems a]

lookup :: Symbol s => s -> SymbolMap s a -> Maybe a
lookup s (S a) = map snd (lookup' (lexToInt s) a)
lookup' i a =
    if inRange (bounds a) i
    then a!i
    else Nothing

--empty = S (array (1,0) []) -- hbc does not allow empty arrays!
empty = S (array (-1,-1) [(-1,error "empty ! -1 ")])
	 -- testing for negative upper bound instead,
	 -- assuming lexToInt s >= 0

S a1 `union` S a2 =
    if hi1<0 then S a2
    else if hi2<0 then S a1
    else S (--trace (show lohi) $ -- shows constructed array sizes
	    array lohi [(i,lookup' i a2++lookup' i a1)|i<-range lohi])
  where
    lohi = (min lo1 lo2,max hi1 hi2)
    (lo1,hi1) = bounds a1
    (lo2,hi2) = bounds a2

fromList xs = S (accumArray (const id) Nothing (lo,hi) assocs)
  where
    assocs = [(lexToInt s,Just (s,x)) | (s,x)<-xs]
    is = map fst assocs
    lo = minimum is
    hi = maximum is

instance Functor (SymbolMap a) where
  map f (S a) = S (map (map (apSnd f)) a)
