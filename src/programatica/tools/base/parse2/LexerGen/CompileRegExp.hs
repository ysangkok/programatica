{-+
Compiling regular expressions, the new way.
-}
module CompileRegExp where
import RegExpOps
import FiniteMap
import Data.Maybe(isJust,fromJust)
--import Trace

compile r = number (build emptyFM [r])
  where
    build dfa [] = []
    build dfa (r:rs) =
       if isJust (lookupFM dfa r)
       then build dfa rs
       else (r,fs):build dfa' (frs++rs)
     where
       dfa' = addToFM dfa r fs
       fs = factors r
       frs = map snd (snd fs)

    number states = map numb states
      where
        mapping = listToFM (zip (map fst states) [(1::Int) ..])
	num = fromJust . lookupFM mapping

        numb (r,(b,edges)) = (num r,(b,[(t,num r)|(t,r)<-edges]))
