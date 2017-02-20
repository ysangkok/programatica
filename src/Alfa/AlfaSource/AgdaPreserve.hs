module AgdaPreserve where
import CSyntax
import SwapMetaVars
import RemMetaVarsThomas
import MetaVars(MetaVar)
import NonStdTrace(trace)
import Agda(ppAll)

-- try to preserve comments from orig but obtain meta vars from mutated
preserve orig mutated = 
     if length ms == length ms'
     then orig'
     else trace (unlines
		 ["Sorry, comments and some syntactic sugar was lost...",
		  show (length ms,length ms')--,
		  --ppAll orig,
		  --ppAll mutated
		  ])
		 $
          mutated -- no 1-1 correspondence, fail to preserve comments
  where
    mutated' = rmGeneratedDecls orig mutated
    (_,ms) = remMetaVars mutated'
    (orig',ms') = swapMetaVars orig (ms++repeat 0)

    rmGeneratedDecls orig mutated = mutated -- !! could do better...
