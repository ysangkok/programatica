module Cycles collapseCycles where
import utils

collapseCycles (sccs,g)= map (collapseLevel sccs) g

collapseLevel sccs gs =
  let levelsccs = filter (not . null . intersect (map fst gs)) sccs
  in let sccs = map (sccnode gs) levelsccs
  in map (sccnode gs) levelsccs

sccnode g ns = (ns,(unionmap snd . filter (\(x,_)->x `elem` ns)) g)
