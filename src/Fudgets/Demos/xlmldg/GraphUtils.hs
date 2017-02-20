module GraphUtils(
	graphmap,       -- apply a function to all nodes and edges of a graph
        revg,	        -- reverse the arrows in a graph
	collapse,	-- collapse a graph
        collapseCycles)  -- create one node per scc
where
--import utils
import Fudgets -- utils
import ListMap(lookupWithDefault)
import List(nub)
import Array

revg g =
  let nodes = map fst g
      deps = concatMap (\(x,xs) -> map ((,) x) xs) g
  in map (\x->(x,(map fst . filter ((==x) . snd)) deps)) nodes

collapse f g=
  let gf = graphmap f g
  in let nodes = nub (map fst gf)
  in map (\n->(n,(nub . concatMap snd . filter ((==n) . fst)) gf)) nodes

graphmap = map . mapPair . pairwith map

collapseCycles (sccs,g)=
  let nsccs = number 1 sccs
      sccidtab = concatMap (\(id,ns) ->map (flip (,) id) ns) nsccs
      sccid = lookupWithDefault sccidtab (error "assoc")
      sccparttab = listArray (1,length sccs) sccs
      sccparts n = sccparttab ! n
      idg = (map . map . mapPair . pairwith (nub `oo` map)) sccid g
  in (sccparts,idg)

{-
number g =
  let nodetable = map swap (number 0 (map fst g))
  in let f n = (assoc n nodetable,n)
  in graphmap f g
-}
