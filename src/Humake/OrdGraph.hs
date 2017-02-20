module OrdGraph where
import Prelude hiding (reverse)
import List(partition)
import OrdMap(OrdMap)
import qualified OrdMap as OM
import OrdSet2(OrdSet)
import qualified OrdSet2 as OS
import List(partition)
import Utils3(swap)

#ifdef __HASKELL98__
#define map fmap
#endif

--- An abstract type for directed graphs. ------------------------------------

--data OrdGraph a

--{-
--- Constructing/Modifying graphs --------------------------------------------
empty :: OrdGraph node
addNode :: Ord node => (node,[node]) -> OrdGraph node -> OrdGraph node
remNode :: Ord node => node -> OrdGraph node -> OrdGraph node
addEdge, remEdge :: Ord node => node -> node -> OrdGraph node -> OrdGraph node
addEdges, remEdges :: Ord node => node -> OrdSet node -> OrdGraph node -> OrdGraph node

replaceEdges :: Ord node => node -> OrdSet node -> OrdGraph node -> OrdGraph node
-- replaceEdges n ns g = addEdges n ns (remEdges n (neighbours g n)) g

--- Transforming graphs ------------------------------------------------------
transitiveClosure, reverse :: Ord node => OrdGraph node -> OrdGraph node

--- Inspecting graphs --------------------------------------------------------
neighbours, reachable :: Ord node => OrdGraph node -> node -> OrdSet node
revNeighbours, revReachable :: Ord node => OrdGraph node -> node -> OrdSet node

-- reachable = neighbours . transtiveClosure
-- revNeighbours = neighbours . reverse

topologicalSort :: Ord node => OrdGraph node -> [[node]]

--- Conversions to/from graphs -----------------------------------------------
fromList :: Ord node => [(node,OrdSet node)] -> OrdGraph node
toList :: OrdGraph node -> [(node,OrdSet node)]
fromEdges :: Ord node => [(node,node)] -> OrdGraph node
edges :: Ord node => OrdGraph node -> [(node,node)]
---}

-- ============================================================================
-- = Implementation ===========================================================
-- ============================================================================

newtype OrdGraph node = G (OrdMap node (OrdSet node)) deriving (Show)
-- Representing a graph as a map from nodes to neighbours (outgoing edges).
-- Could also store incoming edges and/or transitive closure.

unG (G g) = g

--- Constructing/Modifying graphs --------------------------------------------

empty = G OM.empty

addNode (n,ns) = G . OM.add (n,OS.fromList ns) . unG
--remNode n = modNode n (const OS.empty) -- !! doesn't remove n from the map
remNode n =
  G . OM.fromList . filter ((/=n).fst) . OM.toList . unG -- slow, O(n*log n)
addEdge n = modNode n . OS.add
remEdge n = modNode n . OS.delete
addEdges n = modNode n . OS.union
remEdges n = modNode n . flip OS.difference

replaceEdges n = modNode n . const

--- Transforming graphs -----------------------------------------------------
transitiveClosure (G imap) = G tmap
   where
     tmap = {-OM.a-}map tnbrs imap
--     tmap = OM.amap tnbrs imap
     tnbrs ns = OS.union ns (foldr (OS.union.lookupSet tmap)
                                 OS.empty
				 (OS.toList ns))
     -- This should work for acyclic graphs (simple inductive proof).
     -- Cycles -> nontermination or black holes.

reverse = fromEdges . map swap . edges
-- wasteful if only one node is the reverse graph is inspected...

--- Inspecting graphs -------------------------------------------------------
neighbours = lookupSet.unG

reachable = neighbours . transitiveClosure
revNeighbours = neighbours . reverse
revReachable = neighbours . transitiveClosure . reverse

topologicalSort = tsort . OM.toList . unG
  where
    tsort nes =
      case partition (OS.null.snd) nes of
        ([],[]) -> []
        ([],_) -> error "OrdGraph.topologicalSort failed because of cycles in the graph"
        (ns,rest) -> map fst ns:tsort [(n,OS.difference es skip) | (n,es)<-rest]
	  where skip = foldr (OS.union.snd) OS.empty ns

--- Conversions to/from graphs -----------------------------------------------
fromEdges = foldr (uncurry addEdge) empty
fromList = G . OM.fromList
toList = OM.toList . unG
edges g = [ (n,n') | (n,ns)<-OM.toList (unG g), n'<-OS.toList ns ]

-- auxiliary function
modNode :: Ord node => node -> (OrdSet node->OrdSet node) -> OrdGraph node -> OrdGraph node
modNode n f (G g) = G (OM.add (n,f (lookupSet g n)) g)

lookupSet m i =
  case OM.lookup i m of
    Nothing -> OS.empty
    Just s -> s
