module IntGraph where
import IntMap
import IntSet
--import Maybe
import Utils(swap,uncurry)

--- An abstract type for directed graphs. ------------------------------------

--data IntGraph
type Node = Int
type NodeSet = IntSet

--{-
--- Constructing/Modifying graphs --------------------------------------------
iEmptyGraph :: IntGraph
iAddNode,iRemNode :: Node -> IntGraph -> IntGraph -- Is iAddNode unnecessary ?
iAddEdge, iRemEdge :: Node -> Node -> IntGraph -> IntGraph
iAddEdges, iRemEdges :: Node -> NodeSet -> IntGraph -> IntGraph

iReplaceEdges :: Node -> NodeSet -> IntGraph -> IntGraph
-- iReplaceEdges n ns g = iAddEdges n ns (iRemEdges n (iNeighbours g n)) g

--- Transforming graphs ------------------------------------------------------
iTransitiveClosure, iReverse :: IntGraph -> IntGraph

--- Inspecting graphs --------------------------------------------------------
iNeighbours, iReachable :: IntGraph -> Node -> NodeSet
iRevNeighbours, iRevReachable :: IntGraph -> Node -> NodeSet

-- iReachable = iNeighbours . transtiveClosure
-- iRevNeighbours = iNeighbours . iReverse

iTopologicalSort :: IntGraph -> [[Node]]

--- Conversions to/from graphs -----------------------------------------------
edgeListToIntGraph :: [(Node,Node)] -> IntGraph
intGraphToEdgeList :: IntGraph -> [(Node,Node)]
---}

--============================================================================
--= Implementation ===========================================================
--============================================================================

newtype IntGraph = G (IntMap IntSet)
-- Representing a graph as a map from nodes to neighbours (outgoing edges).
-- Could also store incoming edges and/or transitive closure.

unG (G g) = g

--- Constructing/Modifying graphs --------------------------------------------

iEmptyGraph = G iEmptyMap

iAddNode n = G . iAddItem n iEmpty . unG
--iRemNode n = iModNode n (const iEmpty) -- !! doesn't remove n from the map
iRemNode n =
  G . listToIntMap . filter ((/=n).fst) . intMapToList . unG -- slow, O(n*log n)
iAddEdge n = iModNode n . iAddElem
iRemEdge n = iModNode n . iRemElem
iAddEdges n = iModNode n . iUnion
iRemEdges n = iModNode n . flip iDifference

iReplaceEdges n = iModNode n . const

--- Transforming graphs -----------------------------------------------------
iTransitiveClosure (G imap) = G tmap
   where
     tmap = iApply tnbrs imap
     tnbrs ns = iUnion ns (foldr (iUnion.iLookupSet tmap)
                                 iEmpty
				 (intSetToList ns))
     -- This should work for acyclic graphs (simple inductive proof).
     -- Cycles -> nontermination or black holes.

iReverse = edgeListToIntGraph . map swap . intGraphToEdgeList
-- wasteful if only one node is the reverse graph is inspected...

--- Inspecting graphs -------------------------------------------------------
iNeighbours = iLookupSet.unG

iReachable = iNeighbours . iTransitiveClosure
iRevNeighbours = iNeighbours . iReverse
iRevReachable = iNeighbours . iTransitiveClosure . iReverse

iTopologicalSort = tsort . intMapToList . unG
  where
    tsort nes =
      case partition (iIsEmpty.snd) nes of
        ([],[]) -> []
        ([],_) -> error "IntGraph.iTopologicalSort failed because of cycles in the graph"
        (ns,rest) -> map fst ns:tsort [(n,iDifference es skip) | (n,es)<-rest]
	  where skip = foldr (iUnion.snd) iEmpty ns

--- Conversions to/from graphs -----------------------------------------------
edgeListToIntGraph = foldr (uncurry iAddEdge) iEmptyGraph
intGraphToEdgeList g =
  [ (n,n') | (n,ns)<-intMapToList (unG g), n'<-intSetToList ns ]

-- auxiliary function
iModNode :: Node -> (NodeSet->NodeSet) -> IntGraph -> IntGraph
iModNode n f (G g) = G (iAddItem n (f (iLookupSet g n)) g)

iLookupSet m i =
  case iLookup m i of
    Nothing -> iEmpty
    Just s -> s
