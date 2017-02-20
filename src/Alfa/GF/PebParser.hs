module PebParser (pebParser) where

import Operations
import CF

-- modified from Peter Ljunglöf's chart parser. AR 5/6/2001

--- pebParser :: CF -> CFCat -> CFParser 
pebParser cf cat ss = [(parse2tree p, []) | p <- parseCFG gr cat ss] where
  gr = cf2grammar cf

--- cf2grammar :: CF -> Grammar
cf2grammar (rules,p) = (mkLit p, map mkOne rules) where
  mkOne (fun,(cat,its)) = ((fun,cat),its)
  mkLit p s = map mkOne (predefRules p s)

sameCat (_,c) (_,c') = c == c'
funOf = fst
catOf = snd

--- type ThisPredef = CFTok -> [Rule]

---------

--- type Rule    = ((CFFun, CFCat), [CFItem])
--- type Grammar = (ThisPredef,[Rule])

data ParseTree a c f = Leaf a | Node (CFFun f,CFCat c) [ParseTree a c f] 

--- parse2tree :: ParseTree -> CFTree
parse2tree (Node (fun,cat) trees) = CFTree (fun,(cat,trees')) where
  trees' = [parse2tree t | t@(Node _ _) <- trees]   -- ignore leafs

--------------------------------------------------
-- top down chart parser

--- type Edge = ((Int,Int),Rule)

--- parseCFG :: Grammar -> CFCat -> [CFTok] -> [ParseTree]
parseCFG gr@(_,rules) start input = [ tree | 
				   (((0,n),(start',[])),trees) <- edge_trees, 
				   n == length input, start == catOf start', 
				   tree <- trees ]
  where 
    edge_trees = build rules (process gr start input)


--- build :: [Rule] -> [Edge] -> [(Edge,[ParseTree])]
build rules edges = edge_trees
  where
    edge_trees = [ (edge,treesFor edge) | edge <- edges ]

    treesFor (arc,(cat,[])) = [ Node cat' trees | 
                                 (cat',rhs) <- rules, 
                                 sameCat cat cat',
                                 trees <- children rhs arc ] 

    children []              (i,k) = [ [] | i == k ]
    children (CFTerm s :cats) (i,k) = [ rest |  --- ignoring Leaf s
                                        rest <- children cats (i+1,k) ]
    children (CFNonterm cat:cats) (i,k) = [ tree:rest | 
                                       i <= k,
                                       (((i',j),(cat',[])),trees) <- edge_trees, 
                                       i == i', cat == catOf cat',
                                       rest <- children cats (j,k),
                                       tree <- trees ]


--- process :: Grammar -> CFCat -> [CFTok] -> [Edge]
process (plit,rules) start input = processEdges [] axioms
  where
    processEdges = foldr processEdge 
      where 
        processEdge edge chart
            | edge `elem` chart = chart
            | otherwise         = infer (edge:chart) edge
 
    infer chart edge = predict (combine chart edge) edge
 
    combine chart = processEdges chart . combineEdge 
      where
        combineEdge ((j,k),(b,[]))  
            = [ ((i,k),(a,bs)) | 
                ((i,j'),(a,CFNonterm b':bs)) <- chart, 
                catOf b == b', j == j' ]
        combineEdge ((i,j),(a,CFNonterm b:bs)) 
            = [ ((i,k),(a,bs)) | 
                ((j',k),(b',[])) <- chart, 
                catOf b' == b, j == j' ]
        combineEdge ((i,j),(a,CFTerm s:bs)) 
            = [ ((i,j+1),(a,bs)) | 
                j < length input,
                satRegExp s (input !! j) ]

    predict chart = processEdges chart . predictEdge 
      where 
        predictEdge ((i,j),(a,CFNonterm b:bs)) 
            = [ ((j,j),rule) | 
                rule@(b',_) <- rules, 
                b == catOf b' ]
        predictEdge _ 
            = []

    axioms = [ ((0,0),rule) | rule@(start',_) <- rules, start == catOf start' ]
--             ++    --- how else to get in literals? AR
--	     [ ((i,i+1),(fc,[])) | (i,a) <- zip [0..] input, 
--                   	                           rule@(fc,its) <- plit a]

