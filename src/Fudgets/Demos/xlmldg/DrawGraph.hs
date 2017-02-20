module DrawGraph(drawGraph,topSidePoint,bottomSidePoint) where
import Fudgets
import Graph
--import utils
import ListUtil(chopList)
import Scans(mapAccumL)
import ListMap(lookupWithDefault)
import HO(apSnd)

drawGraph (nodenames,rows) textSize =
  let (p,size)= place nodenames rows textSize
      mknode (node,(txt,rect)) = (node,Node rect (txt,nodenames node)) -- hmm
  in (size, Graph (map mknode p) (edges p (concat rows)))

place nodenames rows textSize =
  let noderows = map (map fst) rows
      Point _ h = textSize "A"
  in let placerow (r,row) =
	  let y=yspace*r-(yspace+h) `quot` 2
	  in let placenode x node =
		  let txt = nodetext (nodenames node)
		      Point w h = textSize txt `padd` pP 15 10
		  in (x+w+xspace,(node,(txt,Rect (Point x y) (Point w h))))
	  in mapAccumL placenode xspace row
  in let nodes = map placerow (number 1 noderows)
  in let xmax = maximum (xspace:map fst nodes)
  in (concatMap (centerRow xmax) nodes,
      Point xmax (yspace*(length rows)))

centerRow xmax (w,row) = map ((apSnd . apSnd) (moveRight ((xmax-w) `quot` 2))) row
--moveRight dx (Rect (Point x y) size)= Rect (Point (x+dx) y) size
moveRight dx rect = moverect rect (Point dx 0)

edges p = concatMap (nodeEdges p)

nodeEdges p (n,ns) =
  let ra = lookuprect n p
  in let top = topSidePoint ra
  in let edge m = let rb = lookuprect m p
		  in ((n,m),Line top (bottomSidePoint rb))
  in map edge ns

topSidePoint (Rect (Point x y) (Point w _)) = Point (x+w `quot` 2) y
bottomSidePoint (Rect (Point x y) (Point w h)) = Point (x+w `quot` 2) (y+h)

yspace = argReadKey "yspace" 50 :: Int
xspace = argReadKey "xspace" 15 :: Int

lookuprect x rs = snd (lookupWithDefault rs (error "assoc") x)

nodetext [s] = basename s
nodetext (s:_) = basename s ++ "..." -- don't want too long strings

basename s = last (chopList (splitat '/') s)

splitat x = apSnd (drop 1).break (x==)
