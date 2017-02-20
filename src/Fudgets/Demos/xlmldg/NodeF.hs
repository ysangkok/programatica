module NodeF(nodeF) where
import Fudgets
import Graph
import MovableObjectF

nodeF (Node rect (txt,names)) =
  case names of
    [n] -> let pos = rectpos rect
	   in contF pos (const n>^=<buttonF txt)>=^<const Click
    _  -> --contF rect (simpleMenuF (orig rect) menuFont txt names id)
          contF (rectpos rect) (menuF txt (map (pairwith id) names))

orig (Rect _ size) = Just (Rect (pP (-1) (-1)) size)

contF pos fud = movableObjectF pos fud
