module CanonF(canonF) where
import AllFudgets hiding (size)
import ObjectF

canonF space =
    timerObjectF pos0 size "black" "cyan" [] [] canonK0
  where
    pos0 = startpos space
    rightlimit = space `psub` Point (width+margin) 0

    canonK0 pos gc =
        let canonK pos dx =
                getK $ \msg ->
                  case msg of
	            Low (XEvt (Expose _ 0)) -> putsK (drawCannon gc) cont
		    High (Left talarm) -> move dx
		    High (Right (ButtonEvent _ _ _ _ pressed (Button n))) ->
		      case pressed of
		        Pressed ->
			  case n of
			    1 -> startmove (-dx0)
			    2 -> fire
			    3 -> startmove dx0
			    _   -> cont
			Released -> putK removeTimer stopmove
		    _ -> cont
	       where cont = canonK pos dx
	             fire = putK (High $ Right (pos `padd` (Point (width `div` 2) 0))) cont
	             move dx = let pos' = pos `padd` (Point dx 0)
		                              `pmax` origin `pmin` rightlimit
		               in putK (Low $ XCmd (moveWindow pos'))
			               (canonK pos' dx)
		     startmove dx' =
		        if dx==0
			then putK (setTimer dt dt) $ move dx'
			else cont
		     stopmove = canonK pos 0
	in canonK pos 0

drawCannon gc = map Low
  [wFillPolygon gc Convex CoordModeOrigin canonBorder]

canonBorder =
  let h=height-1; w=width-1; t=towerheight; b=(width-towerwidth) `quot` 2
      m=width `quot` 2
  in [Point 0 h,Point 0 (t+2),Point 2 t,Point b t,Point m 0,
      Point (width-b) t,Point (w-2) t,Point w (t+2),Point w h]

size = Point width height
width = 30
height = 20
towerheight = 8
towerwidth = 10

margin = 10
dt = argReadKey "dt" 30 :: Int
dx0 = argReadKey "dx" 5 :: Int

startpos space = Point margin (ycoord space-height-margin)
