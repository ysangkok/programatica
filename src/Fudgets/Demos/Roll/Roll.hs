import Fudgets
import ExtraFudgets

main = fudlogue $ shellF "Roll" rollF

rollF = revVBoxF (scoreF >==< boardF)

data Scores = Win | Loose deriving (Eq)

scoreF =
    spacerF centerS $
    hBoxF $
    listF [(Win,scoreDispF "You win"),
           (Loose,scoreDispF "You loose")]
  where
    scoreDispF lbl = lbl `labLeftOfF` intDispF >==< countF
       where
	countF = mapstateF inc 0
        inc n _ = (n+1,[n+1])

boardF = eventF eventmask (graphicsBgF "yellow" boardD ballF) >==< tickF
  where
    eventmask = [PointerMotionMask]
    tickF = startupF [Just (dt,1000)] timerF
 
dt = argReadKey "dt" orig_dt
orig_dt = 25 -- time step in the original program

boardD = stackD [dangersD,bouncesD,startAndFinishD,linesD]
  where
    fd = atomicD . FixD size
    dangersD = fgD "black" $ fd $ map FillRectangle dangers
    bouncesD = fgD "blue"  $ fd $ map FillRectangle bounces
    startAndFinishD = fd [DrawString start "Start",DrawString finish "Finish"]
    linesD = fd $ map DrawLine lines'


-- Geometrical info:
magicx = 10
magicy = 10
width = 600+magicx
height = 600+magicy
size = Point width height -- board size
dist = 300
ballsize = 40
halfbsize = ballsize `div` 2
border = 2

dangers = [rR 0 0 100 100, rR 500 250 100 100, rR 300 500 100 100]
bounces = [rR 250 100 300 50, rR 100 100 50 200, rR 250 300 100 100,
	   rR 0 450 200 50]
lines' = [lL dist border (width-dist) border,
          lL dist (height-border) (width-dist) (height-border)]
start = Point 550 25
finish = Point 10 580


ballF = shapedObjectF "red" [fillCircle origin ballsize] startrect ballSP
  where
    ballSP = run state0

    run s = getSP $ either handleEvent handleTick
      where
        same = run s
	handleTick _ = move

	handleEvent event =
	  case event of
	    MotionNotify { pos=pos } -> tilt pos
	    _ -> same

        tilt (Point tx ty) = run (s { tiltx = tx-middlex, tilty = ty-middley })

        move_ball s' =
	  if (x s',y s') /=(x s,y s)
	  then putSP (Left (Point (x s') (y s'))) $
	       run s'
	  else run s'

        move =
	    if any (fall_into (x s1+halfbsize,y s1+halfbsize)) dangers
	    then you Loose
	    else if x s1 < xwin && y s1 > ywin
	         then you Win
	         else move_ball s'
 	  where
             you outcome =
	       putSP (Right (outcome,Tick)) $
	       move_ball state0

	     s1 = s { dx = newdelta (tiltx s) (dx s),
		      dy = newdelta (tilty s) (dy s),
		      x  = x s + round (dx s1 * (dt / orig_dt)),
		      y  = y s + round (dy s1 * (dt / orig_dt)) }
	     s2 = if x s1 > x s
	          then if x s1 > xmax
		       then s1 { x=xmax, dx = -dx s1 }
	               else foldr (bounce_left (x s) (y s)) s1 bounces
	          else if x s1 < x s
                       then if x s1 < 0
	                    then s1 { x=0, dx = -dx s1 }
			    else foldr (bounce_right (x s) (y s)) s1 bounces
		       else s1
	     s' = if y s2 > y s
	          then if y s2 > ymax
		       then s2 { y=ymax, dy = -dy s2 }
	               else foldr (bounce_above (x s) (y s)) s2 bounces
	          else if y s2 < y s
                       then if y s2 < 0
	                    then s2 { y=0, dy = -dy s2 }
			    else foldr (bounce_below (x s) (y s)) s2 bounces
		       else s2

data BallState = B { x,y,tiltx,tilty::Int, dx,dy::Double }
state0 = B xstart ystart 0 0 0.0 0.0

xmax = width - ballsize
ymax = height - ballsize
xstart = xmax - ballsize - 10  -- magicx?
ystart = ballsize + 10  -- magicy?
startpos = Point xstart ystart
startrect = Rect startpos (ballsize+2) -- +2?
middlex = width `div` 2
middley = height `div` 2
xwin = halfbsize
ywin = ymax - halfbsize

newdelta tilt delta =
    if ftilt > 0.0 && delta < 0.0 then
      delta + ftilt / 500.0
    else if ftilt < 0.0 && delta > 0.0 then
      delta + ftilt / 500.0
    else if delta > height then
      height
    else if delta < - height then
       - height
    else
      delta + ftilt / 2000.0
  where
    ftilt = fromIntegral tilt


-- fall_into (xb,yb) r = Point xb yb `inRect` r

fall_into (xb,yb) (Rect (Point x y) (Point w h)) =
    x < xb && xb < x1 && y < yb && yb < y1
  where
    x1 = x+w
    y1 = y+h

bounce_left xb yb (Rect (Point x y) (Point w h)) s@(B{x=xn, y=yn}) =
    if xb <= a && a < xn && b <= bn && bn <= d
    then s { x = a, y = bn, dx = -dx s }
    else s
  where
    bn = ((a - xb) * (yn - yb)) `div` (xn - xb) + yb -- using laziness !
    a = x - ballsize
    b = y - ballsize
    c = x + w
    d = y + h

bounce_right xb yb (Rect (Point x y) (Point w h)) s@(B{x=xn, y=yn}) =
    if xb >= c && c > xn && b <= bn && bn <= d
    then s { x = c, y = bn, dx = -dx s }
    else s
  where
    bn = ((c - xb) * (yn - yb)) `div` (xn - xb) + yb -- using laziness !
    a = x - ballsize
    b = y - ballsize
    c = x + w
    d = y + h

bounce_above xb yb (Rect (Point x y) (Point w h)) s@(B{x=xn, y=yn}) =
    if yb <= b && b < yn && a <= an && an <= c
    then s { x = an, y = b, dy = -dy s }
    else s
  where
    an = ((b - yb) * (xn - xb)) `div` (yn - yb) + xb -- using laziness !
    a = x - ballsize
    b = y - ballsize
    c = x + w
    d = y + h

bounce_below xb yb (Rect (Point x y) (Point w h)) s@(B{x=xn, y=yn}) =
    if yb >= d && d > yn && a <= an && an <= c
    then s { x = an, y = d, dy = -dy s }
    else s
  where
    an = ((d - yb) * (xn - xb)) `div` (yn - yb) + xb -- using laziness !
    a = x - ballsize
    b = y - ballsize
    c = x + w
    d = y + h
