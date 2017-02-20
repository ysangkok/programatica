module move where
import Output(OUTPUT(..))
--export up,down,left,right,moveto,addv,negv,turnleft,turnright,

up = (-1,0)
down = (1,0)
left = (0,-1)
right = (0, 1)
addv (y,x) (dy,dx) = (y+dy,x+dx)
negv (y,x) = (-y,-x)
turnleft (y,x) = (-x,y)
turnright= negv . turnleft

moveto (y,x) = [Moveto x y]
