module InvadersF(invadersF) where
import AllFudgets hiding (size)
import ObjectF

invadersF space = score>^=<invaderArmyF space >==<timer

score _ = 10 -- To be refined.

data InvMsg = Tick' | Turn | Hit Point | Death (Int,Int)

timer = stripEither >^=< idRightF (const Tick' >^=< startupF [Just (500,500)] timerF)
        >=^< (Right . Hit)

{-
putK (Low (SetTimer 500 500 0)) (concmapSP tick)
  where tick (Low (TimerAlarm _)) = [High Tick']
        tick (High p) = [High (Hit p)]
        tick _ = []
-}

invaderArmyF space =
  let invaders = [(pos,invaderF space (place pos) (invdata pos))|pos<-invlist0]
      route (_,Turn) = [Left Turn]
      route (p,Death _) = [Left (Death p),Right (Death p)]
      invsF = {-gCcacheF . fontcacheF . bitmapfilecacheF .-} listF $ invaders
  in loopLeftF (concmapSP route>^^=<invsF>=^^<shoutSP invlist0 False)

shoutSP invlist turning =
  let toall msg t = putsSP [(n,msg)|n<-invlist] (shoutSP invlist t)
  in getSP $ \msg->
       case stripEither msg of
         Turn    -> if turning then shoutSP invlist turning
                    else toall Turn True
         Tick' -> toall Tick' False
	 Death p -> shoutSP (remove p invlist) turning
	 msg     -> toall msg turning

invlist0 = [(x,y)|y<-[5,4..1],x<-[1..hcount]]
place (x,y) = Point (margin+hsep*x) (4*margin+5*height*y `quot` 4)

invaderF space@(Point sw sh) pos0 (fgcolor,pics) = 
    objectF pos0 size "black" fgcolor [] gcattrs invaderK0
  where
  gcattrs = [GCLineWidth 2]

  invaderK0 (Point x0 y0) gc =
      readPics pics $ \pm1 size1 pm2 size2 ->
      let drawInvader1 = Low (wCopyPlane gc (Pixmap pm1) (Rect origin size1) (center'' size size1) 0)
          drawInvader2 = Low (wCopyPlane gc (Pixmap pm2) (Rect origin size1) (center'' size size2) 0)
	  -- do something about the above !!
          invaderK turning x y dx =
              getK $ \msg ->
	        case msg of
		  High Tick' -> move
		  High Turn -> turn
		  High (Hit p) -> if p `inRect` (Rect (Point x y) size)
		                  then deadInvaderF
				  else cont
		  Low (XEvt (Expose _ 0)) -> putK drawInvader1 cont -- !!! which?
		  _ -> cont
	    where
	      cont = invaderK turning x y dx
	      turn = invaderK True    x y dx
	      outside x = x+width>sw-margin || x<margin
	      move =
	        let (x',y',movecmd,dx') =
		      if turning
		      then (x,y+dy,Low $ XCmd (ConfigureWindow [CWY y']),-dx)
		      else (x+dx,y,Low $ XCmd (ConfigureWindow [CWX x']),dx)
		    turnmsg= if outside (x'+dx')
		             then [High Turn]
			     else []
		    drawcmd = case 0 of
		                0 -> drawInvader1
				1 -> drawInvader2
		in putsK (movecmd:{-drawcmd:-}turnmsg)
		        (invaderK False x' y' dx')
      in invaderK False x0 y0 dx0

readPics (file1,file2) cont =
  readPic file1 $ \ pm1 size1 ->
  readPic file2 $ \ pm2 size2 -> cont pm1 size1 pm2 size2

readPic file cont =
  readBitmapFile file $ \ bmr ->
  case bmr of
    BitmapReturn size _ pm -> cont pm size
      -- should use actual size of the pixmaps !!!
    BitmapBad -> error ("Can't read bitmap "++file)

deadInvaderF = putsK [Low $ XCmd $ UnmapWindow,High (Death (0,0))] nullK --(0,0) not used

--invDrawing = [DrawImageString (Point 1 13) "Inv!"]
{-
invDrawing = [DrawCircle (Point 4 0) 15,
              DrawLine (lL 1 0 6 2),DrawLine (lL 1 15 6 13),
	      DrawLine (lL 22 0 17 2),DrawLine (lL 22 15 17 13)]
-}

invdata (_,y) = [inv 1,inv2,inv2,inv3,inv3] !! (y-1)
  where inv2  = inv 2
        inv3  = inv 3
	inv n = (col n,(name n "a",name n "b"))
	col 1 = "blue"
	col 2 = "green"
	col 3 = "red"
	name n a = "pbm/vader"++show n++a++"2.xbm"

center'' big small = scalePoint 0.5 (big `psub` small)

size=Point width height
width = 24
height = 16
hsep = width+4
vsep = height+8

dx0 = 2
dy = 3*height `quot` 4

margin = 10

--dbK (Point x y) (Point w h) = echoK ("Not in "++show((x,y),(w,h)))

hcount :: Int
hcount = argReadKey "hcount" 11

