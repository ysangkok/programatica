module WorldF(worldF) where
-- The entire world, implemented as a single fudget, instead of one fudget
-- per object in the world.
import AllFudgets
import ReadPic
--import ListUtil(concatMap,rept)

data Base     = B (PixmapId, GCId, Size) Point Int
data Torpedo  = T GCId (Maybe Point)
type Invaders = (Int,[Invader])
data Invader  = I ((PixmapId,Size),(PixmapId,Size)) GCId Point

worldF space = windowF startcmds objectsK0>==<tickF
  where
    startcmds = [layoutRequestCmd (plainLayout space True True),
                 XCmd $ ChangeWindowAttributes [CWEventMask eventmask]]
    eventmask = [--KeyPressMask,KeyReleaseMask,
                 ButtonPressMask,ButtonReleaseMask,
		 ExposureMask]

    tickF = startupF [False] (timerF>=^<pre)
      where
        pre True = Nothing
	pre False = Just (dt,dt)

    objectsK0 =
      changeBg "black" $
      initBaseK $ \ base ->
      initTorpedoK $ \ torpedo ->
      initInvadersK $ \ invaders ->
      objectsK 16 (base, torpedo, invaders)

    initBaseK k =
	objGC "cyan" $ \ gc ->
        readPic "pbm/base2.xbm" $ \ (pm,size@(Point _ height)) ->
	let startpos = Point margin (ycoord space-height-margin)
	in k (B (pm,gc,size) startpos 0)

    initTorpedoK k =
        objGC "white" $ \ gc -> k (T gc Nothing)

    initInvadersK k =
        conts readPics imageFiles $ \ pms ->
	conts objGC [blue,green,red] $ \ gcs ->
	let [i1,i2,i3] = zip pms gcs
	    row = replicate hcount
	    places = [Point (margin+hsep*x) (4*margin+vsep*y)|
	                y<-[5,4..1],x<-[1..hcount]]
	in k (dx0,zipWith centerI (concatMap row [i3,i3,i2,i2,i1]) places)
      where
        centerI (pm@((_,Point w _),_),gc) p = I pm gc (p `padd` pP ((invWidth-w) `quot` 2) 0)
        size=Point invWidth invHeight
	hsep = invWidth+4
	vsep = invHeight+8
	dx0 = 2

        imageFiles = [(name n 'a',name n 'b') | n<-"123"]
	  where	name n a = "pbm/vader"++[n,a]++"2.xbm"

    objectsK t state =
        getK $ \ msg ->
	case msg of
          High Tick -> 
	    case t-1 of
	      0 -> moveInvadersK state $ tickK ((invcnt state+6) `quot` 4)
	      t' -> tickK t' state
	  Low (XEvt (Expose r 0)) -> redrawK state $ same
	  Low (XEvt (ButtonEvent _ _ _ _ Pressed (Button 2))) ->
	    cont (fire state)
	  Low (XEvt (ButtonEvent _ _ _ _ pressed (Button n))) ->
	    cont (move state pressed n)
	  _ -> same
      where same = objectsK t state
            invcnt (_,_,(_,is)) = length is
            cont = objectsK t

    redrawK (base,torpedo,invaders) =
      putsK (drawBase base++drawTorpedo torpedo++drawInvaders invaders)

    move (B d p v,t,i) pressed button = (B d p v',t,i)
      where
        v'   = v+dv
	dv   = case pressed of Pressed -> bdir ; Released -> -bdir
	bdir = case button of 1 -> -dx ; 3 -> dx ; _ -> 0
      

    tickK t (base,torpedo,invaders) =
      tickBaseK base $ \ base' ->
      tickTorpedoK torpedo invaders $ \ torpedo' invaders' ->
      objectsK t (base',torpedo',invaders')
      
    tickBaseK b@(B d@(_,_,size) p v) k =
      case v of
        0 -> k b
	_ -> putsK (clear p size:drawBase b') $ k b'
	  where b' = B d p' v
	        p' = p `padd` pP v 0

    --drawBaseK = putsK . drawBase

    drawBase (B (pm,gc,size) p _) = [copy gc pm size p]

    tickTorpedoK t@(T gc optp) inv k = 
      case optp of
        Nothing -> k t inv
	Just p@(Point x y) ->
	  if y<torpedoDy
	  then putK (clear p torpedoSize) $ k (T gc Nothing) inv
	  else case p `hit` inv of
	         Just (I ((_,s),_) _ ip,inv') ->
		   putsK [clear p torpedoSize, clear ip s,High 10] $
		   k (T gc Nothing) inv'
		 _ ->
		   let t' = T gc (Just (Point x (y-torpedoDy)))
		   in putsK (clear p torpedoSize:drawTorpedo t') $ k t' inv

    p `hit` (dx,is) =
        case part isHit is of
          ([],_) -> Nothing
	  (i:_,is) -> Just (i,(dx,is))
      where
        isHit (I ((_,s),_) _ ip) = p `inRect` r
	  where r = Rect (ip `padd` Point 2 0) (s `psub` Point 4 0)

    drawTorpedo (T _ Nothing) = []
    drawTorpedo (T gc (Just p)) = [Low (wFillRectangle gc (Rect p torpedoSize))]


    fire s@(b,T gc t,i) =
      case t of
        Nothing -> (b,T gc (Just (firePos b)),i)
	_ -> s 

    moveInvadersK (b,t,(dx,is)) k = putsK (concat movecmds) $ k (b,t,(dx',is'))
      where
        (dx',d) = if dx>0 && off_right || dx<0 && off_left
	          then (-dx,pP 0 invDy)
		  else (dx,pP dx 0)
	xs = map (\(I _ _ (Point x _))->x) is
	off_left = minimum xs < margin
	off_right = maximum xs > xcoord space-margin-invWidth
	(movecmds,is') = unzip (map moveInvader is)
	moveInvader (I (p1@(_,size),p2) gc p) =
	    case ycoord d of
	      0 -> (drawInvader i',i')
	      _ -> (clear p size:drawInvader i',i')
	  where
	    i' = I (p2,p1) gc (p `padd` d)

    drawInvaders = concatMap drawInvader.snd
    drawInvader (I ((pm,size),_) gc p) = [copy gc pm size p]

objGC fg cont =
  --convGCattrsK [GCBackground "black",GCForeground fg] $ \ attrs ->
  allocNamedColorPixel defaultColormap "black" $ \ bgpixel ->
  allocNamedColorDefPixel defaultColormap fg "white" $ \ fgpixel ->
  let attrs = [GCBackground bgpixel,GCForeground fgpixel] in
  wCreateGC rootGC (GCGraphicsExposures False:attrs) $ \ gc ->
  cont gc

clear p size = Low (XCmd $ ClearArea (Rect p size) False)
copy gc pm size p = Low (wCopyPlane gc (Pixmap pm) (Rect origin size) p 0)

firePos (B (_,_,size) p _) = 
  p `padd` pP (xcoord size `quot` 2) (-torpedoHeight)

torpedoSize = pP 1 torpedoHeight
torpedoHeight = 16
torpedoDy = 12

invHeight = 16
invWidth = 24
invDy = 3*invHeight `quot` 4

margin = 10
dx = read (argKey "dx" "3")::Int
dt = read (argKey "dt" "20")::Int

hcount = read (argKey "hcount" "11")::Int
red = argKey "red" "red"
green = argKey "green" "green"
blue = argKey "blue" "blue"
