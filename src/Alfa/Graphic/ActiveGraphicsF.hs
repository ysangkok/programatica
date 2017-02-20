module ActiveGraphicsF(
	ActiveDrawing(..),activeLeaf,passiveLeaf,
	activeGraphicsF,activeGraphicsF',
	tagLeft,tagRight,
	replaceAllGfx,replaceGfx,GfxEvent(..),GfxCommand(..),
	drawingAnnotPart,drawingAnnotPart',maybeDrawingPart,mapLeafDrawing,
	drawingAnnots,
	Gfx(..),
	Pressed(..))
   where
import AllFudgets
import qualified IntMap as IM
import List(mapAccumL)
import ListUtil(mapSnd)

#ifdef __HASKELL98__
#define map fmap
#endif

--tr s x = ctrace s x x
tr s x = x

type ActiveDrawing lbl leaf i o = Drawing lbl (Either (F i o) leaf)
activeLeaf = atomicD . Left
passiveLeaf = atomicD . Right


tagLeft = tag (Left,stripLeft)
tagRight = tag (Right, stripRight)

tag (tagf,untagf) = mapLeafDrawing (mapEither f id)
  where f fud = tagf >^=< fud >=^^< mapFilterSP untagf


activeLeaves drawing = extractParts drawing sel
  where sel (AtomicD (Left f)) = Just f
	sel _ = Nothing

activeGraphicsF x = activeGraphicsF' standard x

activeGraphicsF' customiser adrawing0 = 
    loopThroughRightF (absF ctrlSP0) (graphicsGroupF' pm activeF)
  where
    pm = customiser . setInitDisp drawing0 . setGfxEventMask mask
    mask = [GfxDragMask,GfxButtonMask,GfxMotionMask]
    activeF = dynListF

    active0 = number 0 (activeLeaves adrawing0)
    drawing0 = mkSpace adrawing0
    ctrlSP0 =
      putsSP [create i f | (i,(_,f))<-active0] $
      ctrlSP (IM.fromList [(i,(dpath,Nothing)) | (i,(dpath,_))<-active0]) []

ctrlSP active pendingReqs =
    getSP $ either fromLoop fromOutside
  where
    same = ctrlSP active pendingReqs
    changeActive active' = ctrlSP active' pendingReqs

    fromLoop = either fromGfx fromActive
    fromActive = either aLayoutReq aUserOutput . pushTag . tr "fromDyn"
    fromOutside = either gfxCmd aUserInput

    aUserOutput (i,x) =
	case IM.lookup i active of
	  Just (dpath,_) -> output1 Right (i,dpath,x)
	  _ -> same -- can't happen

    aUserInput (i,x) = putSP (msg i (Right x)) same

    fromGfx gfxevt =
       case gfxevt of
	 GfxResized _ -> output Left gfxevt $
			 adjustPlaces active pendingReqs
	 _ -> output1 Left gfxevt

    aLayoutReq (i,(fpath,lmsg)) =
      case lmsg of
	LayoutRequest req ->
	  case IM.lookup i active of
	    Just (dpath,_) ->
		--putSP (toGfx $ replaceGfx dpath (spaceD req)) $
		adjustPlaces
		  (IM.add (i,(dpath,Just (fpath,nowhere))) active)
		  ((dpath,req):[(p,r)|(p,r)<-pendingReqs,p/=dpath])
	    _ -> error "ActiveGraphicsF.aLayoutReq" --same -- !! can't happen
	_ -> same -- !! other layout msgs are ignored

    adjustPlaces active pendingReqs =
        ctrace "ap" (length unstarted) $
	if null unstarted
	then putSP (toGfx (replaceManyGfx (mapSnd spaceD pendingReqs))) $
	     getGfxPlaces (map fst paths) $ \ places ->
	     let (active',msgss) = mapAccumL adj active (zip paths places)
	     in putsSP (concat msgss) $
	        ctrlSP active' []
	else ctrlSP active pendingReqs
      where
        unstarted = [()|(_,(_,Nothing))<-activeL]
	paths = [(dpath,(i,x)) | (i,(dpath,Just x))<-activeL]
	activeL = IM.toList active

        adj active ((dpath,(i,(fpath,place))),place') =
	  if place'/=place
	  then (IM.add (i,(dpath,Just (fpath,place'))) active,
		[msg i (Left (fpath,place'))])
	  else (active,[])

    gfxCmd cmd =
	putSP (toGfx (map mkSpace cmd)) $
	case cmd of
	  GetGfxPlaces paths ->
	     -- Do this synchonously so that the response isn't accidentally
	     -- mixed up with the stuff in adjustPlaces.
	     getGfxPlaces paths $ \ places ->
	     putSP (toOutside (Left (GfxPlaces places))) $
	     same
          ChangeGfx changes ->
	      doActiveChanges changes activeL [] pendingReqs n $ \ active' pendingReqs' ->
	      ctrlSP (IM.fromList active') pendingReqs'
	    where
	      activeL = IM.toList active
	      n = if null activeL
	          then 1
		  else 1+maximum (map fst activeL) -- unused tags for dynListF
	  _ -> same -- other commands need no special treatment

    -- changes are assumed to be non-overlapping
    doActiveChanges changes oldActive newActive pends n cont =
      case changes of
	[] -> putsSP [create i fud | (i,(_,fud))<-newActive] $
	      cont ([(i,(dpath,Nothing))|(i,(dpath,_))<-newActive]++oldActive) pends
	(path,(cur,Just gfx)):changes ->
	  case part (isBelow path.fst.snd) oldActive of
	    (scrapped,oldActive') ->
		 --(if pscrappedcnt/=0 then ctrace "pend" pscrappedcnt else id) $
	         putsSP [destroy i | (i,_)<-scrapped] $
	         doActiveChanges changes oldActive' (active'++newActive) pends' n' cont
	       where
		 pscrappedcnt = length pscrapped
	         (pscrapped,pends') = part (isBelow path.fst) pends
	         active' = [(i,(path++path',fud))|(i,(path',fud))<-new]
	         new = number n (activeLeaves gfx)
		 n' = if null new then n else 1+fst(last new)

	_:changes -> doActiveChanges changes oldActive newActive pends n cont

    output1 tag x = output tag x same
    output tag x = putSP (toOutside (tag x))

toOutside = Right
toLoop = Left
toDyn = toLoop . Right
toGfx = toLoop . Left

mkSpace = mapLeafDrawing (mapEither (const (emptyMG 20)) id)
spaceD = activeLeaf . emptyMG'

create i fud = --ctrace "toDyn" ("DynCreate",i) $
	       toDyn (i,DynCreate (userLayoutF (wrapF fud)))
destroy i = --ctrace "toDyn" ("DynDestroy",i) $
	    toDyn (i,DynDestroy)
msg i x = --ctrace "toDyn" (i,x) $
	  toDyn (i,DynMsg x)

getGfxPlaces paths =
    cmdContSP (toGfx $ GetGfxPlaces paths) wait
  where
    wait (Left (Left (GfxPlaces ps))) = Just ps
    wait _ = Nothing

pushTag (i,Left x) = Left (i,x)
pushTag (i,Right y) = Right (i,y)

nowhere = Rect 500 (-1)

isBelow this path = lhead this path==this

replaceManyGfx changes =
  ChangeGfx [(dpath,(False,Just d))| (dpath,d)<-changes]

--wrapF = autoLayoutF
wrapF fud = showCommandF "agado" (autoLayoutF' agnowait Dynamic (showCommandF "adagi" fud))
--wrapF fud = stripEither >^=< groupF [ConfigureWindow [CWBorderWidth 0]] nullK fud >=^< Right

agnowait = argFlag "agnowait" True
