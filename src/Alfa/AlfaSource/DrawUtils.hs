module DrawUtils where
import Fud
import UAnnots(LayoutDirection(..))
import Debug2(trace)

emparen = emparen' g
emparen' g d = hboxrD' 1 [lparD,d,rparD]
      where
	lparD = g lpar
	rparD = g rpar

embrace = embrace' g
embrace' g d = hboxrD' 2 [lbraceD,d,rbraceD]
      where
	lbraceD = g lbrace
	rbraceD = g rbrace

emabrack = emabrack' g
emabrack' g d = hboxrD' 2 [labrackD,d,rabrackD]
      where
	labrackD = g lAngleBracket
	rabrackD = g rAngleBracket

embrack = embrack' g
embrack' g d = hboxrD' 2 [lbrackD' g,d,rbrackD' g]
lbrackD = lbrackD' g
rbrackD = lbrackD' g
lbrackD' g = g lbrack
rbrackD' g = g rbrack

hLineD = g (hFiller 1)

hrLineD = g hrLineFD
hrLineFD = flex' (Point 5 8) f
  where f (Rect p (Point w _)) = [line p1 p2]
	  where p1=p+dy
		p2=p1+dx
		dy=Point 0 7
		dx=Point w 0

upArrowD = g upArrowFD
upArrowFD = flex' 8 f
  where f (Rect p (Point _ h)) = upArrow p h

upArrowTailD = g upArrowTailFD
upArrowTailFD = flex' 8 f
  where f (Rect p (Point _ h)) = upArrowTail p (h `div` 2)

upArrow = upArrow' False
upArrowTail = upArrow' True
upArrow' tail p0 h =
    line p1 p2: line (pa-dx) p1: line (pa+dx) p1:
    (if tail then [line (p2-dx) (p2+dx)] else [])
  where
    p1 = p0+dx
    p2 = p1+Point 0 h
    pa = p1+dy
    dx = Point awidth 0
    dy = Point 0 awidth

awidth = 2
line p1 p2 = DrawLine (Line p1 p2)

lowerRightD = g lowerRightFD
lowerRightFD = flex' 10 f
  where f (Rect p s@(Point w _)) =
	    --[line p1 p2,line p2 p3]
	    [lrTurn (p1-dy) as]
	  where p1 = p+dx
	        p2 = p1+dy
		p3 = p+dy+Point w 0
	        as = Point (w-mw) (2*mh)
	        dx = Point mw 0
	        dy = Point 0 mh
	        mw = awidth
		mh = 7

forkRightD = g forkRightFD
forkRightFD = flex' 10 f
  where f (Rect p s@(Point w h)) =
	    [line p1 (p+Point mw h),
	     --line p2 p3,
	     lrTurn (p1-dy) as]
	  where p1=p+Point mw 0
		p2=p1+dy
		p3 = p+dy+Point w 0
		dy=Point 0 mh
	        as = Point (w-mw) (2*mh)
		mw = awidth
		mh = 7
		m = Point mw mh

lrTurn p s = DrawArc (Rect p s') (180*64) (105*64) -- fudge: 15 degrees extra
  where s' = s+Point 5 0 -- fudge: 5 pixels extra

--revhboxcaD = placedD (revP horizontalCenterP) . boxD
revhboxcaD = placedD (revP horizontalAlignP) . boxD

hboxcaD = hboxcaD' defaultSep
hboxcaD' sep = placedD (horizontalAlignP' sep) . boxD

hboxcarD = hboxcarD' defaultSep
hboxcarD' sep = placedD (refEdgesS `spacerP` horizontalAlignP' sep) . boxD

hboxrD = hboxrD' defaultSep
hboxrD' sep = placedD (refEdgesS `spacerP` horizontalP' sep) . boxD

refEdgesD = spacedD refEdgesS
refMiddleD = spacedD refMiddleS
noRefsD = spacedD noRefsS

vboxlrD = placedD (refMiddleS `spacerP` verticalLeftP) . boxD

--vboxcD' sep = placedD (spacersP (verticalP' sep) (repeat hCenterS)) . boxD

flex'' = FlexD (Point 30 5) False True

interleave x [] = [x]
interleave x (y:ys) = x:y:interleave x ys

hCenterD = spacedD hCenterS
vCenterD = spacedD vCenterS

hPadD d = spacedD (hMarginS d d)

indentD = spacedD . indentS
indentS d = hMarginS d 0

hvIndentD h v = spacedD (hvIndentS h v)
hvIndentS h v = hvMarginS (Point h v) 0
vis1D [] = trace "vis1D []" $ boxVisibleD 1 []
vis1D ds = boxVisibleD 1 ds
hiddenD d = boxVisibleD 0 [d]
fpartS = noRefsS `compS` hMarginS 5 5 `compS` hCenterS -- fraction part spacer


wideOrTallD' = wideOrTallD'' 12
wideOrTallD'' indent maxwidth dir =
    placedD wtP . boxD 
  where
    wtP = case dir of
	    Just Wide -> wideP
	    Just Tall -> tallP
	    Nothing -> autoP

    wideP = horizontalAlignP
    tallP = if indent>0
	    then spacersP verticalLeftP (idS:repeat (indentS indent))
	    else verticalLeftP

    autoP = ifSizeP p wideP tallP
      where p (Point w1 _) (Point w2 _) =
              -- "'" (haskell mode font-lock bug workaround)
	      --trace (show ("wideOrTallD'",w1,w2,maxwidth)) $
	      w1<=maxwidth || w1-w2<150 && w2>9*w1 `div` 10
