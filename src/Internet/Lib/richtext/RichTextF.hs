module RichTextF(richTextFormF,richTextF) where
import AllFudgets
import RichTextMetrics
import RichTextLayout
import RichText
import RichTextFormatter
--import NonStdTrace(trace)

richTextF optsize =
  stripEither>^=<richTextFormF optsize nullF>=^<(\rt->Left (rt,[]))

richTextFormF optsize formF = 
  let size = case optsize of
	       Just s  -> s
	       Nothing -> Point 480 260
  in oldVscrollF False (size,size) (richTextRectF size formF)
--  in vscrollF (size,size) (showCommandF "rtF" (richTextRectF size))

richTextRectF s@(Point w h) formF =
  idRightF (idLeftF (idempotSP>^^=<pasteURLF)) >==<
  (cacheF $
   groupF startcmds 
          (changeBg paperColor $ richTextK [] w h [] ([],[]))
	  formF)
  where eventmask = [ExposureMask,ButtonPressMask,PointerMotionMask]
        startcmds =
         [ll w h,
          XCmd $ ChangeWindowAttributes [CWEventMask eventmask,
				  CWBitGravity NorthWestGravity],
	  XCmd $ ConfigureWindow [CWBorderWidth 0],
	  XCmd $ MapRaised]

pasteURLF =
    stripEither >^=< idRightF (mapFilterSP post>^^=<selectionF>=^<const PasteSel)
  where
    post (SelNotify x) = Just (inputMsg x)
    post _ = Nothing

ll w h = layoutRequestCmd (plainLayout (Point w h) False False)
goHome = LCmd (LayoutMakeVisible (rR 0 0 0 0) (Nothing,Nothing))

toOut = High. Right. Right
toSel = High. Right. Left
toForm = High. Left
putOut x = putsK [toOut x]
pasteSel = putsK [toSel ()]
putForm x = putsK [toForm x]

richTextK rs w h mrt lcmds =
  getK $ \msg ->
    case msg of
      Low (XEvt (Expose r 0)) -> redrawRichTextK (addrect r rs) w h mrt lcmds
      Low (XEvt (Expose r _)) -> richTextK (addrect r rs) w h mrt lcmds
      Low (XEvt (ButtonEvent _ _ _ _ Pressed (Button 2))) -> pasteSel same
      Low (XEvt (ButtonEvent _ p _ _ Pressed (Button 1))) ->
        case anchor lcmds p of
          Just a  -> putOut (inputMsg a) same
	  Nothing -> same
      Low (XEvt (MotionNotify _ p _ _)) ->
        case anchor lcmds p of
          Just a  -> putOut (InputChange a) same
	  Nothing -> same
      Low (LEvt (LayoutSize (Point w' h'))) ->
		if w'==w
		then richTextK rs w h' mrt lcmds
		else drawRichTextK w' h' mrt
      High (rt,sizes) -> newRichTextK w h (formatRichText rt sizes)
      _ -> same
  where same = richTextK rs w h mrt lcmds

newRichTextK w h rt =
  measureRtK rt $ \mrt ->
  drawRichTextK w h mrt

drawRichTextK w h mrt =
  let (h',cmds) = rtLayout w mrt
      places = [r|(_,acmds)<-cmds,(High r,_)<-acmds]
      --lcmds = map (apSnd (filter (isLow.fst))) cmds
      lcmds = [(y,[c|(Low c,a)<-acmds])|(y,acmds)<-cmds]
      anchors = [a | (_,acmds)<-cmds, (_,Just a) <-acmds]
  in putForm places $
     putsK (map Low [XCmd $ ClearArea (rR 0 0 (-1) (-1)) True,goHome,ll w h']) $
     richTextK [] w h' mrt (lcmds,anchors)

redrawRichTextK rs w h rt la@(lcmds,_) = 
    putK (Low $ wDrawMany [cmd|(y,cmds)<-lcmds,y `overlaps` rs, cmd<-cmds]) $
    richTextK [] w h rt la
  where
    --y `overlaps` rs = trace (show (y,rs)) $ any (overlap y) rs
    y `overlaps` rs = any (overlap y) rs
    overlap (y1,y2) (ya,yb) = y2>=ya && y1<yb

--cacheF = colorcacheF . fontcacheF . fstructcacheF . gCcacheF
cacheF = id

addrect (Rect (Point x y) (Point w h)) rs = (y,y+h):rs


anchor (_,anchors) p =
  case [(r,a,attrs) | (r,a,attrs)<-anchors, p `inRect` r] of
    [] -> Nothing
    as -> Just (mapPos p (last as))

mapPos p (r,a,attrs) =
  if "ISMAP" `elem` map fst attrs
  then let Point x y = p `psub` rectpos r
                       `psub` (Point 3 3) -- compensates for sepF 3 !!!
       in a++"?"++show x++","++show y
  else a

-- x `inside` (Low (Draw _ _ (DrawString (Point x1 _) _))) = x>=x1
--_ `inside` _ = False
  -- a temporary hack !!!
