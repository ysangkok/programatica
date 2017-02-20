module MovableObjectF where
import AllFudgets

movableObjectF p0 fud =
    loopThroughRightF (mapF route) objF
  where
    objF = startupF [Left (putS p0)] (dynSpacerF (groupF startcmds oK0 fud))
    startcmds = map XCmd $
                [GrabButton False (Button 3) []
                   [PointerMotionMask, ButtonReleaseMask],
		 ConfigureWindow [CWBorderWidth 0],
		 ChangeWindowAttributes [CWSaveUnder True]
		]
    route = either fromLoop fromOutside
      where
	fromLoop = either fromK fromFud
	fromOutside = toFud
	fromFud x = toOutside (Right x)
	fromK = either toSpacer (toOutside . Left)

	toOutside = Right
	toLoop = Left
	toSpacer = toLoop . Left
	--toK = toLoop . Right . Left
	toFud = toLoop . Right . Right

    oK0 = oK p0 (-1) (-1)
    oK p g size = getK $ message low high
      where
        same = oK p g size
	high _ = same
	low event =
	  case event of
	    XEvt (ButtonEvent {rootPos=rp,type'=Pressed}) -> oK p rp size
	    XEvt (MotionNotify {rootPos=rp}) ->
		if rp/=g
		then putHigh (Left (putS p')) $
		     putHigh (Right (Rect p' size)) $
		     same
		else same
	      where p' = mv rp
	    XEvt (ButtonEvent {rootPos=rp,type'=Released}) -> oK (mv rp) (-1) size
	    LEvt (LayoutSize size') ->
		(if size'/=size && g== -1
		 then putHigh (Right (Rect p size'))
		 else id) $
		oK p g size'
	    _ -> same

	mv rp = pmax 0 (p+rp-g)

putS p = hvMarginS p 0 `compS` hvAlignS aLeft aTop
