module DrawStatus where
import Fudgets

createDrawStatus cont =
    fg ["black"] $ \ black ->
    fg ["red","#ff3300","#ff0033","#cc0000","grey40","black"] $ \ red ->
    fg ["green3","#00cc00","#00c000","green","white"] $ \ green ->
    let drawStatus ok =
          g $ FixCD 12 $
	  if ok
	  then greenLight
	  else redLight

	redLight = colLight red
	greenLight = colLight green
	colLight fg = [light fg,noLight]
	light fg = (fg,[FillArc (Rect 2 9) 0 (64*360)])
	noLight = (black,[DrawArc (Rect 2 9) 0 (64*360)])
    in cont drawStatus
  where
    fg colspec cont =
      wCreateGCtx rootGCtx (gcFgA colspec) $ cont . gctx2gc
