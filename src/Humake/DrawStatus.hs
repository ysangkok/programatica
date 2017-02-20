module DrawStatus where
import Fudgets
import Modules(Status(..))

createDrawStatus cont =
    fg ["black"] $ \ black ->
    fg ["red","#ff3300","#ff0033","#cc0000","grey40","black"] $ \ red ->
    fg ["yellow","#ffff33","white"] $ \ yellow ->
    fg ["green3","#00cc00","#00c000","green","black"] $ \ green ->
    fg ["blue3","#0000cc","#0000c0","blue","black"] $ \ blue ->
    let drawStatus status =
          g $ FixCD 12 $
	  case status of
	    Unknown     -> [noLight]
	    Nonexistent -> redLight
	    Good        -> greenLight
	    Library     -> blueLight
	    OutOfDate   -> amberLight
	    InError     -> redLight

	redLight = colLight red
	amberLight = colLight yellow
	greenLight = colLight green
	blueLight = colLight blue
	colLight fg = [light fg,noLight]
	light fg = (fg,[FillArc (Rect 2 9) 0 (64*360)])
	noLight = (black,[DrawArc (Rect 2 9) 0 (64*360)])
	{- old:
	colLight fg = stackD [hardAttribD fg light,noLight]
	light = g (FixD 12 [FillArc (Rect 2 9) 0 (64*360)])
	noLight = g (FixD 12 [DrawArc (Rect 2 9) 0 (64*360)])
	-}
    in cont drawStatus
  where
    fg colspec cont =
      wCreateGCtx rootGCtx (gcFgA colspec) $ cont . gctx2gc


--instance Graphic Status where measureGraphicK = measureGraphicK . drawStatus
