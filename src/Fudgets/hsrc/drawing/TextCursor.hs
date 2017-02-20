module TextCursor where
import Graphic
import Command
import Geometry
import LayoutRequest
import MeasuredGraphics
import GCtx
import Font(font_ascent,font_descent)

data TextCursor = TextCursor

instance Graphic TextCursor where
  measureGraphicK TextCursor gctx@(GC gc fs) k =
    let d = font_descent fs
	a = font_ascent fs
	p1 = Point 0 a
	h = a+d
	size = Point 1 h
	drawit (Rect p (Point _ h)) = [FillRectangle (Rect p size)]
    in k (LeafM (refpLayout size True True [p1]) gctx drawit)
