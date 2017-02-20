module Bitmaps where
import AllFudgets
import Graphic
import MeasuredGraphics(MeasuredGraphics(..))
import MakeGC(GCtx(..))

data BitmapFile = BitmapFile String

instance Graphic BitmapFile where
  measureGraphicK (BitmapFile filename) gctx@(GC gc fs) k =
    readBitmapFile filename $ \ bmret ->
    case bmret of
      BitmapReturn size _ pixmap ->
          wCreateGC gc [GCGraphicsExposures False] $ \ gc' ->
          k (LeafM ll (GC gc' fs) drawit)
	where r = Rect origin size
              ll = Layout size True True
	      drawit (Rect p _) = [CopyPlane (Pixmap pixmap) r p 0]
      _ ->
          appendChanK stderr ("Failed to load bitmap "++filename++"\n") $
          k (LeafM ll gctx drawit)
	where ll = Layout (pP 20 20) False False
	      drawit r = [FillRectangle r]
