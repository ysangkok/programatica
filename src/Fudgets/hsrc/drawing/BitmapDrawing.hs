module BitmapDrawing where
import Graphic
import MeasuredGraphics(MeasuredGraphics(..))
import GCtx(GCtx(..))
--import Command
import Event(BitmapReturn(..))
import DrawTypes
import Geometry(Rect(..),origin)
import Xtypes
--import FudgetIO
import NullF() -- instances, for hbc
import LayoutRequest(refpLayout,plainLayout)
import Gc
import StdIoUtil(appendChanK)
import Pixmap(readBitmapFile)
import Maybe(maybeToList)
import ContinuationIO(stderr)

data BitmapFile = BitmapFile String

instance Graphic BitmapFile where
  measureGraphicK (BitmapFile filename) (GC gc _) k =
    readBitmapFile filename $ \ bmret ->
    case bmret of
      BitmapReturn size optrefp pixmap ->
          wCreateGC gc [GCGraphicsExposures False] $ \ gc' ->
          k (LeafM ll (drawit gc'))
	where r = Rect origin size
              ll = refpLayout size True True (maybeToList optrefp)
	      drawit gc (Rect p _) = [(gc,[CopyPlane (Pixmap pixmap) r p 0])]
      _ ->
          appendChanK stderr ("Failed to load bitmap "++filename++"\n") $
          k (LeafM ll drawit)
	where ll = plainLayout 20 False False
	      drawit r = [(gc,[FillRectangle r])]
