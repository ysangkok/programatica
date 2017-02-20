module HyperGraphics where
import AllFudgets
import Graphic
import Drawing
--import DrawingOps
import GraphicsF
import ListUtil(assoc)

hyperGraphicsF = hyperGraphicsF' 1 paperColor

hyperGraphicsF' :: (Eq annot,Graphic g) =>
                  Int -> ColorName ->
                  Drawing annot g ->
		  F (Either (Drawing annot g) (annot,Drawing annot g))
		    annot
hyperGraphicsF' bw bgcol init =
    loopThroughRightF (mapstateF ctrl state0) (graphicsDispF bw bgcol init)
  where
    state0 = (annotPaths init,init)

    ctrl state@(paths,drawing) = either input output
      where
        same = (state,[])
        output = either new newpart
	  where
	    new d = newpart' d []
	    newpart (a,d) = assoc (newpart' d) same paths a
	    newpart' d path = ((paths',drawing'),[replaceGfxPart d path])
	      where drawing' = replacePart drawing path d
	            paths' = annotPaths drawing'

            replaceGfxPart d path = Left (ChangeGfx [(path,(False,Just d))])

        input = either mouse keyboard
	  where
	    mouse msg =
	      case inputDone msg of
	        Just (path:_) ->
		  case maybeDrawingPart drawing (drawingAnnotPart drawing path) of
		    Just (LabelD a _) -> (state,[Right a])
		    _ -> same
		_ -> same

	    keyboard _ = same

    annotPaths = map swap . drawingAnnots
