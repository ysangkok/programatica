module InfoDispF(infoDispF) where
import Fud
import Fudgets
import Gedit(stdSelect,parentPath,build)
import UAbstract(ExpAnnot(..),Exp(..))
import UAnnots
import AlfSyntax(Syntax(..))
import Annot
import EditAnnot
import DrawAlf(AlfAnnot)
--import AlfState(AlfState)
--import AlfOps(AlfEditMsg)

infoDispF state0 =
    scroll $ loopThroughRightF (Left>^=<mapstateF ctrl state0) gfxDispF
  where
    gfxDispF = graphicsDispF' custom
	where
	  custom = setInitDisp (fst state0) . setSizing Dynamic .
		   setGfxEventMask [GfxButtonMask] . setAdjustSize False
    initD = fst state0 :: (ADrawing AlfAnnot Syntax)
    scroll = if argFlag "scrollstatus" False
	     then oldScrollF False (10,500)
	     else spacer1F (maxSizeS (pP 2000 250)) -- quick hack

    ctrl state@(drawing,(draw,drawTop)) = either fromGfxDisp fromOutside
      where
	same = (state,[])
	new state' = (state',[replaceAllGfx (fst state')])
	fromOutside = new
	fromGfxDisp msg =
	  case msg of
	    GfxButtonEvent {gfxButton=Button 1,gfxType=Pressed, gfxPaths=ps } ->
	      case ps of
		((path',_):_) ->
		    case path of
		      [] -> same -- e.g. if user click on an error message
		      _ -> case part of
		             --ExpAnnotS (FoldTo e) -> flipParent UnfoldTo e
			     --ExpAnnotS (UnfoldTo e) -> flipParent FoldTo e
			     _ -> same
		  where
		    path = stdSelect drawing path'
		    parentpath = parentPath drawing path
		    part = build (drawingPart drawing path)
		    parentdrawing = drawingPart drawing parentpath
		    parent = build parentdrawing
		    flipParent a e =
		      case parent of
			ExpS (EAnnot _ e') -> replaceParent (ExpS (EAnnot (a e') e))
			_ -> same
		    replaceParent e = (state',[ChangeGfx [(parentpath,(False,Just pdrawing'))]])
		      where
		        state' = (drawing',(draw,drawTop))
			drawing' = replacePart drawing parentpath pdrawing'
		        pdrawing' = --undefined {-
			  case parentdrawing of
			    LabelD (Annot a _) _ -> draw a e
			    _ -> drawTop e -- !!
			    --}
		_ -> same
	    _ -> same
