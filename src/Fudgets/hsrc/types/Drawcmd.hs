module Drawcmd(moveDrawCommands, moveDrawCommand) where
import DrawTypes
import Geometry(moveline, moverect, padd)
--import Xtypes

moveDrawCommand cmd v =
    case cmd of
      DrawLine l -> DrawLine (moveline l v)
      DrawLines cm ps -> DrawLines cm (movePoints cm ps v)
      DrawImageString p s -> DrawImageString (padd p v) s
      DrawString p s -> DrawString (padd p v) s
      DrawRectangle r -> DrawRectangle (moverect r v)
      FillRectangle r -> FillRectangle (moverect r v)
      FillPolygon shape coordMode ps ->
	FillPolygon shape coordMode (movePoints coordMode ps v)
      DrawArc r a1 a2 -> DrawArc (moverect r v) a1 a2
      FillArc r a1 a2 -> FillArc (moverect r v) a1 a2
      CopyArea d r p -> CopyArea d r (padd p v)
      CopyPlane d r p n -> CopyPlane d r (padd p v) n
      DrawPoint p -> DrawPoint (padd p v)
      CreatePutImage r fmt pxls -> CreatePutImage (moverect r v) fmt pxls
      DrawImageStringPS p s -> DrawImageStringPS (padd p v) s
      DrawStringPS p s -> DrawStringPS (padd p v) s
      DrawImageString16 p s -> DrawImageString16 (padd p v) s
      DrawString16 p s -> DrawString16 (padd p v) s

movePoints cm ps v =
 case cm of
   CoordModeOrigin -> map (padd v) ps
   CoordModePrevious ->
     case ps of
       p:ps' -> padd v p:ps'
       [] -> []

moveDrawCommands cmds p = map (`moveDrawCommand` p) cmds
