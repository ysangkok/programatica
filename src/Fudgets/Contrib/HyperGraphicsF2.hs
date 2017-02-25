module HyperGraphicsF2(
  module HyperGraphicsF2,
  GfxCommand(..),GfxEvent(..),replaceGfx
 ) where
import AllFudgets
import Maybe(fromJust,mapMaybe)
import ListUtil(mapFst)
import qualified Data.Map.Strict as Map
--import Fudget
--import Defaults(paperColor)
--import InputMsg(inputDone)
--import Utils(swap)
--import Xtypes(ColorName)
--import Loops(loopThroughRightF)
--import SerCompF(mapstateF)
--import Graphic
--import Drawing
--import DrawingOps
--import GraphicsF
--import FDefaults
--import ListUtil(assoc)
--import Sizing(Sizing(..))
--import GCAttrs() -- instances
--import Event(Pressed(..))
--import Maptrace(ctrace) -- debugging
--import SpyF(teeF) -- debugging
--import CompOps((>==<)) -- debugging

#include "../hsrc/exists.h"

highlightGfx path on = ChangeGfx [(path,(on,Nothing))]

hyperGraphicsF2 x = hyperGraphicsF2' standard x

hyperGraphicsF2' custom init =
    loopThroughRightF
	(mapstateF ctrl state0)
	({-teeF show "\n" >==<-} graphicsDispF' (custom . params))
  where
    --tr x = ctrace "hyper" (show x) x -- debugging
    params = setInitDisp init .
	     setGfxEventMask [GfxButtonMask] .
	     setSizing Dynamic
    state0 = (annotPaths init,init)

    ctrl state@(paths,drawing) = either gfxEvent gfxCommand
      where
        same = (state,[])
	lbl2path lbl = fromJust (Map.lookup lbl paths)

        gfxCommand lcmd =
	    case mapGfxCommandPath lbl2path lcmd of
	      cmd@(ChangeGfx changes) -> (changeState changes,[Left cmd])
	      cmd -> (state,[Left cmd])
	  where
	    changeState changes = (paths',drawing')
	      where
	        drawing' = foldr replace drawing
		                 [(path,d)|(path,(_,Just d))<-changes]
		replace (path,d) drawing = replacePart drawing path d

		paths' = annotPaths drawing'
			-- Space leak: drawing' isn't used until user clicks
			-- in the window, so the old drawing is retained in
			-- the closure for drawing'

        gfxEvent msg = (state,[Right msg'])
	  where
	    msg' = mapGfxEventPath path2lbl msg

        path2lbl path = do let part = drawingAnnotPart drawing path
			   LabelD a _ <- maybeDrawingPart drawing part
			   return a

    annotPaths = Map.fromList . map swap . drawingAnnots

mouseClicksSP = mapFilterSP isMouseClick

isMouseClick msg =
    case msg of
      GfxButtonEvent { gfxType=Pressed, gfxPaths=(path,_):_ } -> Just path
      _ -> Nothing

---

mapGfxCommandPath f cmd =
    case cmd of
      ChangeGfx changes -> ChangeGfx (mapFst f changes)
      ShowGfx path a -> ShowGfx (f path) a
      GetGfxPlaces paths -> GetGfxPlaces (map f paths)
      -- _ -> cmd -- Operationally, the rest is the same as this line.
      ChangeGfxBg c -> ChangeGfxBg c
      ChangeGfxBgPixmap pm b -> ChangeGfxBgPixmap pm b
#ifdef USE_EXIST_Q
      ChangeGfxBgGfx gfx -> ChangeGfxBgGfx gfx
#endif
      ChangeGfxCursor cursor -> ChangeGfxCursor cursor
      ChangeGfxFontCursor shape -> ChangeGfxFontCursor shape
      BellGfx n -> BellGfx n


mapGfxEventPath f event =
  case event of
    GfxButtonEvent s t b ps -> GfxButtonEvent s t b (mapPaths ps)
    GfxMotionEvent s ps     -> GfxMotionEvent s (mapPaths ps)
    GfxKeyEvent m k l       -> GfxKeyEvent m k l
    GfxFocusEvent b         -> GfxFocusEvent b
    GfxPlaces rs            -> GfxPlaces rs
    GfxResized s            -> GfxResized s
  where
    -- mapPats :: [(a,(Point,Rect))] -> [(b,(Point,Rect))]
    mapPaths = mapMaybe f'
    -- f' :: (a,(Point,Rect)) -> Maybe (b,(Point,Rect))
    f' (path,place) = fmap (\p->(p,place)) (f path)

-- nullPath = null . gfxPaths -- would be ok if gfxPaths was a total function
nullPath = maybe False null . gfxEventPaths

gfxEventPaths event =
  case event of
    -- enumerate all constructors that have a path argument:
    GfxButtonEvent {gfxPaths=ps} -> Just ps
    GfxMotionEvent {gfxPaths=ps}  -> Just ps
    _ -> Nothing

isGfxButtonEvent (GfxButtonEvent {gfxType=Pressed,gfxButton=b}) = Just b
isGfxButtonEvent _ = Nothing
