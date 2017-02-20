module DrawCompiledGraphics2(drawK',drawChangesK,GCId) where
import Fudget
import Xtypes
import XDraw(DrawCommand(FillRectangle),clearArea,drawMany,wDrawMany,Drawable(..))
import Geometry(growrect,(=.>),Rect(rectsize))
import Message
import NullF(putsK,putK)
import Utils(number)
import EitherUtils(mapfilter)
import CompiledGraphics
import Rects
--import Maptrace(ctrace) -- debug
--import Io(echoK) -- debug
import FudgetIO(putLow)

--tr x = seq x $ ctrace "drawtrace" x x
--trLow = Low . tr
--trLow = tr . Low
--maptrLow = map trLow
--debugK = echoK

drawK = drawK' MyWindow
drawK' d higc clip cg =
    case draw cg [] of
      [] -> id
      cmds -> putLow $ drawMany d cmds
  where
    draw (CGMark cg) = draw cg
    draw (CGraphics r cur cmds gs) =
      drawCursor cur higc clip r . (cmds++) . draws gs
    draws [] = id
    draws (g:gs) = draw g . draws gs

drawCursor cur (higc,hiR) clip r =
  if cur
  then ((higc,[FillRectangle cr | hr<-hiR r,cr<-clip hr]):)
  else id

drawCursorK cur higc clip r =
  putK $ Low $ wDrawMany $ drawCursor cur higc clip r []

drawChangesK beQuick higc  (CGMark cg) (CGMark ocg) changes =
    --debugK (show [ ps | ps<-changes, take 1 ps/=[0]]) .
    drawChangesK beQuick higc cg ocg (mapfilter drop0 changes)
  where drop0 [] = Just []
        drop0 (0:ps) = Just ps
	drop0 _ = Nothing

drawChangesK beQuick higc  cg@(CGraphics r  _ _ cgs )
                          ocg@(CGraphics or _ _ ocgs) changes =
    --debugK (unwords ["Changes:",show changes,"or",show or,"nr",show r]) .
    if null changes
    then if r==or
	 then --debugK "Pruning" .
	      id
	 else redrawOldK beQuick higc cg ocg
    else if [] `elem` changes
	 then --debugK "Drawing" .
              redrawK' beQuick higc cg r or
	 else --debugK "Descending" .
	      let changes' i= [ p | i':p <- changes, i'==i]
	      in foldr (.) id [drawChangesK beQuick higc cg ocg (changes' i) |
				(i,(cg,ocg))<-number 1 (zip cgs ocgs)]

drawChangesK beQuick higc  cg ogc _ =
    --debugK "drawNewK" .
    drawNewK cg
  where
    drawNewK (CGMark cg) = drawNewK cg
    drawNewK cg@(CGraphics r _ _ _) =
      redrawK' beQuick higc cg r (cgrect ogc)

eraseOldK newrect oldrect =
  -- It's enough to clear the part of oldrect that is outside newrect.
  ifK (newrect/=oldrect) (putK (Low $ clearArea (growrect oldrect 1) False))

redrawK' beQuick higc cg r or =
  eraseOldK r or .
  redrawK beQuick higc cg

redrawK beQuick higc (CGMark cg) = redrawK beQuick higc cg
redrawK beQuick higc cg@(CGraphics r _ _ _) =
    if not beQuick || rectsize r =.> 400 -- heuristic
    then -- for big areas: wait for exposure event and draw only the
	 -- visible part
	 putK (Low $ clearArea r True) 
    else -- for small areas: draw everything immediately (reduced flicker)
	 putK (Low $ clearArea r False) . drawK higc (:[]) cg

-- Pre: cg and ocg have exactly the same structure,
--      enclosing rectanges and drawing commands may have changed.
redrawOldK beQuick higc cg@(CGraphics r  cur cmds cgs )
                        ocg@(CGraphics or _ ocmds ocgs) =
  if r==or
  then id
  else if null cmds && null ocmds -- greedy version of cmds==ocmds
          && rectsize r =.> 200 -- tradeoff, going all the way to the leaves
			        -- just to save some drawing commands isn't
                                -- worth it.
       then drawCursorK cur higc (intersectRects (diffRect r or)) r .
            foldr (.) id (zipWith (redrawOldK beQuick higc) cgs ocgs)
       else redrawK' beQuick higc cg r or
redrawOldK beQuick higc (CGMark cg) (CGMark ocg) =
  redrawOldK beQuick higc cg ocg
redrawOldK beQuick higc cg ocg =
  redrawK' beQuick higc cg (cgrect cg) (cgrect ocg)

ifK b k = if b then k else id
