module DrawCompiledGraphics3(drawK',drawChangesK) where
import XDraw(DrawCommand(FillRectangle),clearArea,drawMany,Drawable(..))
import Fudget
import FudgetIO
import NullF(putsK,putK,F,K)
import CompiledGraphics
import Rects
import DebugK
import Utils(number)
import EitherUtils(mapfilter)
import Geometry(growrect)
--import Maptrace(ctrace)

drawK = drawK' MyWindow
drawK' d (higc,hiR) clip cg =
    case draw cg [] of
      [] -> id
      cmds -> putLow $ drawMany d cmds
  where
    draw (CGMark cg) = draw cg
    draw (CGraphics r cur cmds gs) =
      (if cur
       then ((higc,[FillRectangle cr | hr<-hiR r,cr<-clip hr]):)
       else id).
      (cmds++) .
      draws gs
    draws [] = id
    draws (g:gs) = draw g . draws gs

drawChangesK beQuick higc  (CGMark cg) (CGMark ocg) changes =
    --debugK (show [ ps | ps<-changes, take 1 ps/=[0]]) .
    drawChangesK beQuick higc cg ocg (mapfilter drop0 changes)
  where drop0 [] = Just []
        drop0 (0:ps) = Just ps
	drop0 _ = Nothing

drawChangesK bg higc  cg@(CGraphics  r  cur  cmds  cgs)
                     ocg@(CGraphics or ocur ocmds ocgs) changes =
    debugK (unwords ["Changes:",show changes,"or",show or,"nr",show r]) .
    if null changes && r==or
    then debugK "Pruning" .
         id
    else if [] `elem` changes
	 then drawAllFromThisNodeK bg higc cg ocg
	 else if r/=or
	      then -- !! if rectsize r==rectsize or then  scrolling is enough
		   if null cmds && null ocmds -- faster than cmds==ocmds
		      && cur==ocur
		   then -- no drawing at this node, but some children changed
			clearRegion (diffRect or r) .
			drawChildrenK
		   else drawAllFromThisNodeK bg higc cg ocg
	      else drawChildrenK
  where
    drawChildrenK =
      --debugK "Descending" .
      let changes' i= [ p | i':p <- changes, i'==i]
      in foldr (.) id [drawChangesK bg higc cg ocg (changes' i) |
			(i,(cg,ocg))<-number 1 (zip cgs ocgs)]

drawChangesK beQuick higc  cg ogc _ =
    --debugK "drawNewK" .
    drawAllFromThisNodeK beQuick higc (nomark cg) (nomark ogc)
  where
    nomark (CGMark cg) = nomark cg
    nomark cg = cg

drawAllFromThisNodeK beQuick higc cg@(CGraphics r _ _ _)
                                  ogc@(CGraphics or _ _ _) =
  debugK "Drawing" . -- This node changed: redraw
  -- Clear the part of or that is outside r.
  clearRegion (diffRect or r) .
  if True -- rectsize r =.> 400 -- heuristic
  then -- for big areas: wait for exposure event and draw only the
       -- visible part
       putK (Low $ clearArea r True) 
  else -- for small areas: draw everything immediately (reduced flicker)
       putK (Low $ clearArea r False) . drawK higc (:[]) cg

ifK b k = if b then k else id

clearRegion [] = id
clearRegion rs = putsK [Low $ clearArea (growrect r 1) False | r<-rs]
