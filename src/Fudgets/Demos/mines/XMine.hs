module Main where
--import Trace
import ListMap(lookupWithDefault)
import Word
import AllFudgets
import FUtil
import MineField
--import BitMaps
import Time

main = getTimeDouble >>= fudlogue . shellF "XMines" . xmines

myFont = defaultFont
separation = 5::Int

xmines d =
	let title = untaggedListLF horizontalP [labelF "FXMines v1.2"]
	    controls = untaggedListLF horizontalP [labelF "Mine count", inp initMineCount, bf "Hint" Hint, bf "Restart" New, bq "Quit"]
	    msg1 = disp ""
	    msg2 = disp ""
	    panel = untaggedListLF verticalP [ignoreI title,				-- 0
							   ignoreI controls, 				-- 1
	                                                   msg1 >=^< (\(Status s)->s),		-- 2
                                                           msg2 >=^< (\(Msg s)->s), 		-- 3
							   boxes' >=^< (\(Repaint l)->l)] >=^< directMsg -- 4
							   where directMsg m@(Repaint l) = (4, m)
							         directMsg m@(Msg s)     = (3, m)
							         directMsg m@(Status s)  = (2, m)
	    but s = buttonF s
	    bf s f = (\_ -> f) >^=< but s
	    bq s = quitF >==< but s
	    disp s = displayF
	    inp n = (\x->case x of { InputChange n -> SetSize n; _ -> Noop} ) >^=< ignoreI (intWF 8 n)
	    (xn, yn) = mineFieldSize
	    boxes :: F [(Coord, Symbol)] MInput
	    boxes = (\(a,f)->f a) >^=< listLF (matrixP xn) [((x,y), boxF) | x<-[1..xn], y<-[1..yn]] >=^^< concatSP
	    boxes' = --allcacheF
	             boxes
	in  loopF ({-debugF show show-} panel >==< absF (playSP d))

pictSize :: Size
pictSize = Point 32 32
initcmds = [XCmd $ ChangeWindowAttributes [CWEventMask [ExposureMask]], but 1, but 2, but 3] 
           where but t = XCmd $ GrabButton True (Button t) [] [ButtonPressMask, ButtonReleaseMask]
boxF :: F Symbol (Coord -> MInput)
boxF = windowF initcmds (readBitmaps (map snd bitmaps) (\ ps -> let sps = [(s, p) | ((s,_),p)<-zip bitmaps ps] in
                             (simpleK (boxK sps) pictSize SBlank)))
boxK :: [(Symbol,PixmapId)] -> Drawer -> Drawer -> Fms' Symbol Symbol (Coord -> MInput)
boxK ps draw clear s event =
    case event of
       High s'                                             -> (s', redraw ps s')
       Low (XEvt (Expose _ 0))                             -> (s, redraw ps s)
       Low (XEvt (ButtonEvent _ _ _ _ Pressed (Button b))) -> (s, case b of { 1 -> [High Move]; 2 -> [High Bomb]; 3 -> [High Free]; _ -> []})
--     Low (LayoutSize nsize)                     -> (s, ??)
       _ -> (s,[])
    where redraw ps SBlank = [Low $ XCmd ClearWindow]
          redraw ps s = 
		let p = lookupWithDefault ps (error ("No bitmap for "++show s)) s
		in  [Low $ XCmd ClearWindow, Low (draw (CopyPlane (Pixmap p) (Rect (Point 0 0) (Point 32 32)) (Point 0 0) 0))]
{-
          redraw ps (SNumber 0) = [Low ClearWindow, Low (draw (CopyPlane (Pixmap ps) (Rect (Point 0 0) (Point 32 32)) (Point 0 0) 0))]
          redraw ps (SNumber n) = [Low ClearWindow, Low (draw (DrawImageString tp (show n)))]
          redraw ps (SCurrent n) = [Low ClearWindow, Low (draw (DrawImageString tp ("X"++show n)))]
	  redraw ps SBombX = [Low ClearWindow, Low (draw (DrawImageString tp "BX"))]
	  redraw ps SBombDone = [Low ClearWindow, Low (draw (DrawImageString tp "BD"))]
	  redraw ps SBomb = [Low ClearWindow, Low (draw (DrawImageString tp "B"))]
	  redraw ps SFree = [Low ClearWindow, Low (draw (DrawImageString tp "F"))]
tp = Point 10 18
-}

bitmaps = 
    [(SNumber n,"num_"++show n++".bm") | n<-[0..5]] ++
    [(SCurrent n,"cur_"++show n++".bm") | n<-[0..5]] ++
    [(SBombX, "bombx.bm"), (SBombDone, "bombdone.bm"), (SBomb, "bomb.bm"), (SFree, "free.bm")]

readBitmaps :: [String] -> ([PixmapId] -> K a b) -> K a b
readBitmaps ns f = readb [] ns f
	where readb ps [] f = f ps
              readb ps (n:ns) f = readBitmapFile n $ \r ->
	                          case r of
				    BitmapBad -> error ("Cannot read bitmap "++n)
				    BitmapReturn _ _ p -> readb (ps++[p]) ns f
