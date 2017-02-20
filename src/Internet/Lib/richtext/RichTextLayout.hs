module RichTextLayout(rtLayout,TxtCmds(..)) where
import AllFudgets
import RichText
import RichTextMetrics(MeasuredRichText(..),MeasuredPara(..),MeasuredWord(..),Word(..))
import Scans(mapAccumL)
import Data.Char(isSpace)

type TxtCmds a b = (Int,   [((Int,Int),[(DrawTxt b,Maybe (Rect,Anchor,[a]))])])
--                height    y1  y2
--type DrawTxt b = KCommand b
type DrawTxt b = Message (GCId,[DrawCommand]) b


rtLayout :: Int -> MeasuredRichText ([a],Size) -> TxtCmds a Rect

rtLayout width rt = (drawText width . getText . psLayout rt) (ctx0 (width-xmargin))

data Ctx a = Ctx Int   Int  [MeasuredWord a] (TxtLines a)
	  --     width xpos

type TxtLine a = ([ParaFmt],Int,[MeasuredWord a])
type TxtLines a = [TxtLine a]

ymargin = 0
xmargin = 4

ctx0 width = Ctx width xmargin [] []

getfield :: (Ctx a->b) -> (b->CtxT a) -> CtxT a
getfield f k ctx = k (f ctx) ctx

getxpos    = getfield (\(Ctx _ x _ _) -> x)
getwidth   = getfield (\(Ctx w _ _ _) -> w)

newLine pfs (Ctx w x l ls) =
  Ctx w xmargin [] ((pfs,x,reverse (skipspace l)):ls)

changeWidth dw (Ctx w x l ls) = Ctx (w+dw) x l ls

getText (Ctx _ _ _ ls) = reverse ls

newword wo@((_,_,Point dx _,_),_,_) = \(Ctx w x l ls) -> Ctx w (x+dx) (wo:l) ls

type CtxT a = Ctx a -> Ctx a

psLayout :: [MeasuredPara a] -> CtxT a
psLayout ps =
  case ps of
    [] -> id
    (p:ps) -> psLayout ps . pLayout p

pLayout :: MeasuredPara a -> CtxT a
pLayout (pfs,ws) =
  let dw = widthChange pfs
  in newLine pfs . changeWidth (-dw) . wsLayout pfs ws . changeWidth dw

wsLayout :: [ParaFmt] -> [MeasuredWord a] -> CtxT a
wsLayout pfs ws =
  case ws of
    [] -> id
    (w:ws) -> wLayout pfs w ws

wLayout :: [ParaFmt] -> MeasuredWord a -> [MeasuredWord a] -> CtxT a
wLayout pfs w@((gc,_,size@(Point dx _),_),cs,_) ws =
  getxpos $ \x ->
  getwidth $ \width ->
  let x' = x + dx
  in if x'>width
     then breaklineK pfs cs w ws
     else wsLayout pfs ws . newword w

breaklineK pfs cs w ws =
  newLine pfs $$
  if allIsSpace cs
  then wsLayout pfs (skipspace ws)
  else newword w $$ wsLayout pfs ws

--drawText :: Int -> TxtLines a -> TxtCmds (a,Point)
drawText width = mapAccumL (drawTxtLine width) ymargin

drawTxtLine width y (pfs,w,l) =
  let space = width-w
      (x0,xair) = case parafmt pfs of
		    FlushRight -> (xmargin+space,0)
		    Center     -> (xmargin+space `div` 2,0)
		    _          -> (xmargin,0) -- FlushLeft
      xindent=indentation pfs
      n=length l
      ln=zip l [n-i|i<-[0..]]
      ((_,_,bl,lh),cmds) =
         mapAccumL (drawTxtWord baseline) (x0+xindent,xair,2,7) ln
      baseline = y+bl
      y'=y+lh
  in (y',((y,y'),concat cmds))

parafmt [] = FlushLeft
parafmt (pf:pfs) =
  if pf `elem` [Center,FlushLeft,FlushRight]
  then pf
  else parafmt pfs

widthChange pfs = indstep*(count' Outdent pfs+count' OutdentRight pfs
                          -count' Indent pfs-count' IndentRight pfs
			  -2*count' Excerpt pfs)

indentation pfs = indstep*(count' Indent pfs+count' Excerpt pfs-count' Outdent pfs)

indstep = 10

drawTxtWord baseline (x,xair,bl,lh) (((gc,ul,Point dx wlh,wbl),w,anch),n) =
  let xsep = xair `div` n
      x' = x+xsep
  in ((x'+dx,xair-xsep,max bl wbl,max lh wlh),
      case w of
        PlainWord cs ->
	  if all isSpace cs
	  then []
	  else [let p = Point x' baseline
	            r = rR x' (baseline-wbl) dx wlh
	        in (Low (gc,[DrawString (Point x' baseline) cs]),
		    fmap (\a->(r,a,[])) anch)]++
	       (if ul
	        then let y=baseline+1
		         l=lL x' y (x'+dx) y
		     in [(Low (gc,[DrawLine l]),Nothing)]
		else [])
        SpecialWord (attrs,size) ->
	  let r = Rect (Point x' (baseline-wbl)) size
	  in [(High r,fmap (\a->(r,a,attrs)) anch)])

skipspace = dropWhile (allIsSpace.(\(_,w,_)->w))

allIsSpace (PlainWord cs) = all isSpace cs
allIsSpace _ = False

{-# MACRO $$ #-}

f $$ g = g . f

count' x = length . filter (x==)
