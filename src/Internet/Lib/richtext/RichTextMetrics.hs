{-# COMPILERFLAGS -fno-overload-restr #-}
module RichTextMetrics(measureRtK,MeasuredRichText(..),MeasuredWord(..),MeasuredPara(..),Word(..)) where
import AllFudgets
import RichText
import RichTextFormatter
--import SpecialMsg
import NonStdTrace(trace)
import Utils2(ord)
import Data.Ix

{-
measureRtK :: FormattedRichText special ->
              MKont (MeasuredRichText special) special b c
-}

measureRtK ps k = measurePsK [] ps k

type MeasuredWord a = ((GCId,Bool,Size,Int),Word a,Maybe Anchor)
type MeasuredPara a = ([ParaFmt],[MeasuredWord a])
type MeasuredRichText a = [MeasuredPara a]

type Kont res a b = (res -> K a b) -> K a b
type MKont res b a c = Kont res (Either Size a) (Either b c)
-- If only type vars could be quantified locally in a type def...

-- measurePsK ::
measurePsK mps ps k =
  case ps of
    [] -> k (reverse mps)
    (p:ps) -> measurepK p (\mp -> measurePsK (mp:mps) ps k)

--measurepK :: RichTextPara a -> MKont (MeasuredPara a) a b
measurepK (pfs,ws) k = measureWsK [] ws (\mws->k (pfs,mws))

--measureWsK :: [MeasuredWord a] -> [RichTextWords a] -> MKont [MeasuredWord a] a b
measureWsK mws ws k =
  case ws of
    []     -> k mws
    (w:ws) -> measurewK w (\mw->measureWsK (mws++mw) ws k)

--measurewK :: RichTextWords a -> MKont [MeasuredWord a] a b
measurewK (cfs,words) k =
  safeLoadQueryFont (fontname cfs) $ \ fs ->
  getColorAttrs cfs $ \ gcattrs ->
  wCreateGC rootGC (GCFont (font_id fs):gcattrs) $ \gc ->
  let size cs = Point (next_pos' cfs fs cs) h
      ascent = font_ascent fs
      h = linespace fs
      d = font_descent fs
      anchr = anchor cfs
      underline = Underline `elem` cfs
      mw size a w = ((gc,underline,size,a),w,anchr)
      getSize w =
        case w of
	  PlainWord s      -> mw (size s) ascent w
	  SpecialWord (_,size) -> mw size (ycoord size-d) w
  in k (map getSize words)

fontname cfs =
    "-*"++fmly++wght++slant++"-*-*"++pxlsz++"-*-*-*-*-*"++encdng
  where fmly=if issymbol
             then "-symbol"
	     else case (isfixed,issans) of
	            (True,True) -> "-courier"
		    (True,False) -> "-courier"     --"-lucidatypewriter"
		    (False,True) -> "-helvetica"
		    (False,False) -> "-new century schoolbook"
        wght=if Bold `elem` cfs then "-bold" else "-medium"
        slant=if Italic `elem` cfs then italic else "-r"
	pxlsz="-"++show (pixelsize cfs)
	encdng= if issymbol then "-*-*" else "-iso8859-1"
	isfixed = Fixed `elem` cfs
	issans = Sans `elem` cfs
	issymbol = Symbol `elem` cfs
        italic = if isfixed || issans then "-o" else "-i"

        pixelsize cfs = fontsize (count Bigger cfs - count Smaller cfs)
        count x xs = length (filter (x==) xs)
        fontsize n =
           case n of
             0  -> 12
	     1  -> 14
	     2  -> 18
	     3  -> 24
	     4  -> 34
	     -1 -> 10
	     -2 -> 8
	     _  -> if n<0 then 8 else 34


getColorAttrs cfs cont =
  case [c | PenColor c<-cfs] of
    []  -> cont []
    c:_ -> allocNamedColorDefPixel defaultColormap c "black" $ \ pixel ->
           cont [GCForeground pixel]

anchor [] = Nothing
anchor (Anchor s:_) = Just s
anchor (_:cfs) = anchor cfs

--
next_pos' cfs fs cs =
  case part (inRange (font_range fs)) cs of
    (cs,[]) -> next_pos fs cs
    (cs,badcs) -> trace (show (cfs,cs,badcs,mapList ord badcs)) (next_pos fs cs)

--mapList = map :: ((a->b)->[a]->[b])
