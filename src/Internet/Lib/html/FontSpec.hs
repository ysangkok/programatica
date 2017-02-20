module FontSpec where
import Fudgets(argReadKey)

data FontSpec
  = F { charset :: CharSet,
        weight :: Weight,
	spacing :: Spacing,
	serif :: Serif,
	slant :: Slant,
	fontsize :: Int }

defaultFont = F IsoLatin1 Medium Proportional Serif Roman fontsize
  where
    fontsize = argReadKey "fontsize" 3

bigger = adjsize 1
smaller = adjsize (-1)
adjsize n fs@(F {fontsize=fontsize}) = fs{fontsize=fontsize+n}
setsize n fs = fs{fontsize=n}
setWeight w fs = fs{weight=w}
setCharset s fs = fs{charset=s}
setSlant s fs = fs{slant=s}
setSpacing s fs = fs{spacing=s}
setSerif s fs = fs{serif=s}

data CharSet = IsoLatin1 | AdobeSymbol
data Weight = Medium | Bold
data Spacing = Fixed | Proportional
data Serif = Serif | SansSerif
data Slant = Roman | Italic

fontname (F {charset=charset,weight=weight,spacing=spacing,serif=serif,slant=slant,fontsize=fontsize}) =
    "-*-"++fmly++wght++slnt++"-*-*"++pxlsz++"-*-*-*-*-*"++encdng
  where
    (fmly,encdng) =
	case charset of
	   AdobeSymbol -> ("symbol","-*-*")
	   _ -> (case (spacing,serif) of
	            (Fixed,Serif) -> "courier"
		    (Fixed,SansSerif) -> "courier"     --"-lucidatypewriter"
		    (Proportional,SansSerif) -> "helvetica"
		    (Proportional,Serif) -> "new century schoolbook",
		 "-iso8859-1")
    wght=case weight of Bold -> "-bold"; _ -> "-medium"
    slnt=case slant of Italic -> italic; _ -> "-r"
    pxlsz="-"++show (pixelsize fontsize)
    italic = if fmly `elem` ["courier","helvetica"] then "-o" else "-i"

pixelsize n =
  case n of
    3  -> 12
    4  -> 14
    5  -> 18
    6  -> 24
    7  -> 34
    8  -> 40
    2  -> 10
    1  -> 8
    _  -> if n<3 then 7 else 50
