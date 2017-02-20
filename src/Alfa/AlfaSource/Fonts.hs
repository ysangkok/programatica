module Fonts(loadFontsF,AlfFontName(..),AlfFonts(..),FontSize(..),varFont,symbolFont,fontsize,fontSizeS) where
import AllFudgets
--import Fud
--import MakeGC(GCtx(..))
import Array

data AlfFontName
  = SymbolFont
  | DelimFont
  | DelimSymbolFont
  | VarFont
  | ConFont
  | KeyWordFont
  | LabelFont
  | CommentFont
  deriving (Eq,Ord,Ix,Bounded)

type AlfFonts = AlfFontName -> FontSize -> GCtx
type FontSize = Int

alfFont :: Array (AlfFontName,FontSize) GCtx -> AlfFonts
alfFont a n s = a!(n,max minSize (min maxSize s))

fontBounds = ((minBound,minSize),(maxBound,maxSize))
fontList :: [([FontName],Int,ColorName)]
fontList = [(fontName n size,size,colorName n) | (n,s)<-range fontBounds,
	                                         let size = sizes!s]
  where
    colorName DelimFont = delimColor
    colorName DelimSymbolFont = delimColor
    colorName KeyWordFont = keywordColor
    colorName CommentFont = commentColor
    colorName _ = fgColor

    fontName n size = (fontNames!n) (show size)
    fontNames = array (minBound,maxBound)
      [ (SymbolFont, symbolFont),
	(DelimFont, delimFont),
	(DelimSymbolFont, symbolFont),
	(VarFont, varFont),
	(ConFont, keyWordFont),
	(KeyWordFont, keyWordFont),
	(LabelFont, const [labelFont]), -- !!
	(CommentFont, commentFont) ]


sizes :: Array Int Int -- must avoid overloaded top level pattern bindings!
sizes = listArray (1,length pxls) pxls
  where pxls = argReadKey "fontsizes" [8,10,12,14,18,24,34]
(minSize,maxSize) = bounds sizes

loadFontsF :: (AlfFonts -> F a b) -> F a b   
loadFontsF fud =
    conts mkFont fontList $ \fonts->
    fud (alfFont (listArray fontBounds fonts))
  where
    mkFont (fn,size,cn) k =
      wCreateGCtx rootGCtx [GCFont (fn++["fixed"]),
			    GCLineWidth (size `div` 9),
			    GCCapStyle CapProjecting,
			    -- GCJoinStyle JoinMiter, -- not yet in fudgetlib
			    GCForeground [cn,fgColor]] k

symbolFont s =
  [argKey "symbolfont" $
   "-*-"++symbolfamily++"-medium-r-*-*-"++s++"-*-*-*-*-*-*-*"]
delimFont s =
  ["-*-"++delimfamily++"-medium-r-*-*-"++s++"-*-*-*-*-*-"++enc,
   "-*-"++fontfamily++"-medium-r-*-*-"++s++"-*-*-*-*-*-"++enc]
varFont s =
  ("-*-"++fontfamily++"-medium-"++varslant++"-*-*-"++s++"-*-*-*-*-*-"++enc):
  delimFont s
keyWordFont s =
  ("-*-"++delimfamily++"-bold-r-*-*-"++s++"-*-*-*-*-*-"++enc):
  delimFont s
commentFont s =
  [argKey "commentfont" $
  "-*-"++fontfamily++"-medium-r-*-*-"++s++"-*-*-*-*-*-"++enc]

fontSizeS = show(sizes!fontsize)
fontsize = argReadKey "fontsize" 4 :: FontSize
fontfamily = argKey "fontfamily" "new century schoolbook"
symbolfamily = argKey "symbolfamily" "symbol"
delimfamily = argKey "delimfamily" fontfamily
varslant = argKey "varslant" "i"
enc = argKey "encoding" "iso8859-1"

keywordColor = argKey "keywordcolor" fgColor
delimColor = argKey "delimcolor" keywordColor
commentColor = argKey "commentcolor" fgColor
