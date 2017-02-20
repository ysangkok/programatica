module Html2RichText(html2RichText,SpecialType(..),specials) where
import Html
import HtmlTags
import HtmlOps
import RichText
import Data.ListUtil(assoc)
import Fudgets(argFlag,argKey)
import Utils2(expandtabs,pair)
import NameMonad
import NonStdTrace(trace)

-- A quick hack...

data SpecialType = Inp | Img | Fupplet deriving (Eq,Ord)

html2RichText :: Html -> RichText ((SpecialType,TagAttrs),Int)
html2RichText = doN 0 . html2rt

html2rt = concatMaprN htmlItem2rt

htmlItem2rt item =
  case item of
    HtmlChars s -> unitN [PlainChars s]
    HtmlContext ctx html -> htmlCtx2rt ctx html
    HtmlCommand cmd -> htmlCmd2rt cmd
    HtmlGarbage tag -> rtGarb tag

htmlCtx2rt ctx@(name,attrs) html =
  case name of
    PRE -> htmlCtx2rt' ctx `mapN` (html2rt (brklines html))
    SELECT -> special (Inp,[])
    TEXTAREA -> special (Inp,[])
    FUPPLET -> special (Fupplet,[]) -- attrs and enclosed html?
    _ -> htmlCtx2rt' ctx `mapN` (html2rt html)

special t = (:[]) `mapN` special1 t
special1 t = (Special. pair t) `mapN` getN

htmlCtx2rt' (name,attrs) =
  case name of
    HTML -> id
    HEAD -> id
    TITLE -> const []
    BODY -> id
    FORM -> id
    MAP -> const [] -- !! client side image maps not implemented
    PRE -> para [Paragraph] . chrs [Fixed]
    LISTING -> para [Paragraph] . chrs [Fixed]
    PLAINTEXT -> para [Paragraph] . chrs [Fixed]
    BLOCKQUOTE -> para [Paragraph,Excerpt] . chrs [Fixed]
    CENTER -> para [Paragraph,Center]
    A -> case lookup "HREF" attrs of
             Just a -> chrs (sans++[Underline,Anchor a,PenColor linkColor])
	     _ -> chrs (sans++[PenColor "darkgreen"])
    H1 -> hdr 1
    H2 -> hdr 2
    H3 -> hdr 3
    H4 -> hdr 4
    H5 -> hdr 5
    H6 -> hdr 6
    EM -> chrs [Italic]
    STRONG -> chrs [Bold]
    CODE -> chrs [Fixed]
    SAMP -> chrs [Fixed]
    KBD -> chrs [Fixed]
    VAR -> chrs [Italic]
    DFN -> chrs [Italic]
    CITE -> chrs [Italic]
    B -> chrs [Bold]
    I -> chrs [Italic]
    U -> chrs [Underline]
    TT -> chrs [Fixed]
    SMALL -> chrs [Smaller]
    BIG -> chrs [Bigger]
    FONT -> chrs (fontsize (lookup "SIZE" attrs))
                -- !!! Nested absolute size changes won't work properly.
    SANS -> chrs [Sans]
    DL -> para [Paragraph,Indent]
    UL -> para [Paragraph,Indent]
    OL -> para [Paragraph,Indent]
    MENU -> para [Paragraph,Indent]
    DIR -> para [Paragraph,Indent]
    ADDRESS -> chrs [Italic]
    TABLE -> para [Paragraph] -- !!
    TR -> (NewLine:) -- !!
    TH -> chrs [Bold]
    TD -> id -- !!
    CAPTION -> id -- !!
    -- Added stuff for testing HtmlParser2:
    P -> para [Paragraph]
    LI -> para [Paragraph] .
	    if symbol
            then (chrs [Symbol] [PlainChars "\xb7 "]++)
            else (PlainChars "* ":)
    _ -> trace (show name) id
  where
    hdr l = para [Paragraph] . chrs (Bold:headersize l)
    headersize = sizechange.(3-)
    fontsize Nothing = []
    fontsize (Just s)= sizechange (newsize s)
    sizechange n = if n<=0 then replicate (-n) Smaller else replicate n Bigger
    newsize ('"':s) = newsize s -- !!! quotes should be removed by parser
    newsize ('+':s) = size s
    newsize ('-':s) = -(size s)
    newsize s = size s-3
    size s =
      case reads s of
        (n,_):_ -> n -- allow trailing quote char
	[] -> 0

htmlCmd2rt t@(name,attrs) =
  case name of
    BR -> unitN [NewLine]
    P  -> unitN [NewLine,NewLine]
    IMG -> special (Img, attrs)
    LI -> if symbol
            then unitN $ chrs [Symbol] [NewLine,PlainChars "\xb7 "]
            else unitN [NewLine,PlainChars "* "]
    DD -> unitN [NewLine,PlainChars "     "]
    DT -> unitN [NewLine]
    HR -> unitN [NewLine,hline,NewLine]
    INPUT -> special (Inp,[])
    ISINDEX -> (\s->[NewLine,hline,NewLine,
                       PlainChars "This is a searchable index.",
		       PlainChars "Enter search keywords.",
		       s,
		       NewLine,hline,NewLine]) `mapN` (special1 (Inp,[]))
    BASE ->  unitN []
    NEXTID -> unitN []
    LINK -> unitN []
    META -> unitN []
    _ -> rtGarb (show name,attrs)

rtGarb ('!':_,_) = unitN []
rtGarb (name,_) = if htmldebug
                  then unitN $ chrs [Sans,PenColor "red"] [PlainChars "<?",PlainChars name,PlainChars ">"]
		  else unitN []

chrs cfs rt = map (FmtChar On) cfs++rt++map (FmtChar Off) cfs
para pfs rt = NewLine:foldr (\pf rt->[FmtPara pf rt]) rt pfs++[NewLine]

hline = PlainChars "______________"

brklines = mapHtmlChars brl
  where
    brl "" = []
    brl ('\n':s) = br:brl s
    brl s = case break (=='\n') s of
              (s1,s2) -> HtmlChars (expandtabs 8 s1):brl s2

specials = concatMap sp
  where
    sp rt =
      case rt of
        Special x -> [x]
	FmtPara _ rts -> specials rts
	_ -> []

sans = if argFlag "sans" False
       then [Sans]
       else []

symbol = argFlag "symbol" True

htmldebug = argFlag "htmldebug" False

linkColor = argKey "linkcolor" "mediumblue"
