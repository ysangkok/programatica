module Html2Drawing(
	drawHtmlDoc,HtmlDrawing(..),HtmlLabel(..),
	HtmlInput(..),HtmlOutput(..),
	FormInput(..),FormOutput(..)) where
import Fudgets hiding (defaultFont,FontSpec)
import Html
import HtmlTags
import HtmlOps(htmlchars,txt,href,br,mapHtmlChars,extractBase)
import Fud(paragraphP'',horizontalAlignP,horizontalAlignP',overlayAlignP,moveRefsS,refEdgesS,noRefsS)
import FontSpec
import Data.ListUtil(assoc,chopList,breakAt)
import Data.ListMap(lookupWithDefault)
import Data.List(groupBy)
import HO(apSnd)
import Data.Char(isHexDigit)
import Data.Maybe(isJust,maybeToList,fromMaybe)
import Utils2(strToUpper,(+++),isSpace')
import ActiveGraphicsF
import HtmlFormF2
import HtmlIsIndexForm
import URL(URL,joinURL)
import ParseURL(parseURL)
import ImageF
import FuppletF
import PixmapDisplayF() -- Show instance for PixmapId, for debugging

default(Int)

type HtmlDrawing = ActiveDrawing HtmlLabel Gfx HtmlInput HtmlOutput

data HtmlOutput
  = FromForm FormOutput
  | FromImage ImageOutput
  | FromFupplet FuppletOutput
  deriving (Show)

data HtmlInput
  = ToForm FormInput
  | ToImage ImageInput
  | ToFupplet FuppletInput
--  deriving (Show)

data HtmlLabel
  = LinkTo URL
  | LinkTarget String
  | Form TagAttrs
  | IsMap Size URL
  deriving (Eq,Show) -- Show instace only for debugging

data Ctx
   = C { font :: FontSpec,
	 underline :: Bool,
	 strike :: Bool,
	 align :: Alignment,
	 linkColor :: ColorSpec,
	 linkTo :: Maybe URL,
	 base :: URL
	}

ctx0 = C defaultFont False False aLeft defaultLinkColorSpec Nothing

changeFont f c@(C{font=font}) = c{font=f font}

drawHtmlDoc :: URL -> Html -> HtmlDrawing
drawHtmlDoc baseURL html = boxD (concatMap (drawHtml baseURL) html)

drawHtml :: URL -> HtmlItem -> [HtmlDrawing]
drawHtml base html =
  case html of
    HtmlContext (BODY,attrs) html -> drawBody base attrs html
    HtmlContext (HTML,_) html -> concatMap (drawHtml base) html
    HtmlContext (NOFRAMES,_) html -> concatMap (drawHtml base) html
    HtmlContext (FRAMESET,_) html -> drawFrameset base html
    _ -> ignore "drawHtml" html $ []

ignore s html = ctrace "ignore" (s,html)

drawFrameset base html =
    drawTextBlock (ctx0 base) $
    txt "Frames: ": extractFrames html
 where
   extractFrames = concatMap extr
   extr item =
     case item of
	HtmlContext (FRAMESET,_) html -> extractFrames html
	HtmlCommand (FRAME,attrs) -> maybeToList (drawFrame attrs)
	_ -> []

   drawFrame attrs =
     do src <- optsrc
        link <- optname+++optsrc
        return (href src [txt link])
     where
       optsrc  = lookup "SRC" attrs
       optname = lookup "NAME" attrs

--drawBody :: URL -> TagAttrs -> Html -> [HtmlDrawing]
drawBody base attrs html =
    [softAttribD [GCForeground textColor,GCFont fnt] $
     drawBlocks' ctx html]
  where
    fnt = fontSpec.fontname.font $ ctx
    ctx = (ctx0 base') { linkColor = linkColor }
    textColor = colorSpec $ maybe id (:) (lookup "TEXT" attrs) [fgColor]
    linkColor = colorSpec $ maybe id (:) (lookup "LINK" attrs) defaultLinkColor
    -- vlinkColor = colorSpec $ lookupWithDefault attrs "black" "LINK"
    base' = fromMaybe base $ lookup "BASE" attrs >>= parseURL

drawBlocks :: Ctx -> Html -> [HtmlDrawing]
drawBlocks ctx html = [drawBlocks' ctx html]
drawBlocks' ctx = placedD' (verticalP' 12) . concatMap (drawBlock ctx) 

drawBlock :: Ctx -> HtmlItem -> [HtmlDrawing]
drawBlock ctx item =
  case item of
    HtmlContext t html -> drawBlockContext ctx t html
    HtmlCommand cmd -> drawBlockCommand ctx cmd
    HtmlChars s -> drawBlockContext ctx (P,[]) [item]
    _ -> ignore "drawBlock" item [] -- !!!

drawBlockCommand ctx (tag,attrs) =
  case tag of
    HR -> [hrD]
    ISINDEX -> drawBlock ctx (isIndexHtml attrs)
    _ -> ignore "drawBlockCommand" tag []
  where hrD = plD (hFiller 2)

drawBlockContext :: Ctx -> HtmlTag -> Html -> [HtmlDrawing]
drawBlockContext ctx (tag,attrs) [] = ctrace "empty" (tag,attrs) []
drawBlockContext ctx (tag,attrs) html =
    case tag of
      UL -> drawList attrs ctx drawUlItem html
      OL ->  drawList attrs ctx drawOlItem (number 1 html)
      DIR -> drawList'' (tableP' 2 Horizontal 1) ctx drawDirItem html
      MENU -> drawList' 1 ctx drawMenuItem html
      DL -> drawList attrs ctx drawDlItem html
      BLOCKQUOTE ->[marginD (pP 30 12) (pP 30 12) (boxD (drawBlocks ctx html))]
      DIV -> drawBlocks (getAlignAttr attrs ctx) html
      CENTER -> drawBlocks (ctx{align=aCenter}) html
      TABLE -> drawTable (getAlignAttr attrs ctx) attrs html
      FORM -> [labelD (Form attrs) $ boxD $ drawBlocks ctx html]
      PRE -> drawPreTextBlock ctx html
      ADDRESS -> drawTextBlock ctx html -- !!!
      P  -> drawTextBlock (getAlignAttr attrs ctx) html
      H1 -> dh 1
      H2 -> dh 2
      H3 -> dh 3
      H4 -> dh 4
      H5 -> dh 5
      H6 -> dh 6
      BODY -> drawBlocks ctx html -- A hack to tolerate bad html from ToHtml.
      _ -> drawTextBlock ctx [HtmlContext (tag,attrs) html] -- !!
  where dh = drawHeading (getAlignAttr attrs ctx) attrs html

drawHeading ctx attrs html n =
    [marginD (pP 0 5) 0 $ changeFontD ctx' $ boxD $ drawTextBlock ctx' html]
  where
     ctx' = changeFont (setWeight Bold . adjsize (4-n)) ctx
     h = pixelsize (fontsize (font ctx))

drawList attrs = drawList' sep
  where sep = if "COMPACT" `elem` map fst attrs then 1 else 5

drawList' vsep = drawList'' (verticalP' vsep)

drawList'' placer ctx drawItem list =
  [marginD (pP 20 0) 0 $ placedD' placer (concatMap (drawItem ctx) list)]

drawUlItem ctx = drawLiItem ctx (changeFontD ctx' (plD "·"):)
  where ctx' = changeFont (setCharset AdobeSymbol) ctx

drawOlItem ctx (i,item) = drawLiItem ctx (plD (show i++"."):)  item
drawDirItem ctx = drawLiItem ctx ((:[]) . marginD (pP 10 0) 0 . boxD)
drawMenuItem ctx = drawLiItem ctx id

drawLiItem ctx bullet item =
  case item of
   HtmlContext (LI,_) html -> [hboxaD (bullet (drawBlocks ctx html))]
   _ -> ignore "drawLiItem" item []

drawDlItem ctx item =
  case item of
   HtmlContext (DT,_) html -> drawBlocks ctx html -- !! drawTextBlock
   HtmlContext (DD,_) html -> [marginD (pP 20 0) (pP 0 12) $ boxD $
			       drawBlocks ctx html]
   _ -> ignore "drawDlItem" item []


--drawTable ctx attrs html = undefined {-
drawTable ctx attrs html =
    [placedD' (hAlignS a `spacerP` verticalP' 1) (
	drawCaps tcaptions++
	[frameD bgcolor bw space $
         tableD' space colcnt (map drawCell (concatMap padrow rows))]++
	drawCaps bcaptions
	)]
  where
    rows      = [(cells row{-,attrs-}) | HtmlContext (TR,attrs) row<-html]
    colcnt    = maximum (map (length {-. fst-}) rows)
    cells row = [(cell,(t,as)) | HtmlContext (t,as) cell<-row, isCell t]
    isCell    = (`elem` [TH,TD])
    padrow cells = take colcnt (map Just cells++repeat Nothing)
    drawCaps = map (drawTextBlock' ctx' . fst)
      where ctx' = ctx { align=aCenter }
    (bcaptions,tcaptions) = part hasAlignBottom captions
      where
	captions  = [(cap,attrs) | HtmlContext (CAPTION,attrs) cap<-html]
	hasAlignBottom (_,attrs) =
	  maybe False ((=="BOTTOM").strToUpper) (lookup "ALIGN" attrs)

    a = align ctx
    bw = readAttr' attrs "BORDER" 0 1
    pad =  readAttr attrs "CELLPADDING" 2
    space = readAttr attrs "CELLSPACING" 2
    bgcolor = getBgColor attrs
    getBgColor attrs = if ignoreColors then Nothing else lookup "BGCOLOR" attrs

    drawCell Nothing = pblankD 5
    drawCell (Just (cell,(t,attrs))) =
	frameD (getBgColor attrs) bw pad $
	case t of
	  TH -> changeFontD ctx' (drawContents ctx' cell)
	    where ctx' = changeFont (setWeight Bold) ctx
	  _  -> drawContents ctx cell
      where
	drawContents ctx cell = spacedD (vAlignS va) $ drawBlocks' ctx' cell
	  where ctx' = getAlignAttr attrs (ctx{align=aLeft})
		va = fromMaybe aCenter $
		     lookup "VALIGN" attrs >>= parseVAlign
--}

drawPreTextBlock ctx html =
    [changeFontD ctx' $ drawTextBlock'' lineD ctx' (breakLines html)]
  where
    ctx' = changeFont (setSpacing Fixed) ctx
    breakLines = mapHtmlChars brk
    brk s = case span (/='\n') s of
	      ("","")   -> []
	      (s1,"")   -> [HtmlChars s1]
	      ("",_:s)  -> br:brk s
	      (s1,_:s2) -> HtmlChars s1:br:brk s2

drawTextBlock ctx html = [drawTextBlock' ctx html]
drawTextBlock' ctx html = drawTextBlock'' paraD ctx (breakWords html)
  where
    breakWords = mapHtmlChars (map txt . words')
    -- Prelude.words does not treat nonbreaking space right, so:
    words' = filter (not . null) . -- trailing spaces => last word == ""
	     chopList takeWord'
    takeWord' = break isSpace' . dropWhile isSpace'

drawTextBlock'' placer ctx html =
    placedD' (verticalP' 1) $ map (placer a . drawText ctx) html'
  where a = align ctx
	html'' = floatBR html
	html' = --ctrace "floatBR" (html,html'')
	        html''

drawText ctx = concatMap drawTextItem
  where
    drawTextItem item =
      case item of
	HtmlContext t html -> drawTextContext ctx t html
	HtmlCommand cmd -> drawTextCommand ctx cmd
	HtmlChars s -> drawChars ctx s
	HtmlGarbage ('!':s,_) -> [] -- comment
	HtmlGarbage (t,_) ->
	  [softAttribD (gcFgA (colorSpec "red")) (plD ("<"++t++">"))]

drawChars ctx s = [drawChars' ctx s]

drawChars' :: Ctx -> String -> HtmlDrawing
drawChars' ctx s = linkLblD ctx (optul (plD s))
  where
    optul = if underline ctx then ul else id
    ul s = stackD [spacedD bottomS (plD (hFiller 1)),s]
--    ul s = placedD' overlayAlignP [lineD, s] -- outputs the wrong ref points?
--    lineD = spacedD (moveRefsS (pP 0 2) `compS` refMiddleS `compS` topS)
--		    (plD (hFiller 1))

linkLblD = linkLblD' LinkTo

linkLblD' f ctx d =
  case linkTo ctx of
    Just link -> labelD (f (joinURL (base ctx) link)) d
    _ -> d

drawTextCommand ctx (tag,attrs) =
  case tag of
--    BR -> [pblankD (pP 600 1)] -- A quick hack!!
    IMG -> [optImageD ctx attrs]
--    BASEFONT ->
    INPUT -> [formD (formInputF attrs)]
    _ -> ignore "drawTextCommand" tag []

drawTextContext ctx (tag,attrs) html =
  (if null html then ctrace "empty" (tag,attrs) else id) $
  case tag of
    EM -> italic
    STRONG -> bold
    DFN -> italic
    CODE -> fixed
    SAMP -> fixed
    KBD -> fixed
    VAR -> italic
    CITE -> italic

    TT -> fixed
    SANS -> sans
    I -> italic
    B -> bold
    U -> drawText (ctx{underline=True}) html
    STRIKE -> strike
    S -> strike
    DEL -> strike
    INS -> sans
    BIG -> changefont (adjsize 1)
    SMALL -> changefont (adjsize (-1))
--    SUB ->
--    SUP ->
    Q -> italic
    ABBR -> sans -- !!
    ACRONYM -> sans -- !!

    A -> --ctrace "A" attrs $
         [tD $ aD $ boxD $ drawText ctx' html]
	where tD = assoc (labelD . LinkTarget) id attrs "NAME"
	      (aD,ctx') =
		case lookup "HREF" attrs >>= parseURL of
	          link@(Just _) -> (softAttribD (gcFgA (linkColor ctx)),
				    ctx { underline=True, linkTo=link })
		  _ -> (id,ctx)

    FUPPLET -> [fuppletD ctx attrs html]
--    APPLET ->
--    OBJECT ->
--    IFRAME ->
    FONT -> [changeColor $ changefont' (fsize $ lookup "SIZE" attrs)]
	where
	  changeColor =
	    if ignoreColors
	    then id
	    else maybe id (fgD . fgD') (lookup "COLOR" attrs)
	  fgD' s = if length s==6 && all isHexDigit s
	           then [s,'#':s,"black"]
		   else [s,"black"] -- could use a context sensitive fallback
	  fsize Nothing  = id
	  fsize (Just s) = setsize (newsize s)
	  newsize ('"':s) = newsize s -- !!! quotes should be removed by parser
	  newsize ('+':s) = size (s0+) s
	  newsize ('-':s) = size (s0-) s
	  newsize s = size id s
	  s0 = fontsize defaultFont -- !! Should use BASEFONT setting!
	  size f s =
	    case reads s of
	      (n,_):_ -> f n -- allow trailing quote char
	      [] -> s0

--    MAP ->
    NOBR -> [hboxaD (drawText ctx html)]

    SELECT -> [formD (formSelectF attrs options)]
      where
	options =
	  [(htmlchars' opt,attrs) | HtmlContext (OPTION,attrs) opt<-html]
        htmlchars' = unwords . words . htmlchars .
	             mapHtmlChars ((:[]).txt.(' ':))

    TEXTAREA -> [formD (formTextAreaF attrs (htmlchars html))]
    SCRIPT -> [] -- Scripts should not be displayed...
    STYLE  -> [] -- Hide style sheet in bad HTML
    _ -> drawText ctx html
  where
    fixed = changefont (setSpacing Fixed)
    italic = changefont (setSlant Italic)
    bold = changefont (setWeight Bold)
    sans = changefont (setSerif SansSerif)
    strike = drawText (ctx{strike=True}) html
    changefont f = [changefont' f]
    changefont' f = changeFontD ctx' (boxD $ drawText ctx' html)
      where ctx' = changeFont f ctx

marginD ul lr = spacedD (hvMarginS ul lr)

frameD optBgColor bw pad d =
    stackD' (background++border++[contents])
  where
    contents = padD (bw+pad) d
    border = if bw==0 then [] else [softAttribD (bwA bw) $ plD frame]
    background =
	case optBgColor of
	  Nothing -> []
	  Just bgcolor -> [fgD [bgcolor,paperColor] $ plD filledRect]

paraD a = placedD' paraP
 where paraP = paragraphP'' (lineP a) (pP 5 0)

lineP a = spacerP (hAlignS a) . horizontalAlignP'
lineD a = placedD' (lineP a 5)

hboxaD = placedD' horizontalAlignP

--vboxlD'' sep [] = pblankD 5
--vboxlD'' sep ds = vboxlD' sep ds

padD = spacedD . marginS
placedD' p [] = pblankD 5
placedD' p ds = PlacedD p (boxD ds)

changeFontD = softAttribD . gcFontA . fontSpec . fontname . font

readAttr attrs name def = readAttr' attrs name def def

-- readAttr is not good for string attributes, since read then require quotes!

readAttr' attrs name def1 def2 = assoc f def1 attrs name
  where f s = case reads s of
		[(x,_)] -> x
		_ -> def2

getAlignAttr = getAlignAttr' "ALIGN" parseAlign
getVAlignAttr = getAlignAttr' "VALIGN" parseVAlign

getAlignAttr' name parse attrs ctx =
  case lookup name attrs >>= parse of
    Just a -> ctx{align=a}
    _ -> ctx

parseAlign s =
  case strToUpper s of
    "LEFT"   -> Just aLeft
    "RIGHT"  -> Just aRight
    "CENTER" -> Just aCenter
    _ -> Nothing

parseVAlign s =
  case strToUpper s of
    "TOP"    -> Just aTop
    "BOTTOM" -> Just aBottom
    "CENTER" -> Just aCenter
    _ -> Nothing

bwA bw = [GCLineWidth bw]

defaultLinkColorSpec = colorSpec defaultLinkColor

defaultLinkColor =
    argKeyList "linkcolor" defdef
  where defdef = words "mediumblue blue3 blue2 grey40 black"

plD x = passiveLeaf . G $ x
alD x = activeLeaf x

optImageD =
    if images
    then imageD
    else imageAltD
  where
    imageAltD ctx attrs = drawChars' ctx (lookupWithDefault attrs "[IMG]" "ALT")

imageD ctx attrs =
    if isJust (linkTo ctx)
    then linkLblD' lnk ctx $ stackD [borderD,imgD]
    else imgD
  where
    lnk = if "ISMAP" `elem` map fst attrs then IsMap border else LinkTo
    borderD = fgD (linkColor ctx) $ plD filledRect
    imgD= alD (FromImage>^=<imageF' border (base ctx) (src,attrs)>=^^<toImageSP)
    src = lookup "SRC" attrs
    border = diag (readAttr attrs "BORDER" 2)
    -- attrs VSPACE, HSPACE
    toImageSP = mapFilterSP f
    f (ToImage msg) = Just msg
    f _ = Nothing

fuppletD ctx attrs html =
    alD (FromFupplet>^=<fuppletF (base ctx) (src,attrs,html)>=^^<toFuppletSP)
  where  
    src = lookup "SRC" attrs
    toFuppletSP = mapFilterSP f
    f (ToFupplet msg) = Just msg
    f _ = Nothing

formD fud = padD 2 $ alD (FromForm>^=<adjRefsF fud>=^^<toFormSP)
  where toFormSP = mapFilterSP f
	f (ToForm msg) = Just msg
	f _ = Nothing
	--adjRefsF = spacer1F refEdgesS
	adjRefsF = spacer1F noRefsS

pblankD = passiveD . blankD

passiveD = mapLeafDrawing Right

stackD' [x] = x
stackD' ds = stackD ds

filledRect = filler False False 1

floatBR :: Html -> [Html]
floatBR = hlines . floatBR'
  where
    hlines = chopList (break' isBr)
    break' p = apSnd (drop 1) . break p

    isBr (HtmlCommand (BR,_)) = True
    isBr _ = False

    floatBR' :: Html -> Html
    floatBR' = concatMap floatBR1

    floatBR1 :: HtmlItem -> Html
    floatBR1 item =
      case item of
	HtmlContext t@(n,_) html | n `notElem` don't_break
		-> map ctx (groupBR $ floatBR' html)
	  where ctx [item] | isBr item = item
                ctx html = HtmlContext t html
	_ -> [item]

    groupBR = groupBy (\i1 i2 -> not (isBr i1 || isBr i2))

    don't_break = [SELECT,TEXTAREA]

--
ignoreColors = argFlag "ignorecolors" False
images = argFlag "images" True
