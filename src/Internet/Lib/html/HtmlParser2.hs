module HtmlParser2 (parseHtml,parseHtmlText,parseHtmlBlocks) where
import Html
import HtmlTags
import HtmlLex
import ParsOps3
import HtmlParsOps
import Utils2(isSpace')

parseHtml :: String -> Either ([String],String) Html
parseHtml = parse' htmlDocument

parseHtmlText :: String -> Either ([String],String) Html
parseHtmlText = parse' optText

parseHtmlBlocks :: String -> Either ([String],String) Html
parseHtmlBlocks = parse' blocks

parse' part s =
   case parseToEof part (htmlLex s) of
     Right html -> Right html
     Left (ts, es) -> Left (map sh1 es, showHtmlLex ts)

htmlDocument =
    two `mapP` doctype `ap` htmlDoc `chk` whitespace
--    one `mapP` htmlDoc
  where
    doctype = space `cap` optional defaultDoctype (comment `chk` whitespace)
      where defaultDoctype = HtmlGarbage (doctypehtml40,[])
    htmlDoc = impliedCtx HTML headAndBody

doctypehtml40 =
  "!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\""

headAndBody =
--  one `mapP` headPart
  (:) `mapP` headPart `chk` whitespace `ap` framesetAndBody

headPart = whitespace `cap` impliedCtx HEAD (many headElement)
  where
    headElement =
		 title 
	`orelse` headCmd
	`orelse` script
	`orelse` style
	`orelse` badHeaderStuff
      where
	title = ctx1 TITLE plaintext
	headCmd = cmd [ISINDEX, LINK, BASE,META] --"NEXTID"
	script =  anythingButCtx SCRIPT
	style = anythingButCtx STYLE
        badHeaderStuff = whitespace1

framesetAndBody = (++) `mapP` optFramesetPart `ap` noFramesPart

optFramesetPart = optional [] (one `mapP` frameset)
  where
    frameset = ctx1' FRAMESET frames
    frames = whitespace `cap` some frame
    frame = (cmd1 FRAME `orelse` frameset) `chk` whitespace

noFramesPart = whitespace `cap` (one `mapP` (noframes `orelse` bodyPart))
  where
    noframes = ctx1' NOFRAMES (one `mapP` bodyPart)

bodyPart = whitespace `cap` impliedCtx BODY blocks `chk` whitespace

blocks = trimctx `mapP` many block

block = {-bclean `mapP`-} block'

block' =
	     ctx [UL,OL,DIR,MENU] list
    `orelse` ctx1 DL dlist
    `orelse` ctx' [H1 .. H6] optText
    `orelse` ctx [BLOCKQUOTE,DIV,CENTER] blocks
    `orelse` ctx1 TABLE table
    `orelse` ctx1 FORM blocks -- form
    `orelse` ctx1 PRE optPreText
    `orelse` ctx1' ADDRESS optText -- <P> is allowed too!
    `orelse` cmd1 HR
    `orelse` cmd1 ISINDEX -- only allowed in head according to standard
    `orelse` ctx1' P optText
    `orelse` (p0 . trim) `mapP` text
  where
    list = trimctx `mapP` many listElement
      where listElement =
			  ctx1' LI blocks
		`orelse` badCtx LI block -- allows white space

    dlist = trimctx `mapP` many dlistElement
      where dlistElement =
	                 ctx1' DT blocks -- std: only text allowed 
		`orelse` ctx1' DD blocks
		`orelse` badCtx DD block -- allows white space

    table = whitespace `cap` many (tableElement `chk` whitespace)
      where tableElement =
			 ctx1 CAPTION optText
		`orelse` ctx1' TR row
	        `orelse` badCtx' TR row'
                `orelse` formElement
            row = whitespace `cap` many (cell `chk` whitespace)
            row' = some cell
	    cell = ctx' [TH,TD] blocks
		   --`orelse` badCtx TD block -- allows white space

--    form = many formElement

optText = optional [] text
optPreText = optional [] preText
--text = text' chars
preText = text --text' prechars

--text' chars = txt
text = txt
  where
    txt = trimctx `mapP` some textElement
    txt0 = optional [] txt
    -- Note: no direct or indirect references to text or preText below!

    textElement =
	 plain `orelse` special `orelse` chars `orelse` comment

    plain = ctx' textLevelTags txt0

    special =
		 cmd [BR,IMG,BASEFONT]
	`orelse` ctx [APPLET,FUPPLET,OBJECT] applet
	`orelse` ctx1 IFRAME txt0
	`orelse` ctx1 MAP map
	`orelse` formElement
        `orelse` anythingButCtx SCRIPT
	`orelse` anythingButCtx STYLE
      where
	applet = many appletElement
	  where appletElement = cmd1 PARAM `orelse` textElement
	map = many mapElement
	  where mapElement = cmd1 AREA `orelse` whitespace1

formElement =
             cmd1 INPUT
    `orelse` ctx1 SELECT slist
    `orelse` ctx1 TEXTAREA plaintext
    -- Form elements are only allowed inside forms, but can
    -- occur in nested elements, so using a separate form
    -- parser (like for tables) is no good.
  where
    slist = many slistElement
      where slistElement = ctx1' OPTION plaintext
---}
plaintext = many chars

-- Text level markup:
logicalTags = [EM,STRONG,DFN,CODE,SAMP,KBD,VAR,CITE,Q,SPAN,ABBR,ACRONYM,DEL,INS]
physicalTags = [TT,I,B,U,STRIKE,BIG,SMALL,SUB,SUP]
-- All text level tags for which we allow the end tag to be missing:
textLevelTags = A:FONT:NOBR:logicalTags++physicalTags

one x = [x]
two x y = [x,y]

--impliedCtx' t p = ctx [t] p

p0 = HtmlContext (P,[("implicit","")])

-- Used for handling bad html:
badCtx t = mapP (HtmlContext (t,[]) . trimctx . one)
badCtx' t = mapP (HtmlContext (t,[]) . trimctx)

bclean item =
  case item of
    HtmlContext t@(n,_) html | n/=PRE -> HtmlContext t (trim html)
    _ -> item

trim [] = []
trim (HtmlChars s:html) =
  case dropWhile isSpace' s of
    "" -> trim html
    s -> HtmlChars s:trim html
trim (HtmlContext t []:html) | okToTrim t = trim html
trim html =trimctx html

trimctx [] = []
trimctx (HtmlContext ta []:html) | okToTrim ta = trimctx html
trimctx (item:html) = item:trimctx html

okToTrim t = not (isTarget t || keep t)
  where
    isTarget (t,attrs) = t==A && "NAME" `elem` map fst attrs
    keep (t,_) = t `elem` [TEXTAREA,TD,TH]
