{-# LANGUAGE CPP #-}
module HtmlF2b(htmlF) where
#ifdef __GLASGOW_HASKELL__
import Fudgets hiding (StreamProcIO(..))
#else
import Fudgets hiding (StreamProcIO)
#endif
import Html2Drawing
import HtmlFormHandler
import HtmlFormSubmit
import HtmlFuns(extractBodyAttrs,extractBase)
import Http(HttpMethod(..))
import ActiveGraphicsF
--import ExternalImageFetchF(imageFetchF)
import ImageFetchF(imageFetchF)
import FuppletFetchF
import ImageF(ImageInput(..),ImageOutput(..)) -- because of -fno-syn-expand
import FuppletF(FuppletInput(..),FuppletOutput(..)) -- because of -fno-syn-expand
import URL(URL,url2str,joinURL,sameDoc,fragment,relativeURL)
import ParseURL(parseURL)
import Html(Html)
import Monads
import ConnectF
--import qualified HtmlF as Old
import Data.Maybe(fromMaybe,listToMaybe)

#ifdef __HASKELL98__
#define map fmap
#endif

--infixr !

--tr x y = ctrace x y y

reactiveF rM = mapstateF (\ s m ->react (rM m) s)

htmlF optsize =  htmlF2 optsize -- >*< (shellF "Old" $  Old.htmlF optsize)

data State = S { current :: HtmlDrawing,
                 currentURL :: URL,
		 optcursor :: Maybe DPath }

htmlF2 :: Maybe Size -> F (URL,Html) (InputMsg (URL,HttpMethod))
htmlF2 optsize = vScrollF dispF
  where
    dispF = loopThroughRightF
		(reactiveF ctrlM state0)
		combF
      where
        ctrlM = either fromLoop fromOutside
	state0 = S adrawing0 undefined Nothing

    putToDisp :&: putToImageFetchF :&: putToFuppletFetchF :&: putToFormHandlerF =
      extend (put . Left) tags

    TagF combF fromLoop tags =
        disp >&< imageFetch >&< fuppletFetch >&< formHandler
      where
        disp = tagF fromDisp $ activeGraphicsF' (setBorderWidth 0) adrawing0
	imageFetch = tagF fromImageFetchF imageFetchF
	fuppletFetch = tagF fromFuppletFetchF fuppletFetchF
	formHandler = tagF fromFormHandlerF $ absF formHandlerSP

        fromDisp = either gfxEvent fromInset

--    adrawing0 = passiveLeaf (G (blankD size))
    adrawing0 = mapLeafDrawing Right (blankD size)
    size = fromMaybe 500 optsize


    putToDispGfx = putToDisp . Left
    putToInsets = putToDisp . Right
    putToOutside = put . Right

    newdoc new url = update $ \ s -> s { current=new,currentURL=url }
    newcursor new = update $ \ s -> s { optcursor=new }

    fromOutside (url,html) =
     do optfetchbgpic
	putToFormHandlerF Nothing -- reset form handler state
	-- putToImageFetchF flush -- flush outstanding requests
	putToDispGfx (ChangeGfxBg bgcolor)
	putToDispGfx (replaceAllGfx next)
	putToDispGfx (showGfx target)
	newdoc next url
      where
	next = spacedD (marginHVAlignS 5 aLeft aTop) $
	       drawHtmlDoc baseURL html

	target = fragmentPath next url

	bgcolor = colorSpec $
		  maybe id (:) (lookup "BGCOLOR" bodyattrs) [paperColor]

	optfetchbgpic =
	    fromMaybe nop $
	    lookup "BACKGROUND" bodyattrs >>= parseURL >>= fetchit
	  where
	    fetchit url = return (putToImageFetchF msg)
	      where absurl = joinURL baseURL url
		    msg = (Nothing,(absurl,Nothing))

	bodyattrs = extractBodyAttrs html
	baseURL = fromMaybe url (extractBase html >>= parseURL)

    fragmentPath drawing url =
	--tr "target" $
	fromMaybe [] (fragment url >>= path)
      where
	path fr = lookup (LinkTarget fr)
			 (--tr "annots" $
			  map swap $ drawingAnnots drawing)

    gfxEvent e =
      case e of
	GfxMotionEvent { gfxState=gfxState } ->
	 do (if Button1 `elem` gfxState
	     then changeCursor e
	     else nop)
	    select False e
	GfxButtonEvent {gfxType=gfxType,gfxButton=Button 1} ->
	    case gfxType of
	      Pressed -> changeCursor e
	      Released -> do removeCursor ; select True e
	      _ -> nop
	_ -> nop

    select isDone e =
      do S { current=current } <- get
	 fromMaybe nop $
	  do (path,(p,_)) <- last' (gfxPaths e)
	     LabelD lbl _ <- enclosingLink current path
			   -- ^^ should search for LinkTo/IsMap labels!!
	     url <- case lbl of
		      LinkTo url -> return url
		      IsMap offset url -> return (addMapPos url (p-offset))
		      _ -> Nothing --zero
	     return (goto url)
      where
	goto url =
	  do S { currentURL=currentURL,current=current } <- get
	     if isDone && sameDoc url currentURL
	      then putToDispGfx (showGfx (fragmentPath current url))
	      else putToOutside (f ({-url2str-} url,HttpGet))
		-- url2str: for compatibility with Old.htmlF -- (now dropped)
	f = if isDone then inputMsg else inputChange

    removeCursor = map optcursor get >>= maybe nop removeIt
      where removeIt path =
	      do putToDispGfx (gfxSetCursor path False)
		 newcursor Nothing

    changeCursor e =
     do cur <- map optcursor get
	if new==cur then nop -- avoid flicker
	 else do removeCursor
		 newcursor new
		 addNewCursor new
      where new = map fst (last' (gfxPaths e))

	    addNewCursor = maybe nop addIt
	      where addIt path = putToDispGfx (gfxSetCursor path True)

    fromImageFetchF (dst,msg) =
      case dst of
	Just n -> putToInsets (n,ToImage msg)
	Nothing -> putToDispGfx (ChangeGfxBgPixmap pm False)
	  where (_,(_,pm)) = msg

    fromFuppletFetchF (dst,msg) = putToInsets (dst,ToFupplet msg)

    fromInset (n,dpath,msg) =
      case msg of
	FromImage imsg -> putToImageFetchF (Just n,imsg)
	FromFupplet imsg -> putToFuppletFetchF (n,imsg)

	FromForm fmsg ->
	  do S { current=current } <- get
	     let formpath = drawingAnnotPart' isForm current dpath
	     putToFormHandlerF (Just (formpath,(n,fmsg)))

	_ -> nop -- ignored for the moment!

    fromFormHandlerF = either toForm submit
      where
	toForm (n,msg) = putToInsets (n,ToForm msg)
	submit (formpath,values) =
	    do S { current=current } <- get
	       let msg = submitForm (attrs,values)
		   attrs = enclosingFormAttrs current formpath
	       putToOutside (inputMsg msg)

enclosingLabel = enclosingLabel' (const True)

enclosingLabel' p drawing path =
  maybeDrawingPart drawing (drawingAnnotPart' p drawing path)

enclosingFormAttrs drawing path =
  case enclosingLabel' isForm drawing path of
    Just (LabelD (Form attrs) _) -> attrs
    _ -> []

isForm (Form _) = True
isForm _ = False

enclosingLink = enclosingLabel' isLink
  where
    isLink (LinkTo _) = True
    isLink (IsMap _ _) = True
    isLink _ = False

gfxSetCursor path on = ChangeGfx [(path,(on,Nothing))]

last' [] = Nothing
last' xs = Just (last xs)

showGfx target = ShowGfx target (Nothing,Just aTop)

addMapPos url (Point x y) =
  joinURL url (relativeURL ("?"++show x++","++show y))
