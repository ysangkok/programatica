module HtmlF(htmlF,Anchor(..)) where
import Fudgets
import RichTextF
import Html2RichText
import Html
import HtmlTags
import Http(HttpMethod(..))
import HtmlFormF
import ImagesF
import FuppletsF
import RichText
import URL(URL)
import ListUtil(assoc)

htmlF :: Maybe Size -> F (URL,Html) (InputMsg (String,HttpMethod))
htmlF optsize =
  loopThroughRightF
    (absF (ctrlSP []))
    (richTextFormF optsize (htmlFormF>+<(optImagesF>+<optFuppletsF)))

toRichText = Left . Left  
toForms = Left . Right . Left
toImages = Left . Right . Right . Left
toFupplets = Left . Right . Right . Right

ctrlSP sp =
    getSP $ either fromRichTextFormF fromOutside
  where
    fromOutside (url,html0) =
      let html = (optRmFupplets . optRmImages) html0
          rt = html2RichText html
          sp = map fst $ specials rt
      in createForms html $ \ sizes ->
         optCreateImages url html $ \ isizes ->
	 optCreateFupplets url html $ \ fsizes ->
         putSP (toRichText (rt, mergeSizes sp sizes isizes fsizes)) $
	 ctrlSP sp

    fromRichTextFormF msg =
      case msg of
        Right (Left (SubmittedForm req)) ->
          putSP (Right req) $
	  ctrlSP sp
        Left (Right selectedLink) ->
          putSP (Right (mapInp (\x->(x,HttpGet)) selectedLink)) $
	  ctrlSP sp
        Left (Left places) ->
            putsSP [toForms (FormPlaces foplaces),
                    toImages (Right iplaces),
		    toFupplets (Right fuplaces)] $
            ctrlSP sp
	  where
	    splaces = zip sp places
	    iplaces = [ p | ((Img,_),p) <- splaces ]
	    foplaces = [ p | ((Inp,_),p) <- splaces ]
	    fuplaces = [ p | ((Fupplet,_),p) <- splaces ]

createForms html =
    cmdContSP (Left (Right (Left (NewForms html)))) ans
  where
    ans (Left (Right (Left (FormSizes sizes)))) = Just sizes
    ans _ = Nothing

optImagesF =
  if images
  then imagesF
  else nullF

optCreateImages =
  if images
  then createImages
  else \ _ _ cont -> cont []

createImages url html =
    cmdContSP ((toImages . Left) (url, html)) ans
  where
    ans (Left (Right (Right (Left sizes)))) = Just sizes
    ans _ = Nothing

optFuppletsF =
  if fupplets
  then fuppletsF
  else nullF

optCreateFupplets =
  if fupplets
  then createFupplets
  else \ _ _ cont -> cont []

createFupplets url html =
    cmdContSP ((toFupplets . Left) (url, html)) ans
  where
    ans (Left (Right (Right (Right sizes)))) = Just sizes
    ans _ = Nothing

mergeSizes [] _ _ _ = []
mergeSizes ((Inp,a):xs) (inp:inps) imgs fups = (a,inp):mergeSizes xs inps imgs fups
mergeSizes ((Img,a):xs) inps (img:imgs) fups = (a,img):mergeSizes xs inps imgs fups
mergeSizes ((Fupplet,a):xs) inps imgs (fup:fups) = (a,fup):mergeSizes xs inps imgs fups

optRmImages =
  if images
  then id
  else rmImages

rmImages = concatMap rmImage
  where
    rmImage html =
      case html of
        HtmlCommand (IMG,attrs) ->
	    [HtmlContext (SANS,[]) [HtmlChars s]]
	  where s = assoc id "[IMG]" attrs "ALT"
	HtmlContext tag html -> [HtmlContext tag (rmImages html)]
	_ -> [html]

optRmFupplets =
  if fupplets
  then id
  else rmFupplets

rmFupplets = concatMap rmFupplet
  where
    rmFupplet html =
      case html of
        HtmlContext (FUPPLET,attrs) html ->
	    [HtmlContext (SANS,[]) html]
	HtmlContext tag html -> [HtmlContext tag (rmFupplets html)]
	_ -> [html]

images   = argFlag "images"   True
fupplets = argFlag "fupplets" False
