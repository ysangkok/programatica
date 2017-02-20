module ImageFetchF(imageFetchF,bitmapFetchF,ppm2PixF,PixmapId) where
import AllFudgets
import URLFetchF
import Http
import URL(URL(..),url2str)
import ParFetchF
import PNM2PixmapK
import Dither
import PNMparser
import PNM
import GIFparser
import GIFdecompress
import GIF2PPM
import GIF()
import CacheF
import ImageFetchFlags
import DialogueIO
import Prelude hiding (IOError)
import Time(CalendarTime(..))

imageFetchF = cacheF (imageFetchF' False)
bitmapFetchF = imageFetchF' True

imageFetchF' toBitmap =
    filterRightSP>^^=<
    idRightF logF>==<
    respToPixF' toBitmap >==<
    (shmsg>^=<parFetchF parallel urlFetchF')
   where
     shmsg (Left (n,s)) = Left (unwords [show n,s])
     shmsg (Right x) = Right x

urlFetchF' = loopThroughRightF (mapstateF addSize Nothing) urlFetchF
  where
    req url = HttpReq url HttpGet acceptHdrs
    addSize optSize = either response request
      where
	request (url,optSize) = (optSize,[Left (req url)])
	response = either status result
	status s = (optSize,[Right (Left s)])
	result (url,r) = (optSize,[Right (Right ((url,optSize),r))])

acceptHdrs = [("Accept","image/"++itype) | itype<-imagetypes]
imagetypes = ["ppm","pgm","pbm","pnm","png","gif","jpeg","x-bitmap"]

logF =  if imglog
        then if imglogstderr
	     then stderrF >=^< (++"\n")
	     else shellF "Image fetch log" $ terminalF defaultFont 24 80
	else nullF

respToPixF' toBitmap =
    if toBitmap
    then ltF bmr2bmF >==< ltF (toXbmF True)>==< ltF (concatMapF extr)
    else defaultVisual $ \ visual ->
	 if color && visualClass visual `elem` [PseudoColor,TrueColor]
	 then idLeftF ppm2PixF>==<
	      ltF (concatMapF parse)>==<
	      ltF toPpmF>==<
	      ltF (concatMapF extr)
	 else idLeftF bm2pmF>==<
	      ltF (toXbmF False)>==<
	      ltF (concatMapF extr)
  where
    bmr2bmF = mapF bmr2bm
      where
        bmr2bm (url,BitmapReturn size _ bm) = Right (url,(size,bm))
	bmr2bm _ = Left "failed to load bitmap"

    extr (url,ans) = extr' ans
      where
        extr' (Left msg) = [Left ("Error: "++msg),out Nothing]
	extr' (Right httpResp) = [out (Just (respBody httpResp))]
	out optresp = Right (url,optresp)
    parse (url,Nothing) = [Right (url,Nothing)]
    parse (url,Just s) =
      case parseImg s of
        Right img -> [Right (url,Just img)]
	Left msg -> [Left ("Parse: "++msg),Right (url,Nothing)]

oColor = if colorCube == 0 then ($Nothing)
	 else allocColorCube colorCube . (.Just)

ppm2PixF = oColor $ \otbl -> ioF $ k0 otbl
   where
    errorPpm =PNM (Point 10 10) (PPM 4 (take (10*10) (RGB 4 0 0:repeat (RGB 3 3 3))))
    k0 otbl = pnm2PixmapK otbl errorPpm $ \ errpm ->
      let k = getK $ \msg ->
            case msg of
	      High (url,Just r)  -> pnm2PixmapK otbl r $ \pm-> 
				    putsK [High (url,pm)] k
	      High (url,Nothing) -> putsK [High (url,errpm)] k
	      _ -> k
      in k

bm2pmF = ioF bm2pmK0
  where
    bm2pmK0 =
      allocNamedColorPixel defaultColormap fgColor $ \ fg ->
      allocNamedColorPixel defaultColormap paperColor $ \ bg ->
      let gcattrs = [GCForeground fg,GCBackground bg,GCGraphicsExposures False]
          bm2pmK =
	    getK $ \ bmr ->
	    case bmr of
	      High (url,BitmapBad) -> error "bad bitmap in ImageFetchF.bm2pmF" -- !!! handle
	      High (url,BitmapReturn size _ bm) ->
		createPixmap size copyFromParent $ \ pm ->
		pmCreateGC pm rootGC gcattrs $ \ gc ->
		putLow (pmCopyPlane pm gc (Pixmap bm) (Rect origin size) origin 0) $
		xcommand (FreeGC gc) $
		xcommand (FreePixmap bm) $
		putHigh (url,(size,pm)) $
		bm2pmK
	      _ -> bm2pmK
      in bm2pmK

toXbmF threshold = ioF toXbmK
  where
    toXbmK =
      getK $ \ msg ->
      case msg of
        High d@(url,Nothing) -> putsK [High (Right (url,BitmapBad))] toXbmK
        High d@(url,Just s) ->
	  case imgFormat s of
	   PnmFmt -> convK ["ppm","pgm","pbm","xbm"] url s -- stupid !!!
	   PngFmt -> convK ["png","ppm","pgm",pbm,"xbm"] url s
	   GifFmt -> convK ["gif","ppm","pgm",pbm,"xbm"] url s
	   XbmFmt -> convK ["xbm"] url s
	   JpgFmt -> convK ["jpg","ppm","pgm",pbm,"xbm"] url s
	   _ -> putsK [High (Right (url,BitmapBad)),
	              High (Left (url2str (fst url)++": unknown type: "++take 23 s))] toXbmK
    pbm = if threshold then "pbmthr" else "pbm"

    convK fmts@(infmt:_) url s =
      storeImgK infmt s $ \ base infile ->
      doConvsK fmts base (Just infile) url

    doConvsK [_] base (Just infile) url =
      readBitmapFile infile $ \ bmr ->
      putsK [High (Left "Done with conversion"){-,Low Flush-}] $
      hIOSucc (DeleteFile infile) $
      putsK [High (Right (url,bmr))] $
      toXbmK
    doConvsK (infmt:fmts@(outfmt:_)) base (Just infile) url =
      convFileK infmt outfmt Nothing base infile $ \ _ outfile ->
      doConvsK fmts base outfile url
    doConvsK _ _ Nothing url =
      putK (High (Left "Conversion failed")) $
      putK (High (Right (url,BitmapBad))) $
      toXbmK

toPpmF = ioF toPpmK
  where
    toPpmK =
      getK $ \ msg ->
      case msg of
        High (url,Nothing) -> put (Right (url,Nothing)) toPpmK
        High (url@(_,optSize),Just s) ->
	  case imgFormat s of
	   PnmFmt -> put (Right (url,Just (PnmFmt,s))) toPpmK
	   GifFmt ->
	     case (optSize,sizeOfGIF s) of
	       (Just rsize,Right (w,h)) | asize/=rsize ->
	           convK "gif" "pnm" url s
	         where asize = pP w h
	       _ -> put (Right (url,Just (GifFmt,s))) toPpmK
	   PngFmt -> convK "png" "pnm" url s
	   XbmFmt -> convK "xbm" "pbm" url s
	   JpgFmt -> convK "jpg" "ppm" url s
	   _ -> puts [Right (url,Nothing), errUnknown url s] toPpmK

    errUnknown (url,_) s = Left (url2str url++": unknown type: "++take 4 s)

    convK infmt outfmt r@(_,optSize) s =
      convertImageK infmt outfmt optSize s $ \ opts' ->
      case opts' of
        Just s' -> put (Right (r,Just (PnmFmt,s'))) toPpmK
	Nothing -> toPpmK

data ImgFormat = PnmFmt | PngFmt | GifFmt | JpgFmt | XbmFmt | UnknownFmt

imgFormat s =
  case s of
    '\137':'P':'N':'G':'\r':'\n':'\26':'\n':_ -> PngFmt
    'P':c:_ | c `elem` "123456" -> PnmFmt
    'G':'I':'F':_ -> GifFmt
    '#':'d':'e':'f':_ -> -- is this a good way to detect xbm format?
      XbmFmt
    _:_:_:_:_:_:'J':'F':'I':'F':_ -> -- jpeg detection ?!
      JpgFmt
    _ -> UnknownFmt

storeImgK fmt s cont =
  getLocalTime $ \ ct ->
  let t = ctPicosec ct `div` 10000000
          + 100 * fromIntegral (ctSec ct + 60*ctMin ct)
      base = "/tmp/wwwb"++show (t-77837317200)++"."
      filename = base++fmt
  in hIOSucc (WriteFile filename s) $
     cont base filename

convFileK infmt outfmt optSize base infile cont =
  let outfile = base++outfmt
      convprg = converter infmt outfmt
      converter "jpg" "ppm" = "djpeg"
      converter "gif" "ppm" = "giftopnm"
      converter "png" "ppm" = "pngtopnm"
      converter "pgm" "pbmthr" = "pgmtopbm -thr"
      converter "pbmthr" fmt = converter "pbm" fmt
      converter infmt outfmt = infmt++"to"++outfmt
      scalecmd =
	case optSize of
	  Nothing -> ""
	  Just (Point w h) ->
	    unwords ["|","pnmscale","-width",show w,"-height",show h]
      cmd = unwords [convprg,infile,scalecmd,">",outfile]
  in putsK [High (Left ("Converting image with: "++cmd))] $
     haskellIO (System cmd) $ \ resp ->
     hIOSucc (DeleteFile infile) $
	-- delete outfile in case of error?
     cont base (optignore resp outfile)
     -- giftopnm sometimes says it fails even though the result useful

optignore =
  if argFlag "ignoreconverr" False
  then const Just
  else \ resp -> case resp of Success -> Just ; _ -> const Nothing

optReadImgK Nothing cont = cont Nothing
optReadImgK (Just filename) cont =
  readImgK filename $ \ s ->
  cont (Just s)

readImgK filename cont =
    hIO (ReadFile filename) $ \ (Str s) ->
    putsK [High (Left "Done with conversion"){-,Low Flush-}] $
    hIOSucc (DeleteFile filename) $
    cont s

convertImageK infmt outfmt optSize s cont =
    storeImgK infmt s $ \ base infile ->
    convFileK infmt outfmt optSize base infile $ \ _ outfile  ->
    optReadImgK outfile $
    cont

parseImg (fmt,s) =
  case fmt of
    PnmFmt -> parsePNM s
    GifFmt ->
      case parseGIF s of
        Right gif -> Right (gif2ppm (decompressGIF gif))
	Left err -> Left err

ltF f = merge>^=<idLeftF f
  where
    merge (Left x) = Left x
    merge (Right y) = y
