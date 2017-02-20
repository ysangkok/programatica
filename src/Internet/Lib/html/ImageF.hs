module ImageF where
import Fudgets
import ParseURL(parseURL)
import URL(URL,joinURL,url2str)
import PixmapDisplayF(pixmapDisplayF',PixmapId)
import Data.ListMap(lookupWithDefault)
import Data.Maybe(listToMaybe)
import Monad(ap)

type ImageOutput = (URL,Maybe Size)
type ImageInput = ((URL,Maybe Size),(Size,PixmapId))

imageF = imageF' 3

imageF' :: Size -> URL -> (Maybe String,[(String,String)]) -> F ImageInput ImageOutput
imageF' border purl (src,attrs) =
    maybe altF imgF (src >>= parseURL)
  where
    altF = sepF border $ labelF alt
    alt = lookupWithDefault attrs "??" "ALT"
    optSize =
	do w <- lookupInt "WIDTH" attrs
	   h <- lookupInt "HEIGHT" attrs
	   return (Point w h)
      where
	lookupInt k xs =
	  do s<-lookup k xs
	     (i,"") <-listToMaybe (reads s)
	     return i

    imgF rurl = sepF border $ putF (url,optSize)
	                      (pixmapDisplayF' optSize) >=^< snd'
      where
	snd' ((url',_),y) = ctrace "imageF" (url2str url,url2str url') $ y
	url = joinURL purl rurl
	--pick ((url',_),img) = if url'==url then Just img else Nothing
