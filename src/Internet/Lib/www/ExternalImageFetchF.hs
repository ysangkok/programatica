module ExternalImageFetchF(imageFetchF) where
import AllFudgets -- need PixmapId
import URL(URL)

type Req a = (a, (URL, Maybe Point))
type Resp a = (a, ((URL, Maybe Point), (Size, PixmapId)))

imageFetchF :: (Show a,Read a) => F (Req a) (Resp a)
imageFetchF =
    rd >^=< inputLinesSP >^^=< filterLeftSP >^^=<
    idLeftF stderrF >==< delayF (subProcessF imagefetcher)
    >=^< sh
  where
    sh (x,y) = show (show x,y)++"\n"
    rd p = (read x,y) where (x,y) = read p::(Resp String)

imagefetcher = argKey "imagefetcher" "ImageFetcher"
