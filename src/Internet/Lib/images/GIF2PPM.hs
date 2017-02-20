module GIF2PPM(gif2ppm) where
import GIF
import PNM
import Array
import qualified Fudgets as F
import List(sortBy)
import ListUtil(chopList)

#ifdef __HASKELL98__
#define map fmap
#endif

gif2ppm gif = PNM (F.pP w h) (PPM 255 rgbs)
  where
    sd = screen_descriptor gif
    w = swidth sd
    h = sheight sd

    Just scmap = map (cmap (sbitsPerPixel sd)) (global_color_map gif)

    rgbs =
      case [(i,deinterlaced_raster_data i) | Right i <- data_blocks gif] of
        (i,Right r):_ -> map (icmap!) r
	  where
	    id = image_descriptor i
	    icmap = maybe scmap (cmap (ibitsPerPixel id)) (local_colorMap i)

cmap bits = listArray (0,2^bits-1) . map rgb
  where
    rgb (r,g,b) = F.RGB r g b

deinterlaced_raster_data image =
  (if interlace d
  then deinterlace (iwidth d) (iheight d)
  else id) (raster_data image)
  where
    d = image_descriptor image

deinterlace w h (Left cb) = Left cb
deinterlace w h (Right pixels) = Right . concat . reorder . lines $ pixels
  where
    lines = chopList (splitAt w)
    reorder = map snd . sortBy cmppos . zip poslist
    poslist = [0,8..l]++[4,12..l]++[2,6..l]++[1,3..l]
    l=h-1
    cmppos (p1,_) (p2,_) = compare p1 p2
