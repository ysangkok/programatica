module PNM2PixmapK(pnm2PixmapK) where

import AllFudgets
import PNM
import ListMap(lookupWithDefault)
import Dither
import Data.Array
import Utils2Janus(mynub)

useDither = (argKey "dither" "yes") == "yes"

--pnm2PixmapK :: (Maybe ColorCube) -> PNM -> Cont (K a b) (Size,PixmapId)
pnm2PixmapK octbl (PNM size pnmdata) =
  let rgb2pix = rgb2pixmapK octbl size in
  case pnmdata of
    PPM maxcolval rgbs -> rgb2pix maxcolval rgbs
    PGM maxgrayval grays -> rgb2pix maxgrayval (map gray2rgb grays)
    PBM bits -> bits2pixmapK size bits

bits2pixmapK size bits cont =
  bits2pixelsK bits $ \pixels ->
  pixels2pixmapK size pixels $
  cont

bits2pixelsK bits cont =
  allocNamedColorPixel defaultColormap fgColor $ \ fg ->
  allocNamedColorPixel defaultColormap paperColor $ \ bg ->
  cont [if b then fg else bg | b<-bits]
  
--rgb2pixmapK :: (Maybe ColorCube) -> Size -> Int -> 
--	    [RGB] -> Cont (K a b) (Size,PixmapId)
rgb2pixmapK octbl size maxcolval rgbs cont =
  case octbl of
    Nothing -> rgb2pixelsK maxcolval rgbs
    Just ctbl ->  ($ rgb2pixelswithCube size ctbl maxcolval rgbs)
  $ \pixels ->
      pixels2pixmapK size pixels $
      cont

rgb2pixelsK maxcolval rgbs c =
       let srgbs = scaleMaxRGBs maxcolval rgbs
           rgbtbl = mynub srgbs
       in
       conts (tryAllocColor defaultColormap) rgbtbl $ \ocolortbl ->
       let table = fixcolors rgbtbl ocolortbl
	   pixels = map (lookupWithDefault table (error "unknown color?")) srgbs
       in c pixels

rgb2pixelswithCube size cube maxcolval rgbs =
     map table $ (if useDither && nc<255
                  then dither nc size maxcolval
		  else scaleRGBs nc maxcolval) rgbs
     where nc = colCubeMax cube
	   table = colCubeTable cube

pixels2pixmapK size pixels c =
  createPixmap size copyFromParent $ \pmap ->
  pmCreateGC pmap rootGC [GCFunction GXcopy] $ \gc ->
  putLow (pmCreatePutImage pmap gc (Rect origin size) zPixmap pixels) $
  xcommand (FreeGC gc) $
  c (size,pmap)

fixcolors rgbtbl ocolortbl =
    let realtable = [(rgb,colorPixel color) | 
		     (rgb,Just color) <- zip rgbtbl ocolortbl]
        findbest rgb realtable = find worst (head realtable) realtable where
	    find d best [] = best
	    find d best (e @ (rgb',pix):rest) = let d' = dist rgb rgb' in
	       if d' < d then find d' e rest else find d best rest
	    dist (RGB r1 g1 b1) (RGB r2 g2 b2) = 
		 map (\x->x*x) $ [r1-r2,g1-g2,b1-b2]
	    worst = dist (RGB 0 0 0) (RGB maxRGB maxRGB maxRGB)

    in map (\(rgb,oc) -> case oc of 
                      Nothing -> findbest rgb realtable
		      Just c -> (rgb,colorPixel c)) $ zip rgbtbl ocolortbl

