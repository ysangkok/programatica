{-# COMPILERFLAGS -O #-}
module Dither where

import AllFudgets
import PNM
import Maybe(fromJust)
import Array
--import Word
--import Bits
import HbcWord
import DialogueIO hiding (IOError)
import ContinuationIO(stderr)
--infixl 7 &
--infix 8 >>,<<

tr x = ctrace "visual" (show x) x

data ColorCube = Cube { colCubeMax :: Int, colCubeTable :: RGB -> Pixel }
--colCubeMax :: ColorCube -> Int
--colCubeMax c = ccm where RGB ccm _ _ = snd (bounds c)

allocColorCube ncolors c =
  defaultVisual $ \ visual ->
  --echoK (show visual) $
  case tr $ visualClass visual of
    TrueColor | useTrueColor -> c (trueColorCube visual)
    _ -> pseudoColorCube ncolors c -- hmm

trueColorCube visual = Cube ccmax pixel
  where ccmax = wordToInt mask
	--pixel rgb = ctrace "tccube" (show (rgb,pixel' rgb)) (pixel' rgb)
	pixel (RGB r g b) = Pixel (fromIntegral (rk*r+gk*g+bk*b))
	rk = tr "rk" $ wordToInt (1<<rPos)
	gk = tr "gk" $ wordToInt (1<<gPos)
	bk = wordToInt (1<<bPos)
	rPos = tr "rPos" $ ff1 rm+adj rm0
	gPos = tr "gPos" $ ff1 gm+(tr "adj gm0" $ adj gm0)
	bPos = ff1 bm+adj bm0
	adj m = ff0 m-ff0 mask
	mask = tr "mask" $ ((rm0&gm0)&bm0)
        rm = red_mask visual; gm = green_mask visual; bm = blue_mask visual
	rm0 = tr "rm0" $ m0 rm
	gm0 = tr "gm0" $ m0 gm
	bm0 = m0 bm
	m0 m = m>>ff1 m
	ff1 w = if  w&1 /=0 then 0 else 1+ff1 (w>>1)
	ff0 w = if  w&1 ==0 then 0 else 1+ff0 (w>>1)

#if 1
	(&) = bitAnd
	(>>) = bitRsh
	(<<) = bitLsh
#else
	(&) = (.&.)
	(>>) = shiftR
	(<<) = shiftL
#endif

	tr s x = ctrace "tccube" (s++": "++show x) x

-- avoid this inside color cache.
--pseudoColorCube :: Int -> Cont (F a b) ColorCube
pseudoColorCube ncolors c = 
    let ccmax = ncolors - 1
        rng @ (cstart,cend) = (gray2rgb 0,gray2rgb ccmax)
	srgbs = scaleMaxRGBs ccmax $ range rng
    in
    conts (tryAllocColorF defaultColormap) srgbs $ \ocolors ->
    if Nothing `elem` ocolors then 
      hIOSuccF (AppendChan stderr ("Couldn't allocate "++show ncolors++
		    " cube, trying with "++show (ncolors-1)++"\n")) $
      xcommandF (FreeColors defaultColormap 
                  [colorPixel c | Just c <- ocolors] (Pixel 0)) $
      pseudoColorCube (ncolors-1) c
    else
      let pixels = map (colorPixel . fromJust) ocolors
	  table = listArray rng pixels
	  ptable = Cube ccmax (table!)
      in  c ptable

gray2rgb g = RGB g g g

--maxRGB = 65535
mapRGB f (RGB r g b) = RGB (f r) (f g) (f b)
mapRGBs = map . mapRGB
--scaleRGBs n o = mapRGBs (\x->(x * n + o `div` 2) `div` o)
scaleRGBs n o = mapRGBs (\x->(x * n ) `div` o)
scaleMaxRGBs = scaleRGBs maxRGB



useSerpent = argFlag "serpent" False
use2Dither = argFlag "2dither" False
useTrueColor = argFlag "truecolor" True

dither colCubeMax (Point width _) maxc rgbs | width>=0 = 
    (if use2Dither
     then \srgbs -> let (hacc,drgbs) = unzip $ mapAcRGB2 0 0 0 $
			    zip (take width (repeat (gray2rgb 0))++hacc) srgbs
		    in drgbs
     else serpent . mapAcRGB d 0 0 0 . serpent) $ mapRGBs (*colCubeMax) rgbs
  where
    lines [] = []
    lines l = let (line,rest) = splitAt width l in line : lines rest
    serpent = if useSerpent then serp . lines else id where
       serp (l1:l2:r) = l1++reverse l2++serp r
       serp [l] = l
       serp [] = []
    mapAcRGB f rs gs bs l =  case l of
       (RGB r g b:rest) ->  RGB r' g' b' : mapAcRGB f rs' gs' bs' rest
			    where (r',rs') = f rs r
				  (g',gs') = f gs g
				  (b',bs') = f bs b
       [] -> []
#if 0
    d r v = (v+r) `divMod` maxc
#else
    d r v = let x = v+r	
		q = x `quot` maxc
		rm = x `rem` maxc
	    in if q+rm >= 0 then (q,rm) else error "huh?"
#endif

    mapAcRGB2 rs gs bs l =  case l of
       ((RGB rsv gsv bsv,RGB r g b):rest) ->  if x == x then
          (RGB rsv' gsv' bsv',RGB r' g' b') : mapAcRGB2 rs' gs' bs' rest else error "huh?"
			    where (r',rs',rsv') = d2 rs rsv r
				  (g',gs',gsv') = d2 gs gsv g
				  (b',bs',bsv') = d2 bs bsv b
				  x = r'+g'+b'
       [] -> []
#if 0
    d2 r sv v = let (v',r') = (v+r+sv) `divMod` maxc
		    sv' = r' `div` 2
		in (v',r'-sv',sv')
#else
    d2 r sv v = let x = v+r+sv
		    v' = x `quot` maxc
		    r' = x `rem` maxc
		    sv' = r' `div` 2
		in  if sv'+v' >=0 then (v',r'-sv',sv') else error "huh?"
#endif
