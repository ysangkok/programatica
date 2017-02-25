module PNM2GIF(pnm2gif,pbm2gif,pgm2gif,ppm2gif) where
import GIF
import PNM
import qualified Fudgets as F
import Data.List(nub)
import qualified Data.Map.Strict as OM

pnm2gif (PNM (F.Point w h) pnmdata) =
  case pnmdata of
    PBM bits -> pbm2gif w h bits
    PGM max grays -> pgm2gif w h max grays
    PPM max rgbs -> ppm2gif w h max rgbs

pgm2gif w h max grays = ppm2gif w h max [F.RGB g g g|g<-grays]

pbm2gif w h bits = GIF "GIF87a" sd (Just cm) [Right im]
  where
    sd = SD w h True 1 False 1 0 0
    --cm = [(255,255,255),(0, 0, 0)]
    cm = [(0, 0, 0),(255,255,255)]
    im = Image id Nothing (Right ps)
    ps = map (fromEnum.not) (bits::[Bit])
    id = ID 0 0 w h False False 1

ppm2gif w h maxi rgbs = GIF "GIF87a" sd (Just cm) [Right im]
  where
    sd = SD w h True 8 False bpp 0 0
    id = ID 0 0 w h False False bpp
    iscale i = 255 * i `div` maxi
    rgbscale (F.RGB r g b) = (iscale r,iscale g,iscale b)
    bpp = log2 (length cm)
    rgbs' = map rgbscale rgbs
    cm = nub rgbs'
    cmm = OM.fromList (zip cm [0..])
    ps = [OM.lookupWithDefault cmm undefined rgb | rgb <- rgbs']
    im = Image id Nothing (Right ps)

log2 0 = 0
log2 n = 1+log2 (n `div` 2)
