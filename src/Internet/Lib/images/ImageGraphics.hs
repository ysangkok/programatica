module ImageGraphics(module ImageGraphics,GIF.File,PNM) where
import AllFudgets
import PNM2PixmapK
import PNM(PNM)
import PNMparser
import GIF2PPM
import qualified GIF
import GIFparser
import GIFdecompress
import Dither(ColorCube,trueColorCube)
import DialogueIO hiding (IOError)

type OptColorCube = Maybe ColorCube
data PNMImage = PNMImage OptColorCube PNM
data GIFImage = GIFImage OptColorCube GIF.File
data PNMFile = PNMFile OptColorCube FilePath
data GIFFile = GIFFile OptColorCube FilePath

pnmImage = PNMImage Nothing
gifImage = GIFImage Nothing
pnmFile = PNMFile Nothing
gifFile = GIFFile Nothing

instance PixmapGen PNMImage where
    convToPixmapK (PNMImage optcb pnm) k =
        getTrueColorCube optcb $ \ optcb' ->
      	pnm2PixmapK optcb' pnm $ \ (size,pixmap) ->
      	k (PixmapImage size pixmap)
      where
        getTrueColorCube optcb@(Just _) k = k optcb
	getTrueColorCube Nothing k =
	  defaultVisual $ \ visual ->
	  case visualClass visual of
	    TrueColor -> k (Just (trueColorCube visual))
	    _ -> k Nothing

instance Graphic PNMImage where measureGraphicK = measureImageK
instance Graphic PNMFile  where measureGraphicK = measureImageK
instance Graphic PNM      where measureGraphicK = measureImageK
instance Graphic GIFImage where measureGraphicK = measureImageK
instance Graphic GIFFile  where measureGraphicK = measureImageK
instance Graphic GIF.File where measureGraphicK = measureImageK

instance PixmapGen GIFImage where
    convToPixmapK (GIFImage optcb gif) =
      convToPixmapK (PNMImage optcb (gif2ppm gif))

instance PixmapGen GIFFile where
    convToPixmapK (GIFFile optcb filename) k =
      hIO (ReadFile filename) $ \ (Str s) ->
      case parseGIF s of
        Right gif -> convToPixmapK (GIFImage optcb (decompressGIF gif)) k
	Left s -> error ("GIF file "++show filename++": "++s) --hmm

instance PixmapGen PNMFile where
    convToPixmapK (PNMFile optcb filename) k =
      hIO (ReadFile filename) $ \ (Str s) ->
      case parsePNM s of
        Right pnm -> convToPixmapK (PNMImage optcb pnm) k
	Left s -> error ("PNM file "++show filename++": "++s) --hmm

instance PixmapGen PNM where
  convToPixmapK = convToPixmapK . PNMImage Nothing

instance PixmapGen GIF.File where
  convToPixmapK = convToPixmapK . GIFImage Nothing

{- junk...
data Image img = Image (Maybe ColorCube) img

newtype File = File String
newtype Data = Data String
newtype URL = URL String

class Source a where
  getData :: a -> (Data->K i o)->K i o

instance Source File where
  getData (File name) k =
    hIO (ReadFile name) $ \ (Str s) ->
    k (Data s)

instance Source Data where
  getData d k = k d

--instance Source URL where getData (URL url) = ...

data GIFImage src = GIFImage src
data PNMImage src = PNMImage src    
data Image src = Image src

instance Graphic PNMFile where

instance Graphic GIFFile where

instance Source src => Graphic ImageFile where

instance ImageData img => Graphic (Image img) where
-}
