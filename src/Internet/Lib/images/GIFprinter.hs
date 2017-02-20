module GIFprinter(printGIF) where
import GIF
import GIFcompress(compressGIFLZW)

type Printer = String->String

printGIF gif = prGIF gif ""

prGIF (GIF sig sd optcm dbs) =
    prSignature sig .
    prScreenDescriptor sd .
    maybe id (prColorMap sbpp) optcm .
    prDataBlocks sbpp dbs .
    prChar ';'
  where sbpp = sbitsPerPixel sd

prSignature sig rest = sig++rest

prScreenDescriptor (SD w h hasmap cres sf bpp bg asp) =
  prWord w .
  prWord h .
  prByte (bit 7 hasmap + bits 3 4 (cres-1)+ bit 3 sf + bits 3 0 (bpp-1)) .
  prByte bg .
  prByte asp

prColorMap :: Int -> ColorMap -> Printer
prColorMap bpp cm = prMany prRGB cm . prBytes (replicate pad 0)
  where pad = 3*(2^bpp-length cm)

prRGB :: RGB -> Printer
prRGB (r,g,b) = prBytes [r,g,b]

prDataBlocks = prMany . prDataBlock

prDataBlock sbpp = either prExtensionBlock (prImage sbpp)

prExtensionBlock (EB f bs) =
  prChar '!' .
  prByte f .
  prBlocks bs

prImage sbpp (Image imd0 optcm rd) =
    prChar ',' .
    prImageDescriptor imd .
    maybe id (prColorMap bpp) optcm .
    prRasterData bpp rd
  where
    bpp = ibitsPerPixel imd
    -- if no colormap, use screen bpp, not image bpp.
    imd = maybe (imd0 {ibitsPerPixel=sbpp}) (const imd0) optcm

prImageDescriptor (ID l t w h hasmap ilace bpp) =
  prMany prWord [l,t,w,h] .
  prByte (bit 7 hasmap + bit 6 ilace + bits 3 0 (bpp-1))

prRasterData bpp rd =
  case rd of
    Left cbs -> prCompressedBlocks cbs
    Right ps -> prCompressedBlocks (compressGIFLZW c ps)
      where c = max 2 bpp

prCompressedBlocks (CB c bs) = prByte c . prBlocks bs

prBlocks bs = prMany prBlock bs . prByte 0
  where prBlock bs = prByte (length bs) . prBytes bs

---

prWord w = prByte (w `mod` 256) . prByte (w `div` 256)

prByte :: Byte -> String -> String
prByte b rest = toEnum b : rest -- 0<=b<=255 !!

prBytes :: [Byte] -> String -> String
prBytes bs rest = map toEnum bs ++ rest

prChar c rest = c:rest

prMany prOne = foldr ((.) . prOne) id

bit = bits 1
bits n p x = (2^p)*bits0 n x

bits0 n x =
    if  b `div` 2^n == 0
    then b
    else error ("The value "++show x++" ("++show b++") doesn't fit in "++ 
                show n ++ " bits.")
  where b = fromEnum x
