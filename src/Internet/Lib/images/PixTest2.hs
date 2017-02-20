module Main(main) where
import AllFudgets
import PNMparser
import PNM
import PNM2PixmapK
import Dither
import PixmapDisplayF
import NonStdTrace(trace)
import Utils2(mix)
import IO(progArgs)

title = mix progArgs " "
main = fudlogue $ oColor $ \ccube ->
       allcacheF $ simpleShellF title [] Nothing (pixF ccube)

filename = argKey "file" "test.ppm"

pixF ccube = pixmapDisplayF >==< (ioF $
   hIOK (ReadFile filename) $ \(Str file) -> 
   case parsePNM file of
     Left err -> error ("cannot parse PPM file " ++ filename ++ "\n" ++err)
     Right ppm@(PNM size (PPM max l)) -> 
       if timeparse then error (show $ length l) else 
       pnm2PixmapK ccube ppm $ \pix -> putsK [High pix] nullK)

colorCube = read $ argKey "colorCube" "6"
timeparse = (argKey "timeparse" "no") == "yes"

oColor = if colorCube == 0 then ($Nothing)
	 else allocColorCube colorCube . (.Just)

