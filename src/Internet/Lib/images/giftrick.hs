import GIF
import GIFparser(parseGIF)
import GIFprinter(printGIF)
import GIFdecompress(decompressGIF)
import GIFops
import System(getArgs)
import ListUtil(chopList)
import Char(isDigit)
import List(transpose,partition)
import Random

main =
  do rnd <- newStdGen
     args <- getArgs
     f <- trick rnd args
     interact (either error (printGIF . f . decompressGIF) . parseGIF)

--trick :: [String] -> IO (File->File)
trick rnd (cmd:args) =
  case cmd of
    "topdown" -> rn topDown
    "bottomup" -> rn bottomUp
    "fromleft" -> rn fromLeft
    "fromright" -> rn fromRight
    "squares" -> rn squares
    "lace" -> rn nlace
    "random" -> return (randGif rnd)
    _ -> fail ("unknown trick: "++cmd)
  where
    rn f = return $ f (num args)
    num args =
      case args of
	n:_ | all isDigit n -> read n
	_ -> 2
trick _ _ = fail "trick missing"

squares n = splitImgs (concatMap (vstrips n) . hstrips n)
topDown = splitImgs . hstrips
bottomUp n = splitImgs (reverse . hstrips n)
fromLeft = splitImgs . vstrips
fromRight n = splitImgs (reverse . vstrips n)
nlace n = foldr1 (.) (replicate n laceGif)

splitImgs f = mapDataBlocks (concatMap bu)
  where
    bu = either ((:[]) . Left) (map Right . f)

hstrips n (Image imd optcm (Right ps)) =
    zipWith image [y0,y0+n..] $ lines ps
  where
    image y ps = Image (imd{top=y,iheight=length ps `div` w}) optcm (Right ps)
    ID {top=y0,iwidth=w} = imd
    --ID _ y0 w _ _ _ _ = imd
    lines = chop (n*w)

vstrips n (Image imd optcm (Right ps)) =
    zipWith image [x0,x0+n..] $ columns ps
  where
    image x ps = Image (imd{left=x,iwidth=length ps `div` h}) optcm (Right ps)
    ID {left=x0,iwidth=w,iheight=h} = imd
    --ID _ y0 w _ _ _ _ = imd
    columns = map (concat . transpose) . chop n . transpose . chop w

laceGif = mapDataBlocks lace
  where
    lace = map snd . uncurry (++) . partition fst . zip eo
    eo = True:False:eo

randGif rnd = mapDataBlocks rand
  where
    rand bs = permute (drandomRs rnd (length bs)) bs

    permute (i:is) xs =
      case splitAt i xs of
       (xs1,x:xs2) -> x:permute is (xs1++xs2)
    permute _ _ = []

---

mapDataBlocks f gif =
  gif { signature="GIF89a", data_blocks=f (data_blocks gif) }

chop = chopList . splitAt

drandomRs g 0 = []
drandomRs g n = x:drandomRs g' (n-1)
  where (x,g') = randomR (0,n-1) g
