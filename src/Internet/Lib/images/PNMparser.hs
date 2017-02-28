module PNMparser(parsePNM) where
import Prelude
import Fudgets(Size(..),Point(..),RGB(..),oo)
import Utils2Janus(ord)
import PNM
import Data.Char(isSpace)

--import NonStdTrace

infixl 3 $>
infixl 3 $$
infixl 4 `ap`

type S e s a = s -> (Either e a,s)

($>) :: S e s a -> (a -> S e s b) -> S e s b
(f $> g) s = case f s of
               (Right a,s) -> g a s
	       (Left e,s) -> (Left e,s)
f $$ g = f $> (const g)
unit a = \s -> (Right a,s)
fail' = \s -> (Left s,s)
run f = fst . f
fm `ap` xm = fm $> \ f -> xm $> \ x -> unit (f x)

skipSpace = dropWhile isSpace

takeWord l = get [] $ skipSpace l where
  get a [] = (reverse a,[])
  get a (x:xs) = if x == '#' then get a $ drop 1 $ dropWhile (/='\n') xs
                 else if isSpace x then (reverse a,xs)
		      else get (x:a) xs

getWord l = let (w,rest) = takeWord l in (if null w then Left "EOF!"
						    else Right w,rest)

getRest l = (Right l,[])

getRaw maxval = getRest $> (unit.group.map ord)
  where
    group = if maxval<256 then id else groupBigEndian
    groupBigEndian (hi:lo:rest) = 256*hi+lo:groupBigEndian rest
    groupBigEndian _ = []

getAscii = getRest $> (unit.map read.words) -- !! read can fail
 
parsePNM :: String -> Either String PNM
parsePNM = 
    run (getWord $> \fmt ->
         unit (unpadPNM `oo` PNM) `ap` getSize `ap` parseBody fmt)

parseBody fmt =
  case fmt of
    "P1" -> parsePBM getBits
    "P2" -> parsePGM (const getAscii)
    "P3" -> parsePPM (const getAscii)
    "P4" -> parsePBM getRawBits
    "P5" -> parsePGM getRaw
    "P6" -> parsePPM getRaw
    _    -> fail'

parsePPM get=
     getInt $> \ maxval -> unit (PPM maxval) `ap` getRGBs maxval
  where
     getRGBs maxval = unit (groupWith3 RGB) `ap` get maxval

parsePGM get= getInt $> \ maxval -> unit (PGM maxval) `ap` get maxval

parsePBM get = unit PBM `ap` get

getBits = unit (\grays -> [g/=0 | g<-grays]) `ap` getAscii

getRawBits =
     unit (\bytes -> [bit | byte<-bytes, bit<-explode byte]) `ap` getRaw 1

explode = reverse.take 8.expl
  where
    expl n = (n `mod` 2 /= 0):expl (n `div` 2)

getSize =
     getInt $> \width ->
     getInt $> \height ->
     unit (Point width height)

getInt = getWord $> \w -> 
           case reads w of
		(i,_):_ -> unit i
		_ -> fail'

unpadPNM (PNM s@(Point w _) (PBM bits)) | w>0 =
   PNM s (PBM (unpad w (((w+7) `quot` 8)*8) bits))
unpadPNM pnm = pnm

unpad t d [] = []
unpad t d xs = take t xs++unpad t d (drop d xs)

groupWith3 f (x1:x2:x3:xs) = f x1 x2 x3:groupWith3 f xs
groupWith3 _ _ = []
