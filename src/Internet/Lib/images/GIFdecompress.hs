module GIFdecompress(decompressGIF,decompressGIFLZW) where
import GIF
import GIFops(decompressRasterData)
import BitStream(btake,bitstream,BitStream(..))
import qualified IntMap as Map

--import Trace(trace)
--trace x y = y
--tr x = trace (show x) x
--tr' s x = trace (s++show x) x

decompressGIF = decompressRasterData decompr
  where
    decompr (CB c bs) = decompressGIFLZW c (concat bs)

decompressGIFLZW c = lzwdecompress c . bitstream

lzwdecompress root_code_size = decompr0
  where
    decompr0 = decompr1 (inittable root_code_size)
    decompr1 table End = [] -- to avoid looping if the EOI code is missing
    decompr1 table bs =
      case get_next table bs of
        (c,bs) | c==clear table -> decompr0 bs
	       | c==eoi table -> []
	       | otherwise -> output (dget table c) $ decompr table c bs

    decompr table old End = [] -- to avoid looping if the EOI code is missing
    decompr table old bs =
      case get_next table bs of
	(c,bs) | c==clear table -> decompr0 bs
	       | c==eoi table -> []
	       | otherwise ->
	    case dlookup c table of
	      Just t -> output t $ decompr (add table (ot++first t)) c bs
	      Nothing -> output t $ decompr (add table t) c bs
		where t = ot++first ot
	  where
	    ot = dget table old

    output :: Bytes -> Bytes -> Bytes
    output t r = --trace ("output "++show t) 
                 (t++r)

    add' = add
    --add' table s = trace ("add "++show s++" = "++show (next table)) $ add table s


first (x:_) = [x]

-------------------------------------------------------------------------------

data Table = T { code_size,maxcode,clear,eoi,next:: Int,
                 entries::Map }

type Map = Map.IntMap Bytes

type Bytes = [Byte]
type Code = Int

--inittable :: Int -> Table
inittable root_code_size = T code_size maxcode clear eoi next Map.empty
  where
    code_size = root_code_size+1
    clear=2^root_code_size
    eoi=clear+1
    next=clear+2
    maxcode=2*clear-1

add :: Table -> Bytes -> Table
add t@(T {next=next,entries=entries}) s =
    bump (t { next=next+1,entries=Map.add (next,s) entries })
  where
    bump t@(T {code_size=code_size,maxcode=maxcode}) =
      if next==maxcode && code_size<12
      then t {code_size=code_size+1,maxcode=maxcode*2+1}
      else t

dlookup :: Code -> Table -> Maybe Bytes
dlookup c (T {clear=clear,next=next,entries=entries}) =
  if c<clear
  then Just [c]
  else if c<next
       then Just (dlookup' c entries)
       else Nothing

dlookup' i t =
  case Map.lookup i t of
    Nothing -> error ("dlookup' "++show i)
    Just s -> s

dget :: Table -> Code -> Bytes
dget table i =
  case dlookup i table of
    Just y -> y
    Nothing -> error ("GIFdecompress.dget: "++show i++" not in table")

get_next :: Table -> BitStream -> (Code,BitStream)
{- -- Simple version:
get_next (T{code_size}) bs =
  case splitAt code_size bs of
    (w1,bs) -> --trace ("get "++show code_size++" = "++show (int w1)) $
               (int w1,bs)
-}

{- -- More efficient(?) version:
get_next (T{code_size,eoi}) = g code_size 1 0
  where
    g 0 e a bs = (a,bs)
    g n e a (b:bs) = g (n-1) (2*e) (a+e*fromEnum b) bs
    g n _ _ [] = trace ("GIFdecompress.get_next: expected "++show code_size++" bits, but got only "++show (code_size-n))
		 (eoi,[])
-}

-- Even more(?) efficient version:
get_next (T{code_size=code_size,eoi=eoi}) = {-tr.-} btake eoi code_size
  --where tr r@(c,_) = trace (show (code_size,c)) r
