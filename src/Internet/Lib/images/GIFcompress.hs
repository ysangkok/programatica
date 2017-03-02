module GIFcompress(compressGIF,compressGIFLZW) where
import ListUtil(chopList)
import GIF
import GIFops(compressRasterData)
import BitStream(bput,bytestream,end)
import qualified OrdMap as Map

--import Trace(trace)
--trace x y = y
--tr x = trace (show x) x
--tr' s x = trace (s++show x) x

compressGIF = compressRasterData . compressGIFLZW

compressGIFLZW c = CB c . blocks . bytestream . lzwcompress c
  where
    blocks = chopList (splitAt 255)

lzwcompress root_code_size = compr0
  where
    compr0 = compr1 (inittable root_code_size)
    compr1 table = outputc table (clear table) . compr table []
    compr table [] [] = end' table
    compr table old [] = output table old (end' table)
    compr table old (p:ps) =
	case clookup (p:old) table of
	  Just _ -> compr table (p:old) ps
	  Nothing -> output table old $ compr2 table' p ps
	    where table' = add table (p:old)

    compr2 (Just table) p ps = compr table [p] ps
    compr2 Nothing p ps = bput' max_code_size (clear table) $
                          compr table [] (p:ps)
      where table = inittable root_code_size

    --output :: Table -> Pixels -> BitStream -> BitStream
    output table ps = outputc table (get table ps)
    outputc (T {code_size=size}) code = bput' size code

    end' table = outputc table (eoi table) end
      -- Is the eoi code encoded with the right number of bits?!

    bput' size code =
      --trace (show (size,code)) $
      bput size code

first (x:_) = [x]

-------------------------------------------------------------------------------

data Table = T { code_size,maxcode,clear,eoi,next:: Int,
                 entries::Map }

type Map = Map.OrdMap Pixels Code

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

add :: Table -> Pixels -> Maybe Table
add t@(T {next=next,entries=entries}) ps =
    -- The next line implements deferred clear code, discouraged in GIF89a std
    --if next>max_maxcode then Just t else
    bump (t { next=next+1,entries=Map.add (ps,next) entries })
  where
    bump t@(T {code_size=code_size,maxcode=maxcode}) =
      if next>maxcode
      then if code_size<max_code_size
           then Just t {code_size=code_size+1,maxcode=maxcode*2+1}
	   else Nothing -- throw away the table when it is full
      else Just t

max_code_size=12
max_maxcode=2^max_code_size-1

clookup :: Pixels -> Table -> Maybe Code
clookup [p] _ = Just p
clookup ps (T {entries=entries}) = Map.lookup ps entries

get :: Table -> Pixels -> Code
get table ps =
  case clookup ps table of
    Just y -> y
    Nothing -> error ("GIFcompress.get: "++show ps++" not in table")
