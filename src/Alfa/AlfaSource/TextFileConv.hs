module TextFileConv where
--import Char --(decodeUTF8,encodeUTF8)
import FudUTF8 -- from the Fudget Library
import Fud(argFlag)

fromFile =
  if utf8
  --if True -- better to always rely on the utf8magic mark?
  then decodeUTF8'
  else id

decodeUTF8' s =
  case splitAt (length utf8magic) s of
    (m,r) -> if m==utf8magic
             then decodeUTF8 r
	     else s

toFile =
  if utf8
  then (utf8magic++) . encodeUTF8
  else id

utf8magic = "--#UTF-8\n"

utf8 = argFlag "utf8" False

-- #ifdef __GLASGOW_HASKELL__
-- decodeUTF8 = undefined
-- encodeUTF8 = undefined
-- #endif
