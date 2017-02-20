module URLdecode where
--import Data.ListUtil(breakAt)
import Data.List.Split(chop)
import Utils2Janus(aboth,chr,ord)
import Data.Char(toUpper)

type Query = [(String,String)]


-- Janus added this function from https://hackage.haskell.org/package/hmatrix-0.16.1.3/src/src/Data/Packed/Matrix.hs
breakAt c l = (a++[c],tail b) where
    (a,b) = break (==c) l

decodeQuery :: String -> Query
decodeQuery = map (aboth decode . breakAt '=') . chopList (breakAt '&')

decode [] = []
decode ('%':d1:d2:cs) = chr(16*fromhex d1+fromhex d2):decode cs
decode ('+':cs) = ' ':decode cs
decode (c:cs) = c:decode cs

fromhex c =
  if '0'<=c && c<='9'
  then ord c - ord '0'
  else ord (toUpper c)-(ord 'A'-10)
