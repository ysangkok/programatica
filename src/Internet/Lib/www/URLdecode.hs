module URLdecode where
import ListUtil(chopList,breakAt)
import Utils2(aboth,chr,ord)
import Char(toUpper)

type Query = [(String,String)]

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
