module Dew(decodeEncodedWords) where -- decode encoded-words
import Base64
import Dqp
import Utils2(words')
import ListUtil(chopList,breakAt)

decodeEncodedWords = concatMap decodeWord . words'

decodeWord w =
    case chopList (breakAt '?') w of
      ["=",charset,encoding,etext,"="]
         | "?" `notElem` [charset,encoding,etext] -> decodeEtext encoding etext
      _ -> w

decodeEtext encoding etext =
  case encoding of
    "Q" -> decodeQ etext
    "B" -> decodeBase64 etext
    _   -> encoding++"? "++etext

decodeQ = decodeQuotedPrintable . map uscore2space

uscore2space '_' = ' '
uscore2space c   = c
