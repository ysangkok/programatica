module HaskellImports(parseImports) where
import Unlit
--import SimpleLex(simpleLex)
import ListUtil(chopList)
import Utils3(apFst)
import qualified PackedString as PS
import PackedString(PackedString,packString,unpackPS)
import Char

#ifdef __HBC__
#define lengthPS length
#define takePS take
#endif

parseImports literate =
    filter notPrelude .
    map (moduleName.tail) .
    filter ((==importPS) . head) .
    map (map packString.simpleLex) .
    filter (/= "") .
    map unindent .
    lines .
    (if literate then unlit "???" else id)

notPrelude name = {-PS.takePS lengthPreludePS-} name /= preludePS

unindent = dropWhile isSpace

moduleName (w:_) | w/=qualifiedPS = w
moduleName (_:w:_) = w

lengthPreludePS = PS.lengthPS preludePS
preludePS = packString "Prelude"
importPS = packString "import"
qualifiedPS = packString "qualified"

simpleLex = chopList token
  where
    token s = case first (lex s) of
	        (s@(c1:_),'.':r@(c2:_)) | isUpper c1 && isUpper c2 ->
	            apFst ((s++).('.':)) (token r)
		t -> t

    first [] = ("",[]) -- lexical error, skip the rest
    first (x:xs) = x
