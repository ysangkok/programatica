module HtmlEntities(decode,encode) where
import Char(isDigit,isHexDigit,isSpace,chr,ord)
import Numeric(readHex)

decode [] = []
decode ('&':cs) =
  case break (\c -> c==';' || isSpace c) cs of
    (_,"") -> '&':decode cs
    ("",cs) -> '&':decode cs
    (ent,cs) -> decodeEntity ent++decode (case cs of
					    ';':cs' -> cs'
					    _      -> cs)
decode (c:cs) = c:decode cs

decodeEntity e =
  case e of
    "lt" -> "<"
    "gt" -> ">"
    "amp" -> "&"
    "quot" -> "\""
    "nbsp"    -> "\160" -- no-break space
    "iexcl"   -> "\161" -- inverted exclamation mark
    "cent"    -> "\162" -- cent sign
    "pound"   -> "\163" -- pound sterling sign
    "curren"  -> "\164" -- general currency sign
    "yen"     -> "\165" -- yen sign
    "brvbar"  -> "\166" -- broken (vertical) bar
    "sect"    -> "\167" -- section sign
    "uml"     -> "\168" -- umlaut (dieresis)
    "copy"    -> "\169" -- copyright sign
    "ordf"    -> "\170" -- ordinal indicator, feminine
    "laquo"   -> "\171" -- angle quotation mark, left
    "not"     -> "\172" -- not sign
    "shy"     -> "\173" -- soft hyphen
    "reg"     -> "\174" -- registered sign
    "macr"    -> "\175" -- macron
    "deg"     -> "\176" -- degree sign
    "plusmn"  -> "\177" -- plus-or-minus sign
    "sup2"    -> "\178" -- superscript two
    "sup3"    -> "\179" -- superscript three
    "acute"   -> "\180" -- acute accent
    "micro"   -> "\181" -- micro sign
    "para"    -> "\182" -- pilcrow (paragraph sign)
    "middot"  -> "\183" -- middle dot
    "cedil"   -> "\184" -- cedilla
    "sup1"    -> "\185" -- superscript one
    "ordm"    -> "\186" -- ordinal indicator, masculine
    "raquo"   -> "\187" -- angle quotation mark, right
    "frac14"  -> "\188" -- fraction one-quarter
    "frac12"  -> "\189" -- fraction one-half
    "frac34"  -> "\190" -- fraction three-quarters
    "iquest"  -> "\191" -- inverted question mark
    "Agrave"  -> "\192" -- capital A, grave accent
    "Aacute"  -> "\193" -- capital A, acute accent
    "Acirc"   -> "\194" -- capital A, circumflex accent
    "Atilde"  -> "\195" -- capital A, tilde
    "Auml"    -> "\196" -- capital A, dieresis or umlaut mark
    "Aring"   -> "\197" -- capital A, ring
    "AElig"   -> "\198" -- capital AE diphthong (ligature)
    "Ccedil"  -> "\199" -- capital C, cedilla
    "Egrave"  -> "\200" -- capital E, grave accent
    "Eacute"  -> "\201" -- capital E, acute accent
    "Ecirc"   -> "\202" -- capital E, circumflex accent
    "Euml"    -> "\203" -- capital E, dieresis or umlaut mark
    "Igrave"  -> "\204" -- capital I, grave accent
    "Iacute"  -> "\205" -- capital I, acute accent
    "Icirc"   -> "\206" -- capital I, circumflex accent
    "Iuml"    -> "\207" -- capital I, dieresis or umlaut mark
    "ETH"     -> "\208" -- capital Eth, Icelandic
    "Ntilde"  -> "\209" -- capital N, tilde
    "Ograve"  -> "\210" -- capital O, grave accent
    "Oacute"  -> "\211" -- capital O, acute accent
    "Ocirc"   -> "\212" -- capital O, circumflex accent
    "Otilde"  -> "\213" -- capital O, tilde
    "Ouml"    -> "\214" -- capital O, dieresis or umlaut mark
    "times"   -> "\215" -- multiply sign
    "Oslash"  -> "\216" -- capital O, slash
    "Ugrave"  -> "\217" -- capital U, grave accent
    "Uacute"  -> "\218" -- capital U, acute accent
    "Ucirc"   -> "\219" -- capital U, circumflex accent
    "Uuml"    -> "\220" -- capital U, dieresis or umlaut mark
    "Yacute"  -> "\221" -- capital Y, acute accent
    "THORN"   -> "\222" -- capital THORN, Icelandic
    "szlig"   -> "\223" -- small sharp s, German (sz ligature)
    "agrave"  -> "\224" -- small a, grave accent
    "aacute"  -> "\225" -- small a, acute accent
    "acirc"   -> "\226" -- small a, circumflex accent
    "atilde"  -> "\227" -- small a, tilde
    "auml"    -> "\228" -- small a, dieresis or umlaut mark
    "aring"   -> "\229" -- small a, ring
    "aelig"   -> "\230" -- small ae diphthong (ligature)
    "ccedil"  -> "\231" -- small c, cedilla
    "egrave"  -> "\232" -- small e, grave accent
    "eacute"  -> "\233" -- small e, acute accent
    "ecirc"   -> "\234" -- small e, circumflex accent
    "euml"    -> "\235" -- small e, dieresis or umlaut mark
    "igrave"  -> "\236" -- small i, grave accent
    "iacute"  -> "\237" -- small i, acute accent
    "icirc"   -> "\238" -- small i, circumflex accent
    "iuml"    -> "\239" -- small i, dieresis or umlaut mark
    "eth"     -> "\240" -- small eth, Icelandic
    "ntilde"  -> "\241" -- small n, tilde
    "ograve"  -> "\242" -- small o, grave accent
    "oacute"  -> "\243" -- small o, acute accent
    "ocirc"   -> "\244" -- small o, circumflex accent
    "otilde"  -> "\245" -- small o, tilde
    "ouml"    -> "\246" -- small o, dieresis or umlaut mark
    "divide"  -> "\247" -- divide sign
    "oslash"  -> "\248" -- small o, slash
    "ugrave"  -> "\249" -- small u, grave accent
    "uacute"  -> "\250" -- small u, acute accent
    "ucirc"   -> "\251" -- small u, circumflex accent
    "uuml"    -> "\252" -- small u, dieresis or umlaut mark
    "yacute"  -> "\253" -- small y, acute accent
    "thorn"   -> "\254" -- small thorn, Icelandic
    "yuml"    -> "\255" -- small y, dieresis or umlaut mark
    '#':'x':cs | all isHexDigit cs -> [chr $ fst $ head $ readHex cs]
    '#':cs | all isDigit cs -> [chr (read cs)]
    _ -> '&':e++";"

encode = concatMap encodeEntity

encodeEntity c =
  case c of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    _ | c>'\255' -> "&#"++show (ord c)++";" -- assume 8-bit Latin1 output!!
      | otherwise -> [c]

{-
-- Should probably use Utils.isSpace' instead of isSpace
collapseSpace s =
  case s of
    "" -> ""
    c:cs | isSpace c -> ' ':collapseSpace (dropWhile isSpace cs)
	 | otherwise -> c:collapseSpace cs
-}
