module AlfaToks where

import Operations
import Greek (mkGreek)
import Arabic (mkArabic)
import Hebrew (mkHebrew)
import Russian(mkRussian,mkRusKOI8)
import qualified AlfaPluginKit as A

untoksA :: String -> String
untoksA s = prUpper (ut s) where 
  ut s = case s of
    ' ' : '.': s'           -> '.' : prUpper (ut s')
    ' ' : c : s' | isPunct c -> c  : ut s'
    '/' : '/' : s' -> utGreek s' []
    '/' : '+' : s' -> utHebrew s' []
    '/' : '-' : s' -> "/-" ++ utArabic s' []
    '/' : '_' : s' -> utRussian s' []
    '/' : '*' : s' -> utRusKOI8 s' []
    ' ' : 'a' : 'n' : ' ' : c : s' | isCons c -> " a " ++ (c : ut s') --- do better!
    ' ' : 'd' : 'e' : ' ' : 'l' : 'e' : ' ' : s' -> " du " ++ ut s'   --- do better!
    c : s' -> c : ut s'
    [] -> s
  isPunct = (`elem` ",:;")
  isCons c = not (c `elem` "aeioAEIO")

  utGreek s cc = case s of
    '/' : '/' : s' -> mkGreek cc     -- s' is discareded!!
    c : s' -> utGreek s' (cc ++ [c])
  utRussian s cc = case s of
    '_' : '/' : s' -> mkRussian cc     -- s' is discareded!!
    c : s' -> utRussian s' (cc ++ [c])
  utRusKOI8 s cc = case s of
    '*' : '/' : s' -> mkRusKOI8 cc     -- s' is discareded!!
    c : s' -> utRusKOI8 s' (cc ++ [c])
  utHebrew s cc = case s of
    '+' : '/' : s' -> mkHebrew cc     -- s' is discareded!!
    c : s' -> utHebrew s' (cc ++ [c])
  utArabic s cc = case s of
    '-' : '/' : s' -> mkArabic cc ++ "-/"      -- s' is discareded!!
    c : s' -> utArabic s' (cc ++ [c])


data AlfaToken
  = AString String | AMeta String | AVar String
  | ASymbol (Maybe String) String -- first argument is an optional font name
  | ABegPara | AEndPara | ANewLine
  deriving (Read,Show,Eq)

--gfOutputToAlfaText = A.plaintext . show . length -- to test if the parsing is slow
gfOutputToAlfaText = toAlfaText . prprA

prprA :: String -> [AlfaToken]
prprA str = prA (untoksA str) [] where
 prA ('#':'#':s) tt = 
  astr tt : ANewLine : prA (dropWhile (==' ') s) []
 prA ('[':'#':s) tt = astr tt : ABegPara : prA s []
 prA ('#':']':s) tt = astr tt : AEndPara : prA s []
--- prA ('[':'?':' ':'`':'`':s) tt = astr tt : pmA s []
 prA ('[':'?':s) tt = astr tt : pcA s []
 prA ('`':'`':s) tt = astr tt : pvA s []
 prA ('/':'-':s) tt = astr tt : psA s []
 prA (c:s)       tt = prA s (c:tt)
 prA []          tt = [astr tt]
 psA ('-':'/':s) tt = ASymbol justArabic (reverse tt) : prA s []
 psA (c:s) tt  = psA s (c:tt)
 psA s _       = [AString "UNTERMINATED METAVARIABLE"]
 pcA ('?':']':s) tt = AMeta (reverse tt) : prA s []
 pcA (c:s) tt  = pcA s (c:tt)
 pcA s _       = [AString "UNTERMINATED METAVARIABLE"]
--- pmA ('\'':'\'':' ':'?':']':s) tt = AMeta (reverse tt) : prA s []
--- pmA (c:s) tt  = pmA s (c:tt)
--- pmA s _       = [AString "UNTERMINATED METAVARIABLE"]
 pvA ('\'':'\'':c@'\'':s) tt = pvA (c:c:s) (c:tt)
 pvA ('\'':'\'':s) tt = AVar (reverse tt):prA s []
 pvA (c:s) tt = pvA s (c:tt)
 pvA [] tt = [AVar (reverse tt),AString "UNTERMINATED VARIABLE"]
 --ppA ('?':']':s) tt = AMeta (reverse tt) : prA s []
 --ppA (c:s) tt  = ppA s (c:tt)
 --ppA s _       = [AString "UNTERMINATED PARAGRAPH"]

 astr = AString . reverse

 justArabic = 
   Just "-arabic-newspaper-medium-r-normal--32-246-100-100-p-137-iso10646-1"
---   Just "-mutt-clearlyu-medium-r-normal--17-120-100-100-p-101-iso10646-1"

alfaTokens2String :: [AlfaToken] -> String
alfaTokens2String = ats 0 where
 ats n toks = case toks of
   AString s : tt -> indent n s +++ ats n tt
   ASymbol _ s : tt -> s +++ ats n tt
   AMeta s   : tt -> "[?" ++ s ++ "?]" +++ ats n tt
   ABegPara  : tt -> "\n" ++ ats (n+indentIncr) tt
   AEndPara  : tt -> "\n" ++ ats (n-indentIncr) tt
   ANewLine  : tt -> "\n" ++ ats n tt
   _ -> ""
 indentIncr = 2

type AText = [AlfaToken]

type P a = AText -> Maybe (a,AText)

toAlfaText = parse atext -- . filter (/=AString "")
  where
    atext :: P A.SyntaxText
    atext = concat `pmap` many para

    para = plainpara `orelse` nl `orelse` nested
    plainpara = (one . A.PlainPara . concat) `pmap` some aword

    nested = empty (one. A.NestedPara) `chk` ABegPara `ap` atext `chk` AEndPara

    one x = [x]

    nl = lit [] ANewLine

    aword = tok t
      where
        t (AString s) = Just (map plain (words s))
        t (ASymbol optfont s) = Just (map (A.TSymbol optfont) (words s))
	t (AVar s) = Just [A.TVar s]
	t (AMeta s) =
	    case reads s of
	      (n,[]):_ -> Just [meta n]
	      _ -> Just
	             --[A.syntext (A.eComment s (A.EMeta (metanum s)))]
		     [A.syntext (A.ETyped (A.EMeta metanum) (A.EVar (A.Var name)))]
		where
		  (prims,rname) = span (=='\'') (reverse s)
		  name = reverse rname
		  metanum = length prims
	t _ = Nothing

        meta = A.syntext . A.EMeta

        -- A quick hack for Chinese characters...
        plain s = if any isCJK s
		  then A.TSymbol (Just cjkfont) s
		  else A.TPlain s

        isCJK c =
-- #ifdef __HBC__
          '\x4e00' <= c && c <= '\x9fa5'
-- #else
--          False
-- #endif
        cjkfont = "-misc-fixed-medium-r-normal-ja-18-120-100-100-c-180-iso10646-1"

    --- Parsing combinators...

    tok _ [] = Nothing
    tok p (x:xs) = do y<-p x; return (y,xs)

    p1 `orelse` p2 = \ xs0 -> case p1 xs0 of
		                Just r -> Just r
				_ -> p2 xs0

    p1 `ap` p2 = \ xs0 -> do (f,xs1) <- p1 xs0
			     (x,xs2) <- p2 xs1
			     return (f x,xs2)

    p1 `chk` t = \ xs0 -> do (x,xs1) <- p1 xs0
			     (_,xs2) <- lit () t xs1
			     return (x,xs2)

    f `pmap` p = \ xs0 -> do (x,xs1) <- p xs0; return (f x,xs1)

    lit r x (y:xs) | x==y = Just (r,xs)
    lit r _ _ = Nothing

    some p = (:) `pmap` p `ap` many p
    many p = some p `orelse` empty []

    empty x xs = Just (x,xs)

    parse p xs =
      case p xs of
        Just (y,_) -> y
