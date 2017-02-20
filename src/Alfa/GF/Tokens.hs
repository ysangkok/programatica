module Tokens where
import Operations
import Parsers (Parser, fails, longestOfSome, pQuotedString, (***), (+..), (..+), literal)
import LexPos -- hmm
import List (isPrefixOf, isSuffixOf)

instance LexPos Str where
   lexPos t = noPos -- no position information available
   lexShow = unwords . map showTok . take 10

class (Eq a, Show a, Read a) => Token a where
 plusTok  :: a -> a -> a
 glueTok  :: a -> a -> a
 zeroTok  :: a
 readTok  :: String -> a
 showTok  :: a -> String  -- to print linearization results
 prTok    :: a -> String  -- to print grammars
 parseTok :: Parser Char a
 parseToks :: Parser Char [a]
 isZeroTok :: a -> Bool
 isPrefixTok :: a -> a -> Bool
 isSuffixTok :: a -> a -> Bool
 plusTok x y = x
 glueTok = plusTok
 readTok = read
 showTok = show
 prTok = showTok
 parseTok = fails "<parseTok not implemented>"
 parseToks = longestOfSome parseTok
 isZeroTok = (== zeroTok)

instance (Token a) => Token [a] where
 plusTok = (++)
 zeroTok = []
 parseTok = parseTok *** (:[])
 parseToks = parseToks *** map (:[])
 showTok = unwords . map showTok ---
 prTok = (prTList "++") . map prTok ---
 readTok = map readTok . words
 glueTok [] tt = tt
 glueTok ss [] = ss
 glueTok ss tt = init ss ++ glueTok (last ss) (head tt) : tail tt
 isPrefixTok t1 t2 = isPrefixTok (cc t1) (cc t2) where cc = foldr plusTok zeroTok
 isSuffixTok t1 t2 = isSuffixTok (cc t1) (cc t2) where cc = foldr plusTok zeroTok
 isZeroTok t = case t of
   [s] -> isZeroTok s
   _   -> t == zeroTok

instance Token () where
 zeroTok = ()
 showTok = show
 readTok _ = ()
 isPrefixTok _ _ = True
 isSuffixTok _ _ = True

newtype Str = Str String deriving (Read, Show, Eq, Ord) -- Ord for CP; maybe good

instance Token Str where
 showTok (Str s) = s
 readTok = Str
 prTok = show . showTok
 plusTok (Str s) (Str s') = Str (s ++ s')
 zeroTok = Str ""
 parseTok = pQuotedString *** Str
 parseToks = literal '[' +.. (pQuotedString *** map Str . words) ..+ literal ']'
 isPrefixTok (Str s) (Str s') = isPrefixOf s s'
 isSuffixTok (Str s) (Str s') = isSuffixOf s s'

type Alternatives a = (a, Variants a) -- default, fix-dependent variants
type Variants a = [(a,a)]             -- if ... then ...
type Fix a =  a -> a -> Bool
type Join a = a -> a -> a
noVariants s = (s,[])

chooseByFix :: Fix a -> a -> Variants a -> Maybe a
chooseByFix fix str vars = case vars of
  [] -> Nothing
  ((c,p):vv)  
   | fix c str -> Just p
   | otherwise -> chooseByFix fix str vv

joinByFix :: Join a -> Fix a -> Alternatives a -> a -> a
joinByFix join fix (def,vars) str = case chooseByFix fix str vars of
  Just p -> join str p
  _ -> join str def

joinToAlts :: Join a -> Fix a -> a -> Alternatives a -> Alternatives a
joinToAlts join fix str alts@(def,vars) =
  (join str def, [(c,join str v) | (c,v) <- vars])

joinAlts :: Join a -> Fix a -> Alternatives a -> Alternatives a -> Alternatives a
joinAlts join fix alts1 alts2@(def,vars) = 
  let jf = joinByFix join fix alts1 in (jf def, [(c,jf v) | (c,v) <- vars])

plusPrefixAlts :: Token a => Alternatives a -> Alternatives a -> Alternatives a
plusPrefixAlts = joinAlts (flip plusTok) isPrefixTok 

gluePrefixAlts :: Token a => Alternatives a -> Alternatives a -> Alternatives a
gluePrefixAlts = joinAlts (flip glueTok) isPrefixTok 

--- nothing for suffix so far...
