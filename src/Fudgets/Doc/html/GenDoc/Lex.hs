module Lex(hlex) where
import Char

#ifdef __HASKELL98__
#define isAlphanum isAlphaNum
#endif

hlex "" = []
hlex s@(c:cs) =
  if c=='_' || isUpper c
  then con (span isIdChar s)
  else if c=='?' || isLower c
       then var c (span isIdChar cs)
       else if isSpace c
	    then hlex (dropWhile isSpace cs)
	    else plain (lex s)

plain ((s1,s):_) = s1:hlex s

var c (s1,s) = (c:s1):hlex s

con (s1,"") = [s1]
con (s1,s@(c:s')) =
  s1: if c=='.'
      then ".":hlex s'
      else hlex s
  
isIdChar c = c=='_' || c=='\'' || isAlphanum c
