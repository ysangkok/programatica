{-# OPTIONS_GHC -cpp -O #-}

module HsLexUtils(module HsLexUtils,Token(..),isSymbol) where
import HsTokens
import qualified Data.Char as C

gotEOF [] = []
gotEOF as = [(GotEOF, reverse as)]

gotError as is =
  (ErrorToken, reverse as):
  if null is then [(GotEOF,[])] else [(TheRest,is{-reverse (take 80 is)-})]

-- Inlining the call to output does not make a big difference.
--output token as cont = (token, reverse as):cont

-- Not reversing the token string seems to save about 10% of the time with HBC.
-- The difference in speed seems insignificant with ghc-6.0.1 -O.
output token as cont = (token,reverse as):cont

-- This avoids constructing a closure for the call to reverse.
-- This saves about 10% too.
{-
output token as cont =
    rev as []
  where
    rev [] as' = (token,as'):cont
    rev (a:as) as' = rev as (a:as')
--}

#if __GLASGOW_HASKELL__>=606
isSymbol c = C.isSymbol c || C.isPunctuation c
#else
isSymbol _ = False
#endif

nestedComment as is next = nest 0 as is
  where
    nest n as is =
      case is of
	'-':'}':is -> if n==0
		      then next gotError ('}':'-':as) is
		      else nest (n-1) ('}':'-':as) is
        '{':'-':is -> nest (n+1) ('-':'{':as) is
	c:is -> nest n (c:as) is
	_ -> gotError as is -- EOF inside comment
