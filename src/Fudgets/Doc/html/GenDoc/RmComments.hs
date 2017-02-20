module RmComments where

-- removes nested comments
-- Bug: wrong treatment of comment starters inside string literals.

rmcomments = rm 0
  where
    rm n [] = []
    rm n ('{':'-':s) = ' ':rm (n+1) s
    rm n ('-':'}':s) = rm (n-1) s
    rm 0 ('-':'-':s) = rm 0 (dropWhile (/='\n') s)
    rm 0 (c:s) = c:rm 0 s
    rm n (c:s) = rm n s
