module ParseURL where
import ParsOps
import URL
import Char(isAlpha,isDigit)

parseURL s =
    case parse url s of
      Right u -> Just u
      _ -> Nothing
  where
    url = unit absolute `ap` proto `ap` host `ap` port `ap` path `ap` fragment

    proto = maybeP (some (scan isAlpha) `chk` tok ':')
    host = maybeP (toks "//" `cap` some (scan (`notElem` ":/")))
    port = maybeP (tok ':' `cap` unit read `ap` some (scan isDigit))
    path = many (scan (/='#'))
    fragment = maybeP (tok '#' `cap` some (lit Just))

--    absolute "file" Nothing Nothing path = Relative path
    absolute proto host port path fragment =
        URL proto host port path' fragment
      where path' = if path=="" && host/=Nothing then "/" else path
