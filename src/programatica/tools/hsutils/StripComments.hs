module StripComments(stripcomments) where
import HsLexerPass1(lexerPass1,startPos,nextPos)
import List(mapAccumL)

stripcomments :: String -> String
stripcomments = redo_layout . map snd . lexerPass1

redo_layout ts = concat . snd . mapAccumL place (y0,1) $ ts
  where y0 = fst . fst . head $ ts

place p0 (p,s) = (nextPos p s,space p0 p++s)

space (y0,x0) p@(y,x) =
  if y/=y0 
  then '\n':space (y,1) p -- squeeze blank lines
  else if x0<=x
       then replicate (x-x0) ' '
       else error "Weird layout bug?!"
