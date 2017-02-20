{-# COMPILERFLAGS -fpbu #-}
module Load(splitAnnots) where

-- splitAnnot separates the annotations
-- (delimited by {-# #-}) from ordinary text.
-- It is designed to avoid space leaks (if the parts are used in the same
-- order they appear in the file). -fpbu flag required.

splitAnnots :: String -> (String,String)
splitAnnots s =
  case s of
    '{':'-':'#':' ':cs -> splitAnnots' cs
    c:cs -> let (annots,body) = splitAnnots cs
            in (annots,c:body)
    [] -> ([],[])

splitAnnots' s =
  case s of
    '#':'-':'}':cs -> splitAnnots cs
    c:cs -> let (annots,body) = splitAnnots' cs
            in (c:annots,body)
    [] -> ([],[])
