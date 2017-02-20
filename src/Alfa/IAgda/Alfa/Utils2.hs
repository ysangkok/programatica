module Alfa.Utils2 where
--import Char

mix :: [[a]] -> [a] -> [a]
mix [] d = []
mix (x:xs) d = x++case xs of [] -> []; _ -> d ++ mix xs d

apBoth f (x,y) = (f x,f y)

splitEitherList xys = ([x|Left x<-xys],[y|Right y<-xys])

-- From HBC's module HO:
apFst f (x,y) = (f x,y)
apSnd f (x,y) = (x,f y)

-- From HBC's module ListUtil:
mapSnd f = map (apSnd f)
mapFst f = map (apFst f)
