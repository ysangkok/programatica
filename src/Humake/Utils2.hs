module Utils2 where

mix :: [[a]] -> [a] -> [a]
mix [] d = []
mix (x:xs) d = x++case xs of [] -> []; _ -> d ++ mix xs d
