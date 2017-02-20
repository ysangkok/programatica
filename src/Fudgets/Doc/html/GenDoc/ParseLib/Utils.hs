module Utils2(module Utils2,chr,ord) where
import Char
import HO(apFst,apSnd)

-- For Haskell 1.3

-- Something is missing in Haskell 1.3...
--chr = toEnum :: (Int->Char)
--ord = fromEnum :: (Char->Int)

pair x y = (x,y)
apair (f,g) (x,y) = (f x,g y)
aboth f (x,y) = (f x,f y)
swap (x,y) = (y,x)
pairwith f x = (x,f x)
dup x = (x,x)

{-
stoi s = f 0 s
  where f n "" = n
  	f n (c:cs) = f (n*10+ord c-ord '0') cs
-}

space n = replicate n ' '

--uncurry f (x,y) = f x y

stripMaybeDef def Nothing = def
stripMaybeDef _ (Just x) = x


words' cs =
      case break isSpace cs of
        ([],[]) -> []
	([],cs2) -> words'' cs2
	(cs1,cs2) -> cs1:words'' cs2

words'' cs =
      case span isSpace cs of
        ([],[]) -> []
	([],cs2) -> words' cs2
	(cs1,cs2) -> cs1:words' cs2

expandtabs n = exp n
  where
      exp k [] = []
      exp k ('\t':xs) = space k ++ exp n xs
      exp k (x:xs) = x:exp (if k==1 then n else k-1) xs

mix :: [[a]] -> [a] -> [a]
mix [] d = []
mix (x:xs) d = x++case xs of [] -> []; _ -> d ++ mix xs d

strToLower :: String -> String
strToLower = map toLower

mapPartition f [] = ([],[])
mapPartition f (x:xs) =
  case f x of
    Just y -> apFst (y:) $ mapPartition f xs
    Nothing -> apSnd (x:) $ mapPartition f xs
