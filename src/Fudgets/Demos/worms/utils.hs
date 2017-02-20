module utils where
import List(union)

ord = fromEnum :: (Char->Int)
chr = toEnum :: (Int->Char)

afst f (x,y)=(f x,y)
asnd f (x,y)=(x,f y)
aboth f (x,y)=(f x,f y)
apair (f,g) (x,y)=(f x,g y)
pairwith f x=(x,f x)
swap (x,y) = (y,x)

(f `oo` g) x y = f (g x y)

anth _ _ [] = []
anth 1 f (x:xs) = f x:xs
anth n f (x:xs) = x:anth (n-1) f xs

dropto p=while (\l->l/=[] && (not . p . head) l) tail

number _ [] = []
number i (x:xs) = (i,x):number (i+1) xs

C f x y=f y x
loop f = let     yf = f yf in yf

	-- gmap g f = foldr g [] . map f
gmap g f = foldr (\x -> \ys->g (f x) ys) []
unionmap f = gmap union f

remove a (b:bs) | (a == b) = bs
remove a (b:bs) = b:remove a bs
remove a [] = []

replace p [] = [p]
replace (t,v) ((t',v'):ls) | (t == t') = ((t,v):ls)
replace p (l:ls) = l:replace p ls

-- lhead xs ys = take (length xs) ys, but the rhs is stricter
lhead (x:xs) (y:ys) = y:lhead xs ys
lhead _ _ = []

-- ltail xs ys = drop (length xs) ys, but the rhs is stricter
ltail [] ys = ys
ltail _ [] = []
ltail (x:xs) (y:ys) = ltail xs ys

-- lsplit xs ys = (lhead xs ys,ltail xs ys), but without the space leak
lsplit [] ys = ([],ys)
lsplit _  [] = ([],[])
lsplit (x:xs) (y:ys) =
	let (yhs,yts) = lsplit xs ys
	in (y:yhs,yts)

-- JSP 920928
part p []     = ([], [])
part p (x:xs) = 
    let  (ys, zs) = part p xs in
	if p x then (x:ys, zs) else (ys, x:zs)


issubset a b = all (C elem b) a

{-end-}

lindex l1 l2 =
        let len = length l1
            f [] n = -1
            f l  n = if l1 == take len l then n else f (tail l) (n+1)
        in  f l2 0

mix :: [[a]] -> [a] -> [a]
mix [] d = []
mix (x:xs) d = x++case xs of [] -> []; _ -> d ++ mix xs d

rightAdj :: [a] -> Int -> a -> [a]
rightAdj s n x = replicate (n - length s) x ++ s

randlist :: Int -> Int -> Int -> [Int]
randlist seed low high =
	let m = 2147483647
	    k = 10000
	    a = 16807
	    q = 127773
	    rr = 2836
	    r1 = random seed
	    r2 = random r1
	    r = (r1 `quot` (m `quot` k))*k + (r2 `quot` (m `quot` k))
	    random :: Int -> Int
	    random seed = 
		let hi = seed `quot` q
		    lo = seed `rem` q
		    seed' = a * lo - rr * hi
		    seed'' = if seed' < 0 then seed' + m else seed'
		in seed''
	in  r `rem` (high-low+1) + low : randlist r2 low high

while :: (a -> Bool) -> (a -> a) -> a -> a
while f g x = if f x then while f g (g x) else x
