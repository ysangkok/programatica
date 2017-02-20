module NewPP(Doc,(<>),($$),text,sep,nest,pretty) where

infixl <>
infixl $$

data Doc = Nil                 -- text ""
	 | NilAbove Doc        -- text "" $$ x
         | Str `TextBeside` Doc-- text s <> x
	 | Nest Int Doc        -- nest k x
	 | Doc `Union` Doc     -- x U y
	 | Empty               -- {}
	 deriving (Show)

type Str = (Int,String->String)
    -- optimised rep of strings: fast length, fast concat.
len (i,_) = i
(i,s) `cat` (j,t) = (i+j,s.t)
str s = (length s,(s++))
string (i,s) = s []



text s = str s `TextBeside` Nil

nest k x = Nest k x

x $$ y = aboveNest x 0 y

aboveNest Nil k y = NilAbove (nest k y)
aboveNest (NilAbove x) k y = NilAbove (aboveNest x k y)
aboveNest (s `TextBeside` x) k y = 
  seq k' 
  (s `TextBeside` (aboveNest (Nil<>x) k' y))
  where k' = k-len s
aboveNest (Nest k' x) k y = 
  seq k'' (Nest k' (aboveNest x k'' y))
  where k'' = k-k'
aboveNest (x `Union` y) k z = 
  aboveNest x k z `Union` aboveNest y k z
aboveNest Empty k x = Empty

Nil <> (Nest k x) = Nil <> x
Nil <> x = x
NilAbove x <> y = NilAbove (x <> y)
(s `TextBeside` x) <> y = s `TextBeside` (x <> y)
Nest k x <> y = Nest k (x <> y)
Empty <> y = Empty
(x `Union` y) <> z = (x <> z) `Union` (y <> z)

sep [x] = x
sep (x:ys) = sep' x 0 ys

-- sep' x k ys = sep [x,nest k y1,...,nest k yn]
sep' Nil k ys = fit (foldl (<+>) Nil ys)
                `Union` vertical Nil k ys
sep' (NilAbove x) k ys = vertical (NilAbove x) k ys
sep' (s `TextBeside` x) k ys = 
  s `TextBeside` sep' (Nil <> x) (k-len s) ys
sep' (Nest n x) k ys = Nest n (sep' x (k-n) ys)
sep' (x `Union` y) k ys = sep' x k ys `Union` vertical y k ys
sep' Empty k ys = Empty

vertical x k ys = x $$ nest k (foldr1 ($$) ys)
x <+> y = x <> text " " <> y

fit Nil = Nil
fit (NilAbove x) = Empty
fit (s `TextBeside` x) = s `TextBeside` (fit x)
fit (Nest n x) = Nest n (fit x)
fit (x `Union` y) = fit x
fit Empty = Empty

best w r Nil = Nil
best w r (NilAbove x) = NilAbove (best w r x)
best w r (s `TextBeside` x) = s `TextBeside` best' w r s x
best w r (Nest k x) = Nest k (best (w-k) r x)
best w r (x `Union` y) = nicest w r (best w r x) (best w r y)
best w r Empty = Empty

best' w r s Nil = Nil
best' w r s (NilAbove x) = NilAbove (best (w-len s) r x)
best' w r s (t `TextBeside` x) = 
  t `TextBeside` best' w r (s `cat` t) x
best' w r s (Nest k x) = best' w r s x
best' w r s (x `Union` y) = 
  nicest' w r s (best' w r s x) (best' w r s y)
best' w r s Empty = Empty

nicest w r x y = nicest' w r (str "") x y
nicest' w r s x y = if fits (w `min` r) (len s) x then x else y

fits n k x = if n<k then False else
               case x of
	         Nil -> True
		 NilAbove y -> True
		 t `TextBeside` y -> fits n (k+len t) y
		 Empty -> False

layout k (Nest k' x) = layout (k+k') x
layout k x = [' ' | i<-[1..k]] ++ layout' k x

layout' k Nil = "\n"
layout' k (NilAbove x) = "\n" ++ layout k x
layout' k (s `TextBeside` x) = string s ++ layout' (k+len s) x

pretty w r d = layout 0 (best w r d) :: String

