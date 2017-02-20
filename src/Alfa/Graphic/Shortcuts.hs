module Shortcuts(shortcuts,ordShortcuts) where
import ListUtil(mapSnd)
import List(sortBy)

default(Int)

ordShortcuts f = mapSnd snd  . sortBy num . shortcuts (f.snd) . zip [1..]
  where num (_,(i,_)) (_,(j,_)) = compare i j

shortcuts f = list . shorten . unique . tree f

data Tree a b = Node [a] [(b,Tree a b)] deriving (Show)

tree f = foldr (insert f) empty
empty = Node [] []
leaf x = Node [x] []

insert f x = insert' (f x)
  where
    insert' "" (Node xs ts) = Node (x:xs) ts
    insert' (c:cs) (Node xs ts) = Node xs (insert'' ts)
      where
	insert'' [] = [(c,insert' cs empty)]
	insert'' (t1@(c',t'):ts) =
	  if c'==c
	  then (c',insert' cs t'):ts
	  else t1:insert'' ts

list = list' []
  where
    list' s (Node xs ts) = map ((,) s) xs++concatMap list'' ts
      where
	list'' (c,t) = list' (s++[c]) t

unique t@(Node [x] []) = t
unique   (Node xs ts) = Node [] (ts'++unique' ts)
  where ts' = zipWith f [e | e<-extensions, e `notElem` cs] xs
	cs = map fst ts
	f e x = (e,leaf x)

unique' = mapSnd unique


shorten (Node [] ts) = Node [] (mapSnd shorten' ts)
shorten (Node xs ts) = Node xs (mapSnd shorten ts)

shorten' (Node [] [(_,t)]) = shorten' t
shorten' t = shorten t

extensions = '.':['1'..'9']++['a'..'z']++['A'..'Z']
