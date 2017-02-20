mineFieldSizeX, mineFieldSizeY :: Int
mineFieldSizeX = 12
mineFieldSizeY = 12

newField n =
	    rs = newRands r
	    xs = map (`rem` nx) (everyOther rs)
	    ys = map (`rem` ny) (everyOther (tail rs))
	    xys = addOne {-((n `min` mineFieldSizeX * mineFieldSizeY) `max` 0)-}n xs ys []
