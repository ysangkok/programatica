-- A class with operations to support error position reporting
module LexPos where

data Pos = Pos !Int !Int -- line & column within a file

startPos = Pos 1 0 -- lines start at 1, columns at 0 (in the internal repr.)
noPos = Pos (-100000) 0 -- negative line number indicates missing position info

instance Show Pos where show (Pos r c) = show r++":"++show (c+1)

class LexPos t where
  lexPos :: t -> Pos
  lexShow :: [t] -> String

instance LexPos Char where
  lexPos c = noPos
  lexShow = takeWhile (/='\n')

class NextPos t where
  nextPos :: Pos -> t -> Pos

instance NextPos Char where
  nextPos (Pos row col) c =
    case c of
      '\n' -> Pos (row+1) 0
      '\t' -> Pos row (col+(8-col `mod` 8))
      '\v' -> Pos row 0 -- from Agda
      '\f' -> Pos row 0 -- from Agda
      _    -> Pos row (col+1)


instance NextPos a => NextPos [a] where
  nextPos = foldl nextPos
