module HbcWord(module Word) where
import Word
--import Ix

--default(Int)

{-
instance Ix Short where
  range (lo,hi) = map fromIntegral (range (shortToInt lo,shortToInt hi))
  index (lo,hi) i = shortToInt i- shortToInt lo
  inRange (lo,hi) i = lo<=i && i<=hi
-}
