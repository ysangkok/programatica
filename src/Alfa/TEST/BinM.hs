module BinM where
import QuickCheck2
import Monad

   
data  Natural = Natural [Bool]
     deriving Eq

instance Show Natural where
  show (Natural []) = ""
  show (Natural (True:xs)) = "2" ++ show(Natural xs)
  show (Natural (False:xs)) = "1" ++ show(Natural xs)


b2i :: Natural -> Int
b2i (Natural xs) = b2i' xs

b2i'  [] = 0
b2i'  (False:xs) =2* (b2i' xs) + 1
b2i' (True:xs) = 2*(b2i' xs) + 2
toN :: (Integral a) => a -> [Bool]
toN n | n < 0   = error$ "toN" ++ show n
      | n == 0  = []
      | odd n   = False: toN ((n-1) `div` 2)
      | True    = True: toN ((n-2) `div` 2)



instance Arbitrary Natural where
   arbitrary = resize 5 (fmap Natural arbitrary)

