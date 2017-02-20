{-| Simple matrix implementation: addition, multiplication,
construction from a list of rows (?) resp. columns (?).  [Andreas:
have not figured this out.]  -}

module Matrix where

import Array

type MIx = (Int,Int)
type Matrix b = Array MIx b

matrix :: MIx -> [(MIx,b)] -> Matrix b
matrix size = array ((1,1),size)

sizeMatrix :: Matrix b -> MIx
sizeMatrix m = size
            where (_,size) = bounds m

{-
cols ::  Matrix b -> [Array Int b]
cols m = [  array (1,r)  | c <- range 1 mc ] 
         mkCol :: Int -> Int -> Matrix b -> Array Int b
         mkCol mr c m = listArray (1,mr) [ m ! (r,c) | r <- range (1,mr)]

-}

listMatrix :: MIx -> [[b]] -> Matrix b
listMatrix  size bs = array ((1,1),size) (zip (range ((1,1),size)) (foldl (++) [] bs))


isEmptyMatrix :: Matrix b -> Bool
isEmptyMatrix m = mr < 1 || mc < 1
        where (mr,mc) = sizeMatrix m


addMatrix :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
addMatrix f m1 m2 = matrix size [ (i,f (m1!i) (m2!i)) | i <- range ((1,1),size)]
                    where size  = sizeMatrix m1


multMatrix :: (a -> b -> c) -> (c -> c -> c) -> Matrix a -> Matrix b -> Matrix c
multMatrix t p m1 m2 = matrix (mr1,mc2) [((r,c),res) | r <- [1..mr1] , c <- [1..mc2], let res = multiply t p mc1 r c m1 m2 ]
          where 
                (mr1,mc1) = sizeMatrix m1
                (mr2,mc2) = sizeMatrix m2
                multiply :: (a -> b -> c) -> (c -> c -> c)-> Int -> Int -> Int -> Matrix a -> Matrix b -> c
                multiply t p size r c m1 m2 =
                      foldl1 p [ t (m1!(r,i)) (m2!(i,c)) | i <- [1..size]]


diagonal :: Matrix b -> Array Int b
diagonal m = array (1,r)  [(i,m!(i,i)) | i <-[1..r]]
      where    (r,_) = sizeMatrix m
                
printMatrix :: Show b => Matrix b -> String 
printMatrix m 
  | r < 1 || c < 1 = "[]"
  | otherwise = unlines [ printRow i c m | i <- range (1,r)]
          where printRow :: Show b => Int -> Int -> Matrix b -> String
                printRow r mc m = unwords [show (m ! (r,j)) | j <- range (1,mc)]
                (r,c) = sizeMatrix m


{-
list1,list2,list3 :: [[Int]]
list1 = [[1,2,3],[0,2,4]]
list2 = [[0,2,4],[1,2,3]]
list3 = [[1,3],[0,5],[1,1]]


mat1,mat2,mat3 :: Matrix Int
mat1 = listMatrix (2,3) list1

mat2 = listMatrix (2,3) list2

mat3 = addMatrix (+) mat1 mat2

mat4,mat6 :: Matrix Int
mat4 = listMatrix (3,2) list3

--mat5 = listMatrix (2,3) (list2 ++ list1)

mat6 = multMatrix (*) (+) mat2 mat4

res4 = printMatrix (2,3) mat2

res5 = printMatrix (3,2) mat4

res6 = printMatrix (2,2) mat6



-}
