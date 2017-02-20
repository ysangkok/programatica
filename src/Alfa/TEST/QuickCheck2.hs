module QuickCheck2 where
import Random
-- Generator

newtype Gen a
  = Gen (Int -> StdGen -> a)

-- Arbitrary

class Arbitrary a where
  arbitrary   :: Gen a
  coarbitrary :: a -> Gen b -> Gen b


instance Arbitrary a => Arbitrary [a] where
  arbitrary          = sized (\n -> choose (0, n) >>= vector)
  coarbitrary []     = variant 0
  coarbitrary (a:as) = coarbitrary a . variant 1 . coarbitrary as

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [ arbitrary | i <- [1..n] ]

choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

elements :: [a] -> Gen a
elements xs = (xs !!) `fmap` choose (0, length xs - 1)


rand :: Gen StdGen
rand = Gen (\n r -> r)


sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> let Gen m = fgen n in m n r)

resize :: Int -> Gen a -> Gen a
resize n (Gen m) = Gen (\_ r -> m n r)


variant :: Int -> Gen a -> Gen a
variant v (Gen m) = Gen (\n r -> m n (rands r !! (v+1)))
 where
  rands r0 = r2 : rands r2 where (r1, r2) = next r0



instance Monad Gen where
  return a    = Gen (\n r -> a)
  Gen m >>= k =
    Gen (\n r0 -> let (r1,r2) = split r0
                      Gen m'  = k (m n r1)
                   in m' n r2)



instance Functor Gen where
  fmap f m = m >>= return . f


frequency :: [(Int, Gen a)] -> Gen a
frequency xs = choose (1, tot) >>= (`pick` xs)
 where
  tot = sum (map fst xs)

  pick n ((k,x):xs)
    | n <= k    = x
    | otherwise = pick (n-k) xs

instance Arbitrary Bool where
  arbitrary     = elements [True, False]
  coarbitrary b = if b then variant 0 else variant 1

instance Arbitrary () where
  arbitrary     = return ()
  coarbitrary _ = variant 0
