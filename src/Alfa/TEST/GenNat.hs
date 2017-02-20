module GenNat
   ( Nat(..)
   , n2i
   , i2n
   , zero, twenty, hundred
   , BinTree(..)
   , generate
--   , Stream(..)
   , AlfaExp(..)
   --, Tree(..)
   , tree
   , inf_tree
   --, seed
   
   ) where
import Random -- (Random, StdGen, newStdGen)
import QuickCheck2
import Monad
import UAbstract
import ToAlfaExp
import Natural
maxSize = 20

data Nat = Zero | Succ Nat
         deriving (Eq, Ord)
{-
data Stream = Seq Nat Stream
         deriving (Eq, Ord, Show)
-}
instance Show Nat where
   showsPrec _ n = shows (n2i n)

n2i :: Nat -> Int
n2i Zero = 0
n2i (Succ n) = 1 + n2i n

i2n :: Int -> Nat
i2n 0 = Zero
i2n n 
    | n > 0 = Succ (i2n (n - 1))
    | otherwise     = i2n (abs n)

zero = Zero ::Nat
twenty = i2n 20
hundred = i2n 100

{-
instance Random Nat where
  randomR (m, n) g = 
       let (a, b) = randomR (n2i m, n2i n) g
       in  (i2n a, b)
  random g = (i2n a, g')
      where (a, g') = random g
-} 

instance Arbitrary Nat where
  arbitrary = Gen(\n rnd -> i2n(fst (randomR(0, mod n 13) rnd))) -- (zero, i2n n) rnd)) --twenty) rnd)) 

generate :: Arbitrary a => Int -> StdGen -> Gen a -> a
generate n rnd (Gen m) = m size rnd'
  where
     (size, rnd') = randomR (0, n) rnd

data BinTree a    =  Leaf a
                    |  Node  a (BinTree a ) (BinTree a )
                       deriving (Eq, Ord)

instance Show a => Show (BinTree a) where
   showsPrec _ t =
     case t of
        Leaf n -> shows n
        Node n l r -> showChar '<'
                       . shows n
                       . showChar ':'
                       . shows l
                       . showChar '|'
                       . shows r
                       . showChar '>'

  


--empty                 =  Empty

leaf a                = Leaf a



instance  Arbitrary a => Arbitrary (BinTree a ) where
    arbitrary = sized tree'
     where tree' 0 = liftM leaf arbitrary
           tree' n | n>0 = 
                frequency [(1, liftM leaf arbitrary),
                  (4, liftM3 Node arbitrary subtree subtree)]
               where subtree = tree' (n `div` 2)

instance AlfaExp Nat where
  alfaExp n = expNat n

instance AlfaExp Int where
  alfaExp i = alfaExp (i2n i)


instance AlfaExp a => AlfaExp (BinTree a) where
   alfaExp (Leaf n) = EApp(ECon(Con "Leaf"))(alfaExp n)
   alfaExp (Node n l r) = EApp (EApp (EApp (ECon (Con "Node")) (alfaExp n)) (alfaExp l)) (alfaExp r)
 


expNat :: Nat -> Exp
expNat Zero = ECon (Con "zer")
expNat (Succ n) = EApp (ECon (Con "suc"))(expNat n)
expNat x    = EVar (Var "x")

{-
instance Arbitrary Stream where
   arbitrary = Gen(\n rnd -> Seq (fst(randomR (zero, i2n n) rnd)) (generate n (snd(randomR(zero, i2n n) rnd)) arbitrary))


instance AlfaExp Stream where
  alfaExp (Seq x xs) = EApp (EApp (ECon (Con "con"))(alfaExp x)) (alfaExp xs)
 
-}

{-
data Tree = Leaf Nat | Node Nat Tree Tree
         deriving (Show, Eq)
instance AlfaExp Tree where
   alfaExp (Leaf x) = EApp(ECon(Con "Leaf"))(alfaExp x)
   alfaExp (Node n l r) = EApp (EApp (EApp (ECon (Con "Node")) (alfaExp n)) (alfaExp l)) (alfaExp r)
 -}

tree :: Int -> StdGen -> BinTree Nat
tree 0 rnd = Leaf (generate maxSize rnd arbitrary)
tree n rnd = Node (generate maxSize rnd arbitrary) (tree (n-1) rnd1)(tree (n-1) rnd2)
    where
         (rnd1, rnd2) = split rnd

inf_tree :: StdGen -> BinTree Nat
inf_tree rnd = Node (generate maxSize rnd arbitrary) (inf_tree rnd1)(inf_tree rnd2)
    where
         (rnd1, rnd2) = split rnd


instance AlfaExp Digits where
  alfaExp I = ECon(Con "I")
  alfaExp Z = ECon(Con "Z")

instance AlfaExp Natural where
   alfaExp (Natural n) = alfaExp n

{-

data Seed = Leaf' Natural | Node' Natural Seed Seed
         deriving (Show, Eq)

instance AlfaExp Seed where
   alfaExp (Leaf' x) = EApp(ECon(Con "Leaf"))(alfaExp x)
   alfaExp (Node' n l r) = EApp (EApp (EApp (ECon (Con "Node")) (alfaExp n)) (alfaExp l)) (alfaExp r)
 
seed :: Int -> StdGen -> Seed
seed 0 rnd = Leaf' (generate maxSize rnd arbitrary)
seed n rnd = Node' (generate maxSize rnd arbitrary) (seed (n-1) rnd1)(seed (n-1) rnd2)
    where
         (rnd1, rnd2) = split rnd
-}
 
