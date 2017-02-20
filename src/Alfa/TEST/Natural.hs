module Natural where
import QuickCheck2
import Monad

data  Digits  =  I | Z
   deriving Eq

type  Binary  = [Digits]
   
data  Natural = Natural Binary
     deriving Eq
instance Show Digits where
   show I = "1"
   show Z = "2"

instance Show Natural where
  show (Natural n) = show (b2i' n)


b2i :: Natural -> Int
b2i (Natural xs) = b2i' xs

b2i'  [] = 0
b2i'  (I:xs) =2* (b2i' xs) + 1
b2i' (Z:xs) = 2*(b2i' xs) + 2
toN :: (Integral a) => a -> Binary
toN n | n < 0   = error$ "toN" ++ show n
      | n == 0  = []
      | odd n   = I: toN ((n-1) `div` 2)
      | True    = Z: toN ((n-2) `div` 2)



instance Arbitrary Digits where
   arbitrary = elements [I,Z]

instance Arbitrary Natural where
   arbitrary = resize 5 (fmap Natural arbitrary)

infixl 6 `plus`
infixl 7 `mult`
infixl 6 `monus`
infixl 8 <^>
instance Num Natural where
--  fromInt                    = Natural . toN
  fromInteger                = Natural . toN
  (Natural h) + (Natural h') = Natural (plus h h')
  (Natural h) - (Natural h') = Natural (monus h h')
  (Natural h) * (Natural h') = Natural (mult h h')

(<^>) (Natural h)(Natural h') = Natural (pow h h')


mod2 (Natural h)(Natural h') = Natural (mod2' h h')
nat0  = []
suc bs = case bs of {
                                      ([]) -> (:) I [];
                                      (x: xs) -> case x of {
                                                      (I) -> (:) Z xs;
                                                      (Z) -> (:) I (suc xs);};}
nat1  = (:) I []
nat2  = (:) Z []

d2b d = case d of {(I) -> nat0; (Z) -> nat1}

eqDig b b' = case b of {
                                 (I) -> case b' of {
                                          (I) -> True;
                                          (Z) -> False;};
                                 (Z) -> case b' of {
                                          (I) -> False;
                                          (Z) -> True;};}

--eqBinary m n = eqList Digits eqDig m n

add1 b bs = case b of {
                                           (I) -> suc bs;
                                           (Z) -> suc (suc bs);}

plus h h' = case h of {
                                                                         ([]) -> h';
                                                                         (x: xs) -> case h' of {
                                                                                         ([]) -> h;
                                                                                         (x': xs') -> case x of {
                                                                                                           (I) -> case x' of {
                                                                                                                       (I) -> (:) Z (xs  `plus`  xs');
                                                                                                                       (Z) -> (:) I (suc (xs  `plus`  xs'));};
                                                                                                           (Z) -> case x' of {
                                                                                                                        (I) -> (:) I (suc (xs  `plus`  xs'));
                                                                                                                        (Z) -> (:) Z (suc (xs  `plus`  xs'));};};};}


pre a = case a of {
                              ([]) -> nat0;
                              (x: xs) -> case x of {
                                              (I) -> twice xs ;
                                              (Z) -> (:) I xs;};}  
 
mult h h' = case h of {
                                                                         ([]) -> nat0;
                                                                         (x: xs) -> case h' of {
                                                                                         ([]) -> nat0;
                                                                                         (x': xs') -> case x of {
                                                                                                           (I) -> (:) x' (xs `mult` h'  `plus`  xs');
                                                                                                           (Z) -> (:) Z (xs `mult` h'  `plus`  pre h');};};}


isPos h = 
      case h of {
        ([]) -> nat0;
        (x: xs) -> suc nat0;}

twice h  = case h of {
                               ([]) -> nat0;
                               (x: xs) -> case x of {
                                               (I) -> (:) Z (twice xs);
                                               (Z) -> (:) Z ((:) I xs);};}

pow h h' = case h' of {
                                                                         ([]) -> nat1;
                                                                         (x: xs) -> case x of {
      (I) -> h `pow` (twice xs) `mult` h;
      (Z) -> h `pow` (twice xs) `mult` (h `mult` h);};}



monus h h' = case h of {
                         ([]) -> nat0;
                         (x: xs) -> case h' of {
                                         ([]) -> h;
                                         (x': xs') -> case x of {
                                                           (I) -> case x' of {
                                                                       (I) -> twice (monus xs xs');
                                                                       (Z) -> pre (twice (monus xs xs'));};
                                                           (Z) -> case x' of {
                                                                        (I) -> (:) I (monus xs xs');
                                                                        (Z) -> twice (monus xs xs');};};};} 
  



mod2' h h' =  case h of {
                         ([]) -> nat0;
                         (x: xs) -> 
                   let r = suc(mod2' (pre h) h')
                   in  mult (isPos (h' `monus` r)) r;} 
 

nat3  = nat1  `plus`  nat2
nat5  = nat2 `mult` nat2  `plus`  nat1 
nat10  = nat5  `plus`  nat5 -- nat2`pow`nat3  `plus`  nat2
big  = (nat10 `mult` nat10)  `plus` ( nat2 `mult` nat10)  `plus`  (nat2`mult`nat2)
big' = nat2 `pow` nat10
n32 = nat2`pow`nat5
n10 = Natural nat10
n3 = Natural nat3
n2 = Natural nat2


{-
propPlus a b =  collect (a, b2i a, b, b2i b) $   b2i b < 10000 && b2i b >0  ==>
    b2i( a `mod2` b) == (b2i a) `mod` (b2i b)
     where types = (a::Natural, b::Natural)
-}

n23 = n10 + n10 + n3
n19 = n10  + n3 * n3
