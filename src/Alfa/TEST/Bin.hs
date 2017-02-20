module Bin where
import QuickCheck
import Monad
  
type Bin = [Bool]
d1 = False
d2 = True
 
inc dx = case dx of (nil    )-> one
                                 (con d x)->    if' d (D1:inc x) (D2: x)
dbl dx = case dx of (nil    )-> D0
                                 (con d x)-> D2:if' d (D1:    x) (dbl x)
dec dx = case dx of (nil    )-> D0
                                 (con d x)->    if' d (D1:    x) (dbl x)

(+) dx ey = case dx of
    (nil    )-> ey
    (con d x)-> case ey of
     (nil    )-> dx  
     (con e y)-> if' (d`and`e) (D2: x + inc y)
                (if' (d`or `e) (D1: x + inc y)
                               (D2: x + y))
  {-
  (*)(x,y::Bin)::Bin = let x2 = dbl x
                           f(d::Dig)(z::Bin)::Bin = dbl z + if' d x2 x
                       in  foldr Dig Bin f D0 y
  -}
  
(*) dx ey = case dx of
    (nil    )-> D0
    (con d x)-> case ey of
      (nil    )-> D0
      (con e y)-> if' (d`and`e) (D2:D1: x * inc y + y)
                 (if'  d        (D2: dx *  y + x)
                 (if'  e        (D2:  x * ey + y)
                                (D1: dx *  y + x)))

  -- begin not here
  
compareB(x,y::Bool)::Ordering = case x `iff` y of
    (true )-> EQ
    (false)-> case x of (true)-> GT; (false)-> LT
  -- end   not here

nothing     ::Maybe Bin = zer@_
just(x::Bin)::Maybe Bin = suc@_ x
mayB = maybe Bin
(-.)(dx,ey::Bin)::Maybe Bin = case ey of 
    (nil    )-> just dx
    (con e y)-> case dx of
      (nil    )-> nothing  
      (con d x)-> let 
          f(z::Bin)::Maybe Bin = case d `compareB` e of
            (LT)-> case z of (nil)-> nothing; (con _ _)-> just(dec(dbl z))
            (EQ)-> just(dbl z)
            (GT)-> just( D1:z)
        in mayB (Maybe Bin) nothing f (x -. y)

  (-)(x,y::Bin)::Bin = mayB Bin D0 (\(x_y::Bin) -> x_y) (x -. y) 

  compare(dx,ey::Bin)::Ordering = case dx of
    (nil    )-> case ey of (nil)-> EQ; (con _ _)-> LT
    (con d x)-> case ey of
      (nil    )-> GT
      (con e y)-> case x `compare` y of
                    (EQ)-> d `compareB` e; (LT)-> LT; (GT)-> GT

  QuotRem::Set = sig q::Bin; r::Bin

  -- this is bogus, but postulating it causes loops sometime.
  quotRem_by_0::QuotRem = struct q=D0; r=D0

  ---- this is actually exponential in size since let/open is call-by-name!
  --quotRem(dx,y::Bin)::QuotRem = case y of
  --  (nil    )-> quotRem_by_0
  --  (con _ _)-> case dx of
  --    (nil    )-> struct q=D0; r=D0
  --    (con d x)-> let
  --         open quotRem x y use q1=q,r1=r
  --         f(z::Bin)::QuotRem = case z==y of (true )->struct q=D2:D0; r=D0
  --                                           (false)->struct q=D1:D0; r=z
  --         it::QuotRem = case q1 of
  --           (nil    )-> mayB QuotRem (struct{q=D0; r=dx}) f (dx-.y)
  --           (con _ _)-> open quotRem (d:r1) y use q2=q, r2=r
  --                       in  struct q= dbl q1 + q2; r= r2
  --      in it

  {- uglier, but fun apps are really by lazy eval. -}
  fC(q1::Bin)(qr2::QuotRem)::QuotRem = struct q= dbl q1+qr2.q; r= qr2.r
  qR(y::Bin)::Bin->QuotRem = let
      mutual
        fA(dx::Bin)::QuotRem = case dx of
         (nil    )-> struct q = D0; r = D0
         (con d x)-> fB dx d x (fA x)  
        fB(dx::Bin)(d::Dig)(x::Bin)(qr1::QuotRem)::QuotRem = case qr1.q of
         (nil    )-> case dx -. y of
           (zer  )-> struct q = D0; r = dx  
           (suc z)-> case z == y of (true )->struct q= D2:D0; r= D0  
                                    (false)->struct q= D1:D0; r = z
         (con _ _)-> fC qr1.q (fA(d:qr1.r))  
      it::Bin->QuotRem = case y of (nil    )-> \ (_::Bin)-> quotRem_by_0
                                   (con _ _)-> fA
    in it
  quotRem(x,y::Bin)::QuotRem = qR y x

  quot(x,y::Bin)::Bin = (quotRem x y).q
  rem (x,y::Bin)::Bin = (quotRem x y).r
           

  n0 = D0
  n1 = D1:D0
  n2 = D2:D0
  n4 = dbl n2
  n8 = dbl n4
  n16= dbl n8
  n3 = D1:n1
  n5 = D1:n2
  n6 = D2:n2
  e8 = n16*n16
  e16= e8 *e8  -- 65536
  e32= e16*e16 -- 4294967296

  (!!)(x,y::Bin)::Bool = open quotRem x y use q,r in x == q*y+r
  foo :: {! !} = {! (e32+n5)!!n6!}
  


