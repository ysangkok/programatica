module Bdt (Bdd(..), taut, faus, bdt) where 
  import BoolAlgebra
  import GenNat(Nat(..))
  -- tautology checker, thanks to Thierry Coquand

  data Bdd = O | I | Pair Bdd  Bdd 
                    deriving (Eq, Show)

  (/\) :: Bdd -> Bdd -> Bdd 
  (/\) b d = Pair b d 

  mkt:: Bdd -> Bdd -> Bdd  
  mkt O O     = O
  mkt I I = I
  mkt h1 h2 = h1 /\ h2

  neg:: Bdd -> Bdd 
  neg O = I
  neg I = O
  neg (Pair b d) = neg b/\ neg d

  and_bdd ::  Bdd -> Bdd -> Bdd 
  and_bdd O h' = O
  and_bdd I h' = h'
  and_bdd h O = O
  and_bdd h I = h
  and_bdd (Pair b d)(Pair b' d') = mkt (and_bdd b b') (and_bdd d d')

  next:: Bdd -> Bdd 
  next h =  (h/\ h)
  var :: Nat -> Bdd  
  var Zero = (I/\ O)
  var (Succ n) = next (var n)

  bdt :: BoolExpr -> Bdd 
  bdt (Val True) = I
  bdt (Val False) = O
  bdt (Var n) = var n
  bdt (Not t) = neg (bdt t)
  bdt (And t1 t2) = and_bdd (bdt t1)(bdt t2)
  bdt (Or t1 t2) = bdt (Not (And(Not t1)(Not t2)))
  bdt (Imply t1 t2) = bdt (Or (Not t1) t2)
  bdt (BiImply t1 t2) = bdt (And (Imply t1 t2)(Imply t2 t1))


  taut t = bdt t == I
  faus t = bdt t == O

