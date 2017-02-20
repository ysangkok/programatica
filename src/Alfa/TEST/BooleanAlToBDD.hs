module BooleanAlToBDD 
   ( expisBDD1
   , ex2cm
   ) where

import UAbstract 
import BddTable (isBddOne)
import GenNat
import qualified BoolAlgebra as BE --(BoolExpr(..)) as BE
import CModel (cm)

isBDD1 :: BE.BoolExpr -> Bool 
isBDD1 e = isBddOne e

  
exp2BE :: Exp -> Either String BE.BoolExpr


exp2BE (EApp (ECon (Con "Var")) e2)  = 
  either (\ a -> Left a) (\ b -> Right $ BE.Var b) (exp2Nat e2) 

exp2BE (EApp (ECon (Con "Val")) e2)  = 
  either (\ a -> Left a) (\ b -> Right $ BE.Val b) (exp2Bool e2) 

exp2BE (EApp (ECon (Con "Not")) e2)  = 
   either (\ a -> Left a) (\ b -> Right $ BE.Not b) (exp2BE e2) 
exp2BE (EApp (EApp (ECon (Con "And")) e1) e2)  =
   either (\a -> Left a) (\ b -> either (\x -> Left x) (\ y -> Right $ BE.And b y)  (exp2BE e2)) (exp2BE e1)
exp2BE (EApp (EApp (ECon (Con "Or")) e1) e2)  =
   either (\a -> Left a) (\ b -> either (\x -> Left x) (\ y -> Right $ BE.Or b y)  (exp2BE e2)) (exp2BE e1)
exp2BE (EApp (EApp (ECon (Con "Imply")) e1) e2)  =
   either (\a -> Left a) (\ b -> either (\x -> Left x) (\ y -> Right $ BE.Imply b y)  (exp2BE e2)) (exp2BE e1)

exp2BE (EApp (EApp (ECon (Con "BiImply")) e1) e2)  =
   either (\a -> Left a) (\ b -> either (\x -> Left x) (\ y -> Right $ BE.BiImply b y)  (exp2BE e2)) (exp2BE e1)


exp2BE (EAnnot _ e)        = exp2BE e
exp2BE _    = Left "This is not a closed Boolean expression"


exp2Bool (ECon (Con "true")) =  Right True
exp2Bool (ECon (Con "false")) = Right False
exp2Bool  _ = Left "Not a Boolean"


exp2Nat :: Exp -> Either String Nat

exp2Nat (ECon (Con "zer")) = Right Zero
exp2Nat (EApp (ECon (Con "suc")) e) =
  either (\a -> Left a) (\b -> Right $ Succ b) (exp2Nat e) 
exp2Nat (EAnnot _ e)   = exp2Nat e
exp2Nat _ = Left "Not a nat"


expisBDD1 :: Exp -> Bool
expisBDD1 e = 
   case exp2BE e of
      Left str -> False --Left str
      Right e' ->  isBDD1 e' --Right $ isBDD1 e'


ex2cm ::Exp -> String

ex2cm e = 
  case exp2BE e of
    Left str -> str
    Right e' -> show $ cm e'  

