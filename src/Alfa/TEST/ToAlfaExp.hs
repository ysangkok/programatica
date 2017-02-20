module ToAlfaExp where
import UAbstract

class AlfaExp a where
  alfaExp :: a -> Exp

instance AlfaExp a => AlfaExp [a] where
   alfaExp [] = ECon (Con "nil")
   alfaExp (x:xs) =  EApp (EApp (ECon (Con "con"))(alfaExp x)) (alfaExp xs)
 
instance AlfaExp Bool where
   alfaExp True = ECon (Con "true")
   alfaExp False = ECon (Con "false")

instance AlfaExp () where
  alfaExp () = ECon (Con "tt")

