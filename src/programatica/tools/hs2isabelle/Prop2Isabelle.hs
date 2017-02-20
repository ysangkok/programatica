{-+
Knot-tying definitions for the base+property syntax to Isabelle translation.
-}

module Prop2Isabelle where
import BaseStruct2Isabelle(transP,transD,transE,transT,transId,transC,transQ)
import PropStruct2Isabelle(transPD,transPA,transPP)
import PropSyntax
import IsabelleAST
import PrettyPrint(Printable)


combine :: IsaDecl -> IsaDecl -> IsaDecl
combine (IsaDomain xs) (IsaDomain ys) = IsaDomain (xs ++ ys)
combine (IsaFixrec xs) (IsaFixrec ys) = IsaFixrec (xs ++ ys)
combine _ _ = error "Illegal mutual recursion"

combineDecls :: [IsaDecl] -> IsaDecl
combineDecls = foldr1 combine

bind2sig :: [(String,Type)] -> HBind -> TypeSig
bind2sig table b = TypeSig n (case lookup n table of Just t -> t)
  where n = head_of_HBind b

addConstsDecls :: [(String,Type)] -> [IsaDecl] -> [IsaDecl]
addConstsDecls table [] = []
addConstsDecls table (x:xs) =
  case x of
    IsaClass c t bs -> IsaClass c t [] : consts bs : rest
    IsaInstance t bs -> IsaInstance t [] : IsaFixrec bs : rest
    IsaFixrec bs -> consts bs : x : rest
    _            -> x : rest
  where
    rest = addConstsDecls table xs
    consts bs = IsaConsts (map (bind2sig table) bs)

------------------------------------------------------------

-- transPat :: forall i. (Printable i, Printable (PI i (HsPatI i))) =>
--   HsPatI i -> Term
transPat (Pat p) = transP transId transPat p

-- transDecs :: forall i.
--   (Printable (DI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i] (HsTypeI i)),
--    Printable (PD i (AssertionI i) (PredicateI i)),
--    Printable i,
--    Printable (TI i (HsTypeI i)),
--    Printable (EI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i]),
--    Printable (PI i (HsPatI i))) =>
--   [HsDeclI i] -> [IsaDecl]
transDecs ds = map transDec ds

-- transDec :: forall i.
--   (Printable (DI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i] (HsTypeI i)),
--    Printable (PD i (AssertionI i) (PredicateI i)),
--    Printable i,
--    Printable (TI i (HsTypeI i)),
--    Printable (EI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i]),
--    Printable (PI i (HsPatI i))) =>
--   HsDeclI i -> IsaDecl
transDec (Dec d) =
    prop (transD transId transExp transPat transLocalDecs transType transContext transType)
         (transPD transId transAssertion transPredicate)
         d

-- transExp :: forall i.
--   (Printable (TI i (HsTypeI i)),
--    Printable i,
--    Printable (PI i (HsPatI i)),
--    Printable (EI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i])) =>
--   HsExpI i -> Term
transExp (Exp e) =
    transE transId transExp transPat transLocalDecs transType transContext e

-- transAssertion :: forall i.
--   (Printable i,
--    Printable (TI i (HsTypeI i)),
--    Printable (EI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i]),
--    Printable (PI i (HsPatI i))) =>
--   AssertionI i -> Prop
transAssertion (PA a) =
    transPA transId transExp transQType transAssertion transPredicate
            a

-- transPredicate :: forall i.
--   (Printable i,
--    Printable (TI i (HsTypeI i)),
--    Printable (EI i (HsExpI i) (HsPatI i) [HsDeclI i] (HsTypeI i) [HsTypeI i]),
--    Printable (PI i (HsPatI i))) =>
--   PredicateI i -> Pred
transPredicate (PP p) =
    transPP transId transExp transPat transQType transAssertion transPredicate
            p
-- p :: PP i (HsExpI i) (HsPatI i) (HsQualTypeI i) (AssertionI i) (PredicateI i)
{- transPP :: forall pp1 pa1 t1 p1 e1 i1 t2.
           (i1 -> String)
           -> (e1 -> Term)
              -> (p1 -> Pattern)
                 -> (t1 -> t2)
                    -> (pa1 -> IsabelleProp.Prop)
                       -> (pp1 -> Pred) -> PP i1 e1 p1 t1 pa1 pp1 -> Pred -}

-- transType :: forall i.
--   (Printable i, Printable (TI i (HsTypeI i))) =>
--   HsTypeI i -> Type
transType (Typ t) = transT transId transType t

-- transContext :: (Printable i, Printable (TI i (HsTypeI i))) => [HsTypeI i] -> [(String, Class)]
transContext ts = transC transType ts

-- transQType :: forall i c.
--   (Printable (TI i (HsTypeI i)), Printable i) =>
--   Q c (HsTypeI i) -> Type
--   HsQualTypeI i -> Type  (c = [HsTypeI i])
transQType qt = transQ transContext transType qt

--transLocalDecs ds = error "transLocalDecs"
transLocalDecs ds = concatMap transLocalDec ds

transLocalDec d =
  case transDec d of
    IsaFixrec bs -> bs
    IsaTypeSig is t -> [HBindSig i t | i <- is]
    _ -> [] -- silently ignores unimplemented things!!!


