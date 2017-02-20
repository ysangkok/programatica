{-+
Reusable functions for translation from the (non-recursive) P-Logic structure
to Isabelle.
-}

module PropStruct2Isabelle where
import IsabelleAST as S
import PropSyntaxStruct as P
import HsPropMaps
import PrettyPrint(pp)
import Maybe(fromMaybe)

-- translate property declarations
-- transPD :: forall pp pa i. (Printable (PD i pa pp)) =>
--   (i -> String) -> (pa -> Prop) -> (pp -> Pred)
--   -> PD i pa pp -> IsaDecl
transPD trId trPA trPP pd =
  case mapPD trId trPA trPP pd of
    HsPropDecl _ n ns p ->
      IsaPredDecl (PredDecl (S.PredAbs n (map getHSName ns)) p)
    HsAssertion _ (Just n) a ->
      IsaPropDecl (PropDecl n a)
    _ -> IsaComment (pp pd)
    {-
  where
    transPredParam (HsCon c) = PredParam c
    transPredParam (HsVar x) = TermParam x
    -}
    --predParam (HsVar x) = "Not supported: Predicate definition: values as parameters" -- !!!

-- translate property assertions
-- transPA :: forall pp pa t e i.
--   (i -> String) -> (e -> Term) -> (t -> Type) -> (pa -> Prop) -> (pp -> Pred)
--   -> PA i e t pa pp -> Prop
transPA trId trE trT trPA trPP pa =
  case mapPA trId trE trT trPA trPP pa of
    P.Quant P.All i optt pa -> propForall (i,optt) pa
    P.Quant P.Exist i optt pa -> propExists (i,optt) pa
    P.PropApp i ts [] -> PropVar i
    P.PropApp i ts es -> S.PropHas (map arg es) (PredWeak (PredVar i)) -- !!
    P.PropNeg a -> S.PropNeg a
    P.PropOp op a1 a2 -> propOp op a1 a2
    P.PropEqual e1 e2 -> S.PropEqual e1 e2
    P.PropHas e p -> S.PropHas [TermArg e] p
    P.PropParen a -> a
    -- _ -> not_supported "Assertion" pa
  where
    propOp op =
      case op of
        P.Conj -> S.PropConj
        P.Disj -> S.PropDisj
        P.Imp  -> S.PropImpl
        P.Equiv-> S.PropEquiv

    arg = either TermArg PredArg
    -- bad _ = not_supported "Predicate application" "predicates as arguments"

-- translate predicate formulas: PP i e p t pa pp -> Pred
-- transPP :: forall pp pa t p e i t2.
--   (i -> String) -> (e -> Term) -> (p -> Pattern) -> (t -> t2) -> (pa -> Prop) -> (pp -> Pred)
--   -> PP i e p t pa pp -> Pred
transPP trId trE trP trT trPA trPP pred =
  case mapPP trId trE trP trT trPA trPP pred of
    P.PredStrong (S.PredWeak p) -> S.PredStrong p
    P.PredParen p -> p
    p -> S.PredWeak $ case p of
      P.PredApp i ts [] -> S.PredVar i
      P.PredApp i ts ps -> S.PredCong i (map transPredArg ps) -- !!!
      P.PredArrow p1 p2 -> S.PredArrow p1 p2
      P.PredInfixApp p1 i p2 -> PredCong i (map PredArg [p1,p2])
      P.PredNeg optt p -> S.PredNeg p
      P.PredOp op optt p1 p2 -> predOp op p1 p2
      P.PredLfp i _ p -> S.PredLfp i p
      P.PredGfp i _ p -> S.PredGfp i p
      P.PredNil -> S.PredCong "Nil" []
      P.PredLifted e -> S.PredLifted e
      P.PredComp pts a -> S.PredComp (map fst pts) a
      -- _ -> not_supported "Predicate" pred
  where
    transPredArg = either TermArg PredArg
    --bad _ = not_supported "Predicate application" "values as arguments"

    predOp op =
      case op of
        P.Conj -> S.PredConj
        P.Disj -> S.PredDisj
        P.Imp  -> S.PredImpl
        P.Equiv-> S.PredEquiv

not_supported s x = error $ s++" not supported (yet): "++pp x
