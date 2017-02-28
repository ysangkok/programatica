module SubstituteBaseStruct where
import Substitute (MapExp, Subst, mapExp, esubst, subst)
import Recursive(rec, Rec)
import HasBaseStruct (hsApp, HasBaseStruct)
import HsDecl (HsMatchI, DI)
import BaseSyntax (mapDecls, mapEI, HsModuleI, EI, HsIdentI( HsVar ), EI(HsId, HsRightSection, HsInfixApp, HsLeftSection), mapDI, mapMatchI)

substE :: (Rec e0 (EI i1 e0 p20 d20 t20 c20), HasBaseStruct.HasBaseStruct e0 (EI i0 e0 p0 ds0 t0 c0), Subst i1 e0, MapExp e0 d20) => (i1 -> e0) -> EI i1 e0 p20 d20 t20 c20 -> e0
substE = substE' rec

substE' rec s e0 =
    case mapEI id (subst s) id (esubst s) id id e0 of
      HsId (HsVar x) -> s x
      HsInfixApp e1 (HsVar x) e2 -> s x `hsApp` e1 `hsApp` e2
      HsLeftSection e (HsVar x) -> s x `hsApp` e
      HsRightSection (HsVar x) e -> error "SubstituteBaseStruct.subst HsRightSection"
      e -> rec e

instance MapExp e ds => MapExp e (DI i e p ds t c tp) where mapExp = mapExpD
instance MapExp e ds => MapExp e (EI i e p ds t c)    where mapExp = mapExpE

mapExpD f = mapDI id f id (mapExp f) id id id
mapExpE f = mapEI id f id (mapExp f) id id

instance MapExp e ds => MapExp e (HsMatchI i e p ds) where
  mapExp f = mapMatchI id f id (mapExp f)

instance MapExp e ds => MapExp e (HsModuleI m i ds) where
  mapExp = mapDecls . mapExp


