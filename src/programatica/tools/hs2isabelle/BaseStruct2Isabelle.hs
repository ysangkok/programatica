{-+
Reusable functions for translation from the (non-recursive) base structure
to Isabelle.
-}

module BaseStruct2Isabelle where
import IsabelleAST
import BaseSyntax -- non-recursive base syntax structure
import PrettyPrint(Printable, pp)
import PropSyntaxStruct(Q((:=>)))
import Char(isAlpha)

-- translate identifiers
transId :: Printable i => i -> String
transId x = pp x
{-
transId x = 
  case orig x of
    G m n _ -> pp m++"."++pp n
    _ -> pp x
-}

-- translate types
-- transT :: forall t i. (Printable (TI i t)) =>
--   (i -> String) -> (t -> Type) -> TI i t -> Type
transT trId trT t =
  case mapTI trId trT t of
    HsTyFun t1 t2 -> tFun t1 t2
    HsTyApp t1 t2 -> tApp t1 t2
    HsTyVar a -> tVar a
    HsTyCon c -> TType c []
    _ -> not_supported "Type" t

-- translate literals
transL :: HsLiteral -> Term
transL lit =
  case lit of
    HsInt   i  -> HInt i
    HsChar  c  -> not_supported "character literal" lit
    HsString s -> HStr s
    HsFrac   x -> not_supported "fraction literal" lit

-- translate patterns
-- transP :: forall p i. (Printable (PI i p)) =>
--   (i -> String) -> (p -> Term) -> PI i p -> Term
transP trId trP p =
  case mapPI trId trP p of
    HsPId (HsVar x) -> HVar x
    HsPId (HsCon c) -> HVar c
    HsPLit _ lit -> transL lit -- new
--  HsPNeg _ lit -> ...
--  HsPSucc _ n l -> ...
    HsPInfixApp x op y -> HInfix x op y
    HsPApp c ps -> hApps (HVar c) ps
    HsPTuple s ps -> HTuple ps
    HsPList s ps -> HList ps
    HsPParen p -> HParen p
    HsPRec i fs -> HRecord (HVar i) (map transField fs)
    HsPAsPat x p -> HAsPat x p
    HsPWildCard -> HWildPat
    HsPIrrPat p -> HLazyPat p
    _ -> not_supported "Pattern" p
  where
    transField (HsField i e) = HField i e

-- translate expressions
-- transE :: forall c t d p e i c2. (Printable (EI i e p d t c)) =>
--   (i -> String) -> (e -> Term) -> (p -> Term) -> (d -> [HBind])
--   -> (t -> Type) -> (c -> c2)
--   -> EI i e p d t c -> Term
transE trId trE trP trDs trT trC e =
  case mapEI trId trE trP trDs trT trC e of 
    HsId (HsVar x)              -> HVar x
    HsId (HsCon c)              -> HVar c
    HsApp x y                   -> HApp x y
    HsLit _ lit                 -> transL lit -- new
    HsInfixApp x (HsVar op) z   -> HInfix x op z
    HsInfixApp x (HsCon c) z    -> HInfix x c z
    -- HsNegApp _ x                -> HVar "Prelude.negate" `HApp` x
    HsLambda ps e               -> hAbss ps e
    HsLet ds e                  -> HLet ds e
    HsIf x y z                  -> HIte x y z
    HsCase e alts               -> HCase e (map transAlt alts)
    HsDo stmts                  -> HDo (transStmts stmts)
    HsTuple xs                  -> HTuple xs
    HsList xs                   -> HList xs
    HsParen x                   -> x
    HsLeftSection x (HsVar op)  -> HLSection x op 
    HsRightSection (HsVar op) y -> HRSection op y
    HsLeftSection x (HsCon c)   -> HLSection x c
    HsRightSection (HsCon c) y  -> HRSection c y
    HsRecConstr _ i fs          -> HRecord (HVar i) (map transField fs)
    HsRecUpdate _ e fs          -> HRecord e (map transField fs)
    -- The following removed by the type checker too...
    HsEnumFrom e1
      -> HVar "enumFrom" `HApp` e1
    HsEnumFromTo e1 e2
      -> HVar "enumFromTo" `HApp` e1 `HApp` e2
    HsEnumFromThen e1 e2
      -> HVar "enumFromThen" `HApp` e1 `HApp` e2
    HsEnumFromThenTo e1 e2 e3
      -> HVar "enumFromThenTo" `HApp` e1 `HApp` e2 `HApp` e3
    HsExpTypeSig _ e c t        -> HConstrain e t -- !! what about context?
    _ -> HVar (not_supported_msg "Expression" e) -- !!
  where
    -- translate case alternatives
    transAlt alt =
      case alt of
	HsAlt _ pat (HsBody rhs) _ -> HAlt pat rhs -- !!!
	 --_ -> not_supported "Case branch" "'where' clauses"
      --where
	--transRhs (HsBody e)       = e -- [NonGuarded e]
	--transRhs (HsGuard gdrhss) = undefined -- [Guarded g e|(_,g,e)<-gdrhss]
    transStmts stmts =
      case stmts of
        HsGenerator _ p e s -> HGenerator p e (transStmts s)
        HsQualifier e s -> HQualifier e (transStmts s)
        HsLetStmt ds s -> HLetStmt ds (transStmts s)
        HsLast e -> HLast e
    transField (HsField i e) = HField i e

-- translate declarations
-- transD :: forall tp c t d p e i c2 d2. (Printable (DI i e p d t c tp)) =>
--   (i -> [Char]) -> (e -> Term) -> (p -> Pattern) -> (d -> d2) -> (t -> Type)
--   -> (c -> c2) -> (tp -> TypePattern)
--   -> DI i e p d t c tp -> IsaDecl
transD trId trE trP trDs trT trC trTp d =
  case mapDI trId trE trP trDs trT trC trTp d of
    HsPatBind _ p rhs ds ->
      IsaFixrec [HBindPat (HMatch p (transRhs rhs ds))]
    HsFunBind _ ms ->
      IsaFixrec [HBindFun (map transMatch ms)]
    HsTypeDecl _ tp t ->
      IsaTypes (TypePattern tp) t
    HsDataDecl _ c tp cons ds ->
      IsaDomain [DomainType (TypePattern tp) (map transCon cons)]
    HsNewTypeDecl _ c tp con ds ->
      IsaDomain [DomainType (TypePattern tp) [transCon con]]
    HsInfixDecl _ f is ->
      IsaInfix (transFixity f) (map transIdentI is)
    HsClassDecl _ [] tp _ ds ->
      IsaClass (snd (transC1 tp)) DefaultSort ds
    HsClassDecl _ c tp _ ds ->
      IsaClass (snd (transC1 tp)) (Sort (map snd c)) ds
    HsInstDecl _ i' c t ds ->
      IsaInstance (addContext c t) ds
    HsTypeSig _ is c t ->
      IsaTypeSig is (addContext c t)
    {-
    HsClassDecl SrcLoc c tp (HsFunDeps i) ds |
    HsInstDecl SrcLoc (Maybe i) c t ds |
    HsDefaultDecl SrcLoc [t] |
    HsTypeSig SrcLoc [i] c t |
    HsInfixDecl SrcLoc HsFixity [HsIdentI i] |
    HsPrimitiveTypeDecl SrcLoc c tp |
    HsPrimitiveBind SrcLoc i t
    -}
    _ -> IsaComment (pp d)
  where
    transMatch (HsMatch _ i ps rhs ds) = if isAlpha (head i)
      then HMatch (hApps (HVar i) ps) (transRhs rhs ds)
      else case ps of [x,y] -> HMatch (HInfix x i y) (transRhs rhs ds)

    transRhs (HsBody e) [] = e
    transRhs (HsBody e) ds = HLet ds e
    transRhs (HsGuard ((_, _, e):_)) ds = transRhs (HsBody e) ds  --- FIXME

    transCon con =
      case con of
        HsConDecl loc _ _ c args -> DomainCons c (map transConArg args)
        HsRecDecl loc _ _ c args -> DomainCons c (concatMap transRecArg args)

    transConArg arg =
      case arg of
        HsBangedType t -> DomainArg Strict "" t
        HsUnBangedType t -> DomainArg Lazy "" t
    
    transRecArg (names, arg) =
      case arg of
        HsBangedType t -> [DomainArg Strict n t | n <- names]
        HsUnBangedType t -> [DomainArg Lazy n t | n <- names]

    transFixity (HsFixity a n) = transAssoc a ++ show n

    transAssoc a = case a of
      HsAssocNone -> ""
      HsAssocLeft -> "l"
      HsAssocRight -> "r"

    transIdentI (HsVar i) = i
    transIdentI (HsCon i) = i

transC1 :: Type -> (String, Class)
transC1 (TType c [TVar a _]) = (a, c)

-- translate contexts
-- transC :: forall a. (a -> Type) -> [a] -> [(String, Class)]
transC trT c = map (transC1 . trT) c

-- translate qualified types
-- transQ :: forall t c. (c -> [(String, Class)]) -> (t -> Type) -> Q c t -> Type
transQ trC trT (c:=>t) = addContext (trC c) (trT t)

not_supported s x = error $ not_supported_msg s x
not_supported_msg s x = s++" not supported (yet): "++pp x
