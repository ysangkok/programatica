module AbstractOps where
import UAbstract
import Utils2(apFst)

-- Printing:
printImport (Import filename) ="--#include \""++filename++"\""

-- Predicates:

isImportDecl (ImportDecl _) = True
isImportDecl _ = False

-- Special variable names:
noname = Var "_" -- hmm !!
ndgoalVar = Var "ndgoal"
itVar = Var "it"

-- The definition   "f   (x:A) :  B = e"
-- is equivalent to  f : (x:A) -> B = \ x -> e.
-- Convert with declArgsToLhs and declArgstoRhs.

declArgsToLhs = mapDeclB defArgsToLhs
declArgsToRhs = mapDeclB defArgsToRhs
changeDeclDetail = mapDecl . changeDefDetail

defArgsToLhs d =
    case d of
      Value (name,d@(ctx,e,t)) ->
        case stripAnnots e of
	  ESum sum -> Data (name,(ctx,sum)) -- t?
	  _ -> Value (name,mvArgs d)
      _ -> d -- !!
  where
    mvArgs d@(Ctx l ctx,e,t) =
      -- Annotations will be lots!
      case (stripAnnots e,stripAnnots t) of
	(EAbs (x:- _) e',EPi (y:-t) t') | x==y ->
            mvArgs (Ctx l (ctx++[(y,t)]),e',t')
	_ -> d

defArgsToRhs d =
  case d of
    Value (name,(ctx@(Ctx l ps),e,t)) ->
      Value (name,(Ctx l [],absExp (ctx2typings ctx) e,piExp ps t))
    Data (name,(ctx,sum)) ->
      Value (name,(ctx,ESum sum,ESort (Sort "Set")))
    _ -> d -- !!

defAddTypeSign d =
  case d of
    Binding (name,e) -> Value (name,(emptyCtx,e,typeOfAbs e))
    _ -> d

defRmTypeSign d =
  case d of
    Value _ ->
      case defArgsToRhs d of
        Value (name,(Ctx _ [],e,t)) -> Binding (name,e)
	_ -> d
    _ -> d

class ChangeProps a where
  changeProps :: (Props->Props) -> a -> a
  addTypeSign,rmTypeSign :: a -> a

instance ChangeProps a => ChangeProps [a] where
  changeProps = map . changeProps
  rmTypeSign = map rmTypeSign
  addTypeSign = map addTypeSign

instance ChangeProps Def where
  changeProps = changeDefProps
  addTypeSign = mapDefB defAddTypeSign
  rmTypeSign = mapDefB defRmTypeSign

instance ChangeProps Decl where
  changeProps = mapDecl . changeProps
  addTypeSign = mapDecl addTypeSign
  rmTypeSign = mapDecl rmTypeSign

instance ChangeProps OpenArg where
   changeProps f (OpenArg ps n t n') = OpenArg (f ps) n t n'
   addTypeSign (OpenArg ps n Nothing n') = OpenArg ps n (Just ePreMetaVar) n'
   addTypeSign oa = oa
   rmTypeSign (OpenArg ps n _ n') = OpenArg ps n Nothing n'

changeDefDetail d (DefA (p,ps,_) def) = DefA (p,ps,d) def
changeDefProps f (DefA (p,ps,d)  def) = DefA (p,f ps,d) def

mapDeclB = mapDecl . mapDefB

mapDecl f (Decl c defs) = Decl c (map f defs)
mapDecl _ d = d

mapModuleDecls f (Module decls) = Module (map f decls)
--mapModuleDecls f (Module decls) = Module (f decls) -- this is more general...

flatApp e1 e2 = flat' e1 [e2]
  where flat' (EApp f a) as = flat' f (a:as)
	flat' (EAnnot _ e) as = flat' e as
	flat' e as = (e,as)

flatApp' (EApp e1 e2) = flatApp e1 e2
flatApp' (EAnnot _ e) = flatApp' e
flatApp' e = (e,[])

--appc (ECon c as) es = ECon c (as++es)
--appc (EApp e1 e2) es = appc e1 (e2:es)
appc e es = app e es

flatPi (EPi (x:-t) e) | x/=noname = apFst ((x,t):) (flatPi e)
flatPi (EAnnot _ e) = flatPi e
flatPi e = ([],e)

flatFun (EPi (_:-t) e) = apFst (t:) (flatFun e)
flatFun e = ([],e)

flatAbs (EAbs n e) = apFst (n:) (flatAbs e)
flatAbs (EAnnot _ e) = flatAbs e
flatAbs e = ([],e)

typeOfAbs (EAbs t e) = EPi t (typeOfAbs e)
typeOfAbs (EAnnot _ e) = typeOfAbs e
typeOfAbs _ = ePreMetaVar

---

specialLet ds e =
  case (ds,e) of
    ([Decl _ [DefA _ (Value (i1,(emptyCtx,e',t)))]], EVar i2) ->
      if i1==i2
      then if i1==ndgoalVar
           then EProofOf t e'
	   else if i1==itVar
	        then ETyped e' t
		else ELet ds e
      else ELet ds e
    _ -> ELet ds e

{- -- old:
isNDproof = isTrivialLet' [ndgoalVar]
isTrivialLet = isTrivialLet' [itVar,ndgoalVar]
isTrivialLet' names e =
  case e of
    ELet [Decl _ [DefA _ (Value (i1,([],e,t)))]] (EVar i2)
      -> i1==i2 && i1 `elem` names
    _ -> False
-}

trivialLet = trivialLet' itVar
ndStyleLet = trivialLet' ndgoalVar
trivialLet' var t e = ELet [decl' [defA (Value (var,(emptyCtx,e,t)))]] (EVar var)

--- Some functions for expressions containing annotations:

stripAnnots (EAnnot a e) = stripAnnots e
stripAnnots e = e

expAnnots (EAnnot a e) = a:expAnnots e
expAnnots _ = []

splitAnnots e = (expAnnots e,stripAnnots e)

attachAnnots a e = foldr EAnnot e a

flatAbs' e =
  case splitAnnots e of
    (a,EAbs x e) -> apFst ((a,x):) (flatAbs' e)
    (a,_) -> ([],e)

absExp' (a:as) (x:xs) e = attachAnnots a (EAbs x (absExp' as xs e))
absExp' _ _ e = e

flatPi' e =
  case splitAnnots e of
    (a,EPi (x:-t) e) | x/=noname -> apFst ((a,(x,t)):) (flatPi' e)
    (a,_) -> ([],e)

piExp' (a:as) ((n,t):nts) e = attachAnnots a (EPi (n:-t) (piExp' as nts e))
piExp' _ _ e = e

-- independent function type, t -> t
eFun e1 e2 = EPi (noname:-e1) e2

eFuns [] e = e
eFuns (a:as) e = eFun a (eFuns as e)

eSet = ESort (Sort "Set")
eType = ESort (Sort "Type")
