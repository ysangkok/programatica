module SubstMeta(SubstMeta(..),MetaSubst,recordSubstMeta) where
--import AlfAbstract
import UAbstract
import ListUtil(assoc)

type MetaSubst = [(MetaVar,Exp)]
type SubstM syntax = MetaSubst -> syntax -> syntax

class SubstMeta syntax where
  substMeta :: SubstM syntax

s ms = substMeta ms -- used locally for conciseness

recordSubstMeta x = substMeta . recordMeta $ x

recordMeta = map r
  where r (m,e) = (m,EAnnot (was m) e)

instance SubstMeta Exp where
  substMeta ms e =
      case e of
	EApp e1 e2 -> EApp (s e1) (s e2)
	EVar _ -> e
	ECon c    -> eCon0 c
	EAbs nt e -> EAbs (s nt) (s e)
	ELet ds e -> ELet (s ds) (s e)
	ETyped e t -> ETyped (s e) (s t)
	EProofOf t e -> EProofOf (s t) (s e)
	ECase e bs -> ECase (s e) (s bs)
	EPi nt e2 -> EPi (s nt) (s e2)
	ESort sort -> e
	ESum sum -> ESum (s sum)
	EProj e n -> EProj (s e) n
	EOpen e1 oargs e2 -> EOpen (s e1) (s oargs) (s e2)
	ESig sig -> ESig (s sig)
	EStr str -> EStr (s str)
	EMeta meta -> assoc s e ms meta
	EAnnot a e -> EAnnot a (s e)
	EChar _ -> e
	EString _ -> e
	EInt _ -> e
	ERat _ -> e
	-- _ -> error ("substMeta "++show e)
    where
      s x = substMeta ms x

instance SubstMeta Branch where
  substMeta ms (Branch b) = Branch (s ms b)

instance SubstMeta Decl where
  substMeta ms (Decl c defs) = Decl c (s ms defs)
  substMeta ms d@(Comment _) = d
  substMeta ms d@(ImportDecl _) = d

instance SubstMeta Def where
  substMeta = mapDefB . s

instance SubstMeta DefB where
  substMeta ms = defb
    where 
      s x = substMeta ms x
      defb d =
	case d of
	  CommentDef _ -> d
	  Value d -> Value (s d)
	  Binding d -> Binding (s d)
	  Package d -> Package (s d)
	  Open d -> Open (s d)
	  Data d -> Data (s d)
	  Type d  -> Type (s d)
	  Axiom d -> Axiom (s d)

instance SubstMeta SigPart where
  substMeta ms (SigField l e) = SigField l (s ms e)
  substMeta ms (SigDef defB)  = SigDef (s ms defB)

instance SubstMeta Context where
  substMeta ms (Ctx l ps) = Ctx l (s ms ps)

instance SubstMeta Constructor where
  substMeta ms (Constr c) =  Constr (s ms c)

instance SubstMeta Typing where
  substMeta ms (n:-e) = n:-s ms e

instance SubstMeta PackageBody where
  substMeta ms b =
    case b of
      PackageDef ds -> PackageDef (s ms ds)
      PackageInst e -> PackageInst (s ms e)

instance SubstMeta Var where substMeta ms = id
instance SubstMeta Con where substMeta ms = id

instance SubstMeta OpenArg where
  substMeta ms (OpenArg ps x optt opty) = OpenArg ps x (s ms optt) opty

---

instance SubstMeta x => SubstMeta [x] where
  substMeta = map . substMeta 

instance (SubstMeta x,SubstMeta y) => SubstMeta (x,y) where
  substMeta ms (x,y) = (s ms x,s ms y)

instance (SubstMeta x,SubstMeta y,SubstMeta z) => SubstMeta (x,y,z) where
  substMeta ms (x,y,z) = (s ms x,s ms y,s ms z)

instance SubstMeta x => SubstMeta (Maybe x) where
  substMeta ms = fmap (s ms)
