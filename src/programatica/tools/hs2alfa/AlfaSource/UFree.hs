module UFree where
import UAbstract
import Data.List(union,(\\),nub)

type Set a = [a]

class Free s where
  free :: s -> Set Var
  freeList :: [s] -> Set Var
  freeList = nub . concatMap free

instance Free s => Free [s] where
  free = freeList

instance (Free s1,Free s2) => Free (s1,s2) where
  free (x,y) = free x `union` free y

--if x `elem` free e then x definitely occurs free in e
--                   else x may occur free in e

xs `except` x = filter (/=x) xs

instance Free Exp where
  free expr =
    --badtrace ("free "++show expr) $
    case expr of
      EAnnot _ e -> free e
      EApp e1 e2 -> free (e1,e2)
      EVar y   -> [y]
  --  ECon c es  -> any (x `freeIn`) es
      ECon _     -> []
  --  ELCon c es -> any (x `freeIn`) es
      EAbs (y:-t) b -> free t `union` (free b `except` y)

      ELet decls b -> free decls `union` (free b \\ namesDecls decls)
      ECase e bs -> free (e,bs)
      EPi (y:-t) b  -> free t `union` (free b `except` y)
      ESort s    -> []
      ESum sum   -> free sum
      EProj b y  -> free b
      ESig sig   -> free sig
      EStr str   -> free str
      EMeta m    -> []
      EProofOf e1 e2 -> free (e1,e2)
      ETyped e1 e2 ->  free (e1,e2)

      EChar _ -> []
      EString _ -> []
      EInt _ -> []
      ERat _ -> []

instance Free Branch where
  free (Branch (c,(ns,e))) = free e \\ ns

instance Free Constructor where
  free (Constr (c,(ctx,restr))) = free ctx `union` (free restr \\ namesCtx ctx)

instance Free Decl where
  free d =
    case d of
      Decl c defs -> free defs -- ok safe approximation
      Comment _ -> []
      ImportDecl _ -> []

  freeList [] = []
  freeList (d:ds) = free d `union` (free ds \\ namesDecl d)


instance Free Def where
  free (DefA _ defb) = free defb


freeB ctx b = free ctx `union` (free b \\ namesCtx ctx)

instance Free DefB where
  free defb =
    case defb of
      CommentDef _ -> []
      Value (n,(ctx,ev,et)) -> free ctx `union`
			       ((free et `union` (free ev `except` n))
			        \\ namesCtx ctx)
      Binding (n,e) -> free e -- recursive?
      Package (n,(ctx,b)) -> freeB ctx b
      Open (e,oargs) -> free (e,oargs)
      Data (n,(ctx,sum)) -> free ctx `union` free sum \\ (n:namesCtx ctx)
      Type (n,(ctx,e)) -> freeB ctx e -- recursive?
      Axiom (n,(ctx,e)) -> free (ctx,e)

instance Free OpenArg where
  free _ = [] -- unsafe approximation!!

instance Free SigPart where
  freeList [] = []
  freeList (p:ps) =
    free p `union` (free ps \\ partNames p)

  free p =
      case p of
	SigDef def -> free def
	SigField lbl e -> free e

partNames (SigField (Label lbl) e) = [Var lbl]
partNames (SigDef def) = defNames def

instance Free Context where
    free (Ctx _ bs) = freeInBs bs
      where
	freeInBs [] = []
	freeInBs ((y,e):bs) = free e `union` (freeInBs bs `except` y)

instance Free PackageBody where
  free body =
    case body of
      PackageDef decls -> free decls
      PackageInst e -> free e
