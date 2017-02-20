module USubstitute(module USubstitute,Free) where
import UAbstract
import Utils2(mapSnd,apFst)
import UFree
--import Debug2(badtrace)
default(Int)

type Subst1 syntax = Exp -> Var -> syntax -> syntax

substitute e' x = subst e' x

class Subst syntax where
  subst :: Subst1 syntax
  substList :: Subst1 [syntax]

  substList e' x = map (subst e' x)

instance Subst a => Subst [a] where
  subst = substList

instance (Subst a,Subst b) => Subst (a,b) where
  subst e' x (e1,e2) = (subst e' x e1,subst e' x e2)

instance (Subst a,Subst b,Subst c) => Subst (a,b,c) where
  subst e' x (e1,e2,e3) = (subst e' x e1,subst e' x e2,subst e' x e3)

instance Subst Con where
  subst _ _ con = con

instance Subst Exp where
  subst e' x expr =
    --badtrace ("subst in "++show expr) $
    case expr of
      EAnnot a e -> EAnnot a (s e)
      EApp e1 e2 -> EApp (s e1) (s e2)
      EVar y   -> if y==x then e' else expr
  --  ECon c es  -> eCon c (map s es)
      ECon c     -> eCon0 c
  --  ELCon c es -> ELCon c (map s es)
      EAbs (y:-t) b ->
	  if y==x
	  then EAbs (y:-t') b
	  else EAbs (y':-t') (s b')
	where
	  (y',b') = avoidNameCapture y b e'
	  t' = s t

      ELet decls b -> ELet (subst e' x decls) b'
	 where b' = if x `elem` namesDecls decls
		    then b
		    else s b
      ETyped e t -> ETyped (s e) (s t)
      EProofOf t e -> EProofOf (s t) (s e)
      ECase e bs -> ECase (s e) (subst e' x bs)
      EPi (y:-t) b -> EPi (y':-s t) (if y==x then b else b')
	where (y',b') = avoidNameCapture y b e'
      ESort s -> expr
      ESum sum -> ESum (subst e' x sum)

      EProj b y -> EProj (s b) y
      ESig sig -> expr --ESig (substSig e' x sig) --!!!
      EStr str -> EStr (subst e' x str)
      EMeta m -> expr
      EChar _ -> expr
      EString _ -> expr
      EInt _ -> expr
      ERat _ -> expr

      --ECom n -> e
      --EExit -> e
    where
      s = subst e' x

instance Subst Decl where

  substList e' x [] = []
  substList e' x (d:ds) =
      if x `elem` namesDecl d
      then substDeclTypes e' x d:ds
      else subst e' x d:substList e' x ds
    where
      substDeclTypes e' x d = d -- !!!

  subst e' x decl =
    case decl of
      (Decl c defs) -> Decl c (map def defs)
      d@(Comment _) -> d
      d@(ImportDecl _) -> d
    where
      def = mapDefB (subst e' x)

instance Subst DefB where
  -- precondition: x `notElem` namesDecls defb ??
  subst e' x defb =
      case defb of
        CommentDef _ -> defb
	Value (n,(ctx,ev,et)) -> Value (n,(s ctx,s' ctx ev,s' ctx et))
	Binding (n,e) -> Binding (n,s e)
	Package (n,(ctx,b)) -> Package (n,(s ctx,s' ctx b))
	Open (e,oargs) -> Open (s e,oargs) -- !!
	Data (n,(ctx,d)) -> Data (n,(s ctx,s' ctx d))
	Type (n,(ctx,e)) -> Type (n,(s ctx,s' ctx e))
	Axiom (n,(ctx,e)) -> Axiom (n,(s ctx,s' ctx e))
    where
      s y = subst e' x y
      s' ctx e = if x `elem` namesCtx ctx then e else s e

instance Subst PackageBody where
  subst e' x b =
    case b of
      PackageDef decls -> PackageDef (subst e' x decls)
      PackageInst e -> PackageInst (subst e' x e)

instance Subst SigPart where
  substList e' x [] = []
  substList e' x (p:ps) = subst e' x p:
			  if x `elem` partNames p
			  then ps
			  else substList e' x ps

  subst e' x (SigField lbl e) = SigField lbl (subst e' x e)
  subst e' x (SigDef def) = SigDef (subst e' x def)

instance Subst Constructor where
  subst e' x (Constr con) = Constr (subst e' x con)

instance Subst Context where
  subst e' x (Ctx l ps) = Ctx l (substBs ps)
   where
     substBs [] = []
     substBs ((y,t):bs) = (y,subst e' x t):
			  if y==x
			  then bs
			  else substBs bs


instance Subst Branch where
  subst e' x br@(Branch (c,(ns,b))) =
      if x `elem` ns
      then br
      else Branch (c,(ns',subst e' x b'))
    where (ns',b') = avoidNameCaptures ns b e'

avoidNameCapture y@(Var s) b e =
    if y `freeIn` e
    then (freshvar,subst (EVar freshvar) y b)
    else (y,b)
  where
    freshvar = head [ x | x<-[Var (s++show n)|n<-[1..]],
	                 not (x `freeIn` b || x `freeIn` e)]

avoidNameCaptures ys b e =
  case ys of
    [] -> ([],b)
    y:ys -> (y':ys',b'')
      where (y',b')   = avoidNameCapture y b e
            (ys',b'') = avoidNameCaptures ys b' e


--if x `freeIn` e then x definitely occurs free in e
--                else x may occur free in e

x `freeIn` expr = x `elem` free expr

{-
x `freeIn` expr =
  --badtrace ("freeIn "++show x++" "++show expr) $
  case expr of
    EAnnot _ e -> x `freeIn` e
    EApp e1 e2 -> x `freeIn` e1 || x `freeIn` e2
    EVar y   -> y==x
--  ECon c es  -> any (x `freeIn`) es
    ECon _     -> False
--  ELCon c es -> any (x `freeIn`) es
    EAbs (y:-t) b -> x `freeIn` t || y/=x && x `freeIn` b

    ELet decls b -> x `freeInDecls` decls ||
		    x `notElem` namesDecls decls && x `freeIn` b
    ECase e bs -> x `freeIn` e || x `freeInBranches` bs
    EPi (y:-t) b  -> x `freeIn` t || y/=x && x `freeIn` b
    ESort s    -> False
    ESum sum   -> x `freeInSum` sum
    EProj b y  -> x `freeIn` b
    ESig sig   -> x `freeInSig` sig
    EStr str   -> x `freeInDecls` str
    EMeta m    -> False
    EProofOf e1 e2 -> x `freeIn` e1 || x `freeIn` e2
    ETyped e1 e2 ->  x `freeIn` e1 || x `freeIn` e2

    EChar _ -> False
    EString _ -> False
    EInt _ -> False
    ERat _ -> False

x `freeInBranches` bs = any (x `freeInBranch`) bs
x `freeInBranch` Branch (c,(ns,e)) = x `notElem` ns && x `freeIn` e

x `freeInSum` sum =
  or [x `freeInCtx` c || x `notElem` namesCtx c && x `freeInComment` cm 
      | (c,cm)<- map (\ (Constr (_,a))->a) sum]

freeInComment x = any (x `freeIn`)

x `freeInDecls` [] = False
x `freeInDecls` (d:ds) =
  x `freeInDecl` d || x `notElem` namesDecl d && x `freeInDecls` ds

x `freeInDecl` d =
    case d of
      (Decl c defs) -> any freeInDef defs -- ok safe approximation
      d@(Comment _) -> False
      d@(ImportDecl _) -> False
  where
    freeInDef (DefA _ defb) = x `freeInDefB` defb

x `freeInDefB` defb =
  case defb of
    CommentDef _ -> False
    Value (n,(ctx,ev,et)) -> x `freeInCtx` ctx ||
			     x `notElem` namesCtx ctx &&
			     ( x `freeIn` et || 
			       x/=n && x `freeIn` ev)
    Binding (n,e) -> x `freeIn` e
    Package (n,(ctx,b)) -> x `freeInCtx` ctx ||
			   x `notElem` namesCtx ctx && x `freeInPkgBody` b
    Open (e,oargs) -> True -- bad safe approximation !!!
    Data (n,(ctx,sum)) -> x `freeInCtx` ctx ||
			  x/=n && x `notElem` namesCtx ctx &&
			  x `freeInSum` sum
    Type (n,(ctx,e)) -> x `freeInCtx` ctx ||
			x `notElem` namesCtx ctx && x `freeIn` e
    Axiom (n,(ctx,e)) -> x `freeInCtx` ctx || x `freeIn` e

x `freeInSig` [] = False
x `freeInSig` (p:ps) =
    freeInPart p || x `notElem` partNames p && x `freeInSig` ps
  where
    freeInPart p =
      case p of
	SigDef def -> x `freeInDefB` def
	SigField lbl e -> x `freeIn` e

x `freeInCtx` (Ctx _ bs) = freeInBs bs
  where
    freeInBs [] = False
    freeInBs ((y,e):bs) = x `freeIn` e || y/=x && freeInBs bs

x `freeInPkgBody` body =
  case body of
    PackageDef decls -> x `freeInDecls` decls
    PackageInst e -> x `freeIn` e
-}
