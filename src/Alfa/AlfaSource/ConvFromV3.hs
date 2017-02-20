module ConvFromV3(name,exp,decls) where
import Prelude hiding (Floating) -- to hide exp
import qualified V3
import UAbstract

--- From proof engine to Alfa ---

exps = listG exp

exp :: V3.Exp a -> Exp
exp e =
  case e of
   V3.EApp e1 e2 -> EApp `mapG` exp e1 `apG` exp e2
   V3.EIdent n -> unitG (EIdent (ident n))
   V3.ECon n es -> ECon (con n) `mapG` exps es
   V3.ELCon n es -> ELCon (con n) `mapG` exps es
   V3.EAbs n e -> EAbs (Typing (name n,EMeta 0)) `mapG` exp e
   V3.ELet c e -> ELet `mapG` decls (com2decls c) `apG` exp e
   V3.ECase e bs -> ECase `mapG` exp e `apG` branches bs
   V3.EPi n e1 e2 -> EPi (name n) `mapG` exp e1 `apG` exp e2
   V3.ESort s -> unitG (ESort (sort s))
   V3.ESum d -> ESum `mapG` esum d
   V3.EProj e n -> flip EProj (name n) `mapG` exp e
   V3.ESig s -> ESig `mapG` sig s
   V3.EStr s -> EStr `mapG` str s
   -- V3.ECom c -> ...
   V3.EMeta n -> unitG (EMeta n)
   V3.EAnnot _ e -> exp e
   V3.EExit -> unitG EExit

com2decls = reverse . decls
  where
    decls V3.Unit = []
    decls (V3.Comp com d) = d : decls com


decls = listG decl

decl :: V3.Decl a -> Decl
decl (V3.Decl ds) = Decl `mapG` defs ds

defs = listG def
def (n,(ctx,e1,e2)) = c (name n) `mapG` context ctx `apG` exp e1 `apG` exp e2
  where c n ctx e1 e2 = (n,(ctx,e1,e2))

esum = listG constr
constr (n,(ctx,cmnt)) = c (name n) `mapG` context ctx `apG` comment cmnt
  where c n ctx cmnt = (n,(ctx,cmnt))

branches = listG branch
branch (V3.Branch (n,(ns,e))) = b (name n) (names ns) `mapG` exp e
  where b n ns e = Branch (n,(ns,e))

comment = listG comment1
comment1 (e1,e2) = (,) `mapG` exp e1 `apG` exp e2

{-
com c =
  case c of
    V3.Unit -> unitG Unit
    V3.Comp c d -> Comp `mapG` com c `apG` decl d
-}

str = listG bind
sig = listG bind
context = listG bind

bind (n,e) = (,) (name n) `mapG` exp e

---

con = name
ident = name
name (V3.Name n) = Name n
sort (V3.Sort s) = Sort s

names = map name
--
f `mapG` m =  f m
mf `apG` mx = mf mx
unitG = id
listG = map
