module ConvToV3(exp,decls,name,mapG,apG) where
import Prelude hiding (Floating) -- to hide exp
import V3
import qualified UAbstract as U

--- From Alfa to proof engine ---

exps = listG exp

exp :: U.Exp -> G a (Exp a)
exp e =
  case e of
   U.EApp e1 e2 -> EApp `mapG` exp e1 `apG` exp e2
   U.EIdent n -> unitG (EIdent (ident n))
   U.ECon n es -> ECon (con n) `mapG` exps es
   U.ELCon n es -> ELCon (con n) `mapG` exps es
   U.EAbs (U.Typing (n,_)) e -> EAbs (name n) `mapG` exp e
   U.ELet ds e -> ELet . decls2com `mapG` decls ds `apG` exp e
   U.ECase e bs -> ECase `mapG` exp e `apG` branches bs
   U.EPi n e1 e2 -> EPi (name n) `mapG` exp e1 `apG` exp e2
   U.ESort s -> unitG (ESort (sort s))
   U.ESum d -> ESum `mapG` esum d
   U.EProj e n -> flip EProj (name n) `mapG` exp e
   U.ESig s -> ESig `mapG` sig s
   U.EStr s -> EStr `mapG` str s
   -- U.ECom c -> ...
   U.EMeta n -> if n<=0 then genMetaG
                else unitG (EMeta n)
   U.EAnnot _ e -> exp e
   U.EExit -> unitG EExit


decls2com = foldl Comp Unit

decls = listG decl

decl :: U.Decl -> G a (Decl a)
decl (U.Decl ds) = Decl `mapG` defs ds

defs = listG def
def (n,(ctx,e1,e2)) = c (name n) `mapG` context ctx `apG` exp e1 `apG` exp e2
  where c n ctx e1 e2 = (n,(ctx,e1,e2))

esum = listG constr
constr (n,(ctx,cmnt)) = c (name n) `mapG` context ctx `apG` comment cmnt
  where c n ctx cmnt = (n,(ctx,cmnt))

branches = listG branch
branch (U.Branch (n,(ns,e))) = b (name n) (names ns) `mapG` exp e
  where b n ns e = Branch (n,(ns,e))

comment = listG comment1
comment1 (e1,e2) = (,) `mapG` exp e1 `apG` exp e2

{-
com c =
  case c of
    U.Unit -> unitG Unit
    U.Comp c d -> Comp `mapG` com c `apG` decl d
-}

str = listG bind
sig = listG bind
context = listG bind

bind (n,e) = (,) (name n) `mapG` exp e

---

con = name
ident = name
name (U.Name n) = Name n
sort (U.Sort s) = Sort s

names = map name
--
f `mapG` m = m `bindG` (unitG . f)
mf `apG` mx = mf `bindG` (`mapG` mx)
