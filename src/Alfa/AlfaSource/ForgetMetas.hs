module ForgetMetas where
import Prelude hiding (Floating) -- to hide exp
import UAbstract
import AlfModules

-- This module is now obsolete... /TH 981219

infixl 0 $$

module' (Module is cds) = Module is (cdecls cds)

cdecls = map cdecl

cdecl (Decl' c d) = Decl' c (decl d)
cdecl cd = cd

-- Constructed in 5 minutes by copying & modifying module ConvToV3.hs
-- Code duplication!

exps = map exp

exp e =
  case e of
   EApp e1 e2 -> EApp $$ exp e1 $$ exp e2
   EIdent n ->  e
   ECon n es -> eCon n $$ exps es
--   ELCon n es -> ELCon n $$ exps es
   EAbs t e -> EAbs $$ typing t $$ exp e
   ELet ds e -> ELet $$ decls ds $$ exp e
   ECase e bs -> ECase $$ exp e $$ branches bs
   EPi t e2 -> EPi n $$ typing t $$ exp e2
   ESort s -> e
   ESum d -> ESum $$ esum d
   EProj e n -> flip EProj n $$ exp e
   EOpen e1 oargs e2 -> EOpen $$ exp e1 $$ oargs $$ exp e2
   ESig s -> ESig $$ sig s
   EStr s -> EStr $$ str s
   --ECom c -> ECom $$ com c
   EMeta n -> EMeta (-1)
   EAnnot a e -> EAnnot a $$ exp e
   EExit ->  e

decls = map decl

decl (Decl ds) = Decl $$ defs ds

defs = map def
def d =
  case d of
    CommentDef _ -> d
    Value ps (n,(ctx,e1,e2)) -> c n $$ context ctx $$ exp e1 $$ exp e2
      where c n ctx e1 e2 = Value ps (n,(ctx,e1,e2))
    Binding ps (n,e) -> Binding ps (n,exp e)
    Package ps (n,(ctx,b)) -> c n $$ context ctx $$ pbody b
      where c n ctx e = Package ps (n,(ctx,e))
    Open ps (e,oargs) -> Open ps (exp e,oargs)
    Data ps (n,(ctx,d)) -> Data ps (n,(context ctx,esum d))
    Type ps (n,(ctx,e)) -> Type ps (n,(context ctx,exp e))
    Axiom ps (n,(ctx,e)) -> Axiom ps (n,(context ctx,exp e))

pbody b =
  case b of
    PackageDef ds -> PackageDef (decls ds)
    PackageInst e -> PackageInst (exp e)

esum = map constr
constr (n,(ctx,cmnt)) = c n $$ context ctx $$ comment cmnt
  where c n ctx cmnt = (n,(ctx,cmnt))

branches = map branch
branch (Branch (n,(ns,e))) = b n ns $$ exp e
  where b n ns e = Branch (n,(ns,e))

comment = map comment1
comment1 (e1,e2) = (exp e1,exp e2)

{-
com c =
  case c of
    Unit ->  Unit
    Comp c d -> Comp $$ com c $$ decl d
-}

str = decls --map bind
sig = map bind
context = map bind

typing (Typing t) = Typing (bind t)

bind (n,e) = (n,exp e)

f $$ x = f x
