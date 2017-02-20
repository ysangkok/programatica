{-# COMPILERFLAGS -fno-monad-compr #-}
module AlfSyntax(Syntax(..),typeof,IsSyntax(..),syn1,syn2,syns,b0,b1,b2,b3,b4,b5)
 where { -- Note that this module doesn't use layout on the top level
import UAbstract;
import UAnnots ();
import AlfAbstract

#ifdef __HASKELL98__
#define map fmap
#endif

-- Here we make heavy use of the C preprocessor to avoid boring, repetivite 
-- manual code construction.

-- To add a new syntactic category, just add a line in the following definiton:

#define FOLDSYNTAX(CON,CONL) \
  CONL(Exp,Exps) \
  CONL(Decl,Decls) \
  CONL(Def,Defs) \
  CON(DefB) \
  CONL(Branch,Branches) \
  CONL(SigPart,Sig) \
  CONL(OpenArg,OpenArgs) \
  CON(Sort) \
  CON(PackageBody) \
  CON(Context) \
  CONL(Constructor,Constructors) \
  CONL(Var,Vars) \
  CON(Con) \
  CON(Label) \
  CONL(Typing,Typings) \
  CON(Module) \
  CON(Import) \
  CON(Goal) \
  CON(Solution) \
  CON(Constraint) \
  CON(Assumption)

--CONL(Import,Imports)
--CONL(CDecl,CDecls)
--CON(Com) --A
--CONL(Bind,Binds)
--CON(ExpAnnot) \

#define ONECON(c) | c/**/S c
#define TWOCON(c1,c2) ONECON(c1) ONECON(c2)

;data Syntax
  = HoleS
  | ListS [Syntax]
  | PairS Syntax Syntax
  | MaybeS (Maybe Syntax)
  FOLDSYNTAX(ONECON,TWOCON)
  deriving (Show)

;typeof :: Syntax -> String
;typeof s =
  case s of
    --HoleS -> "Hole"
    --ListS ss -> "["++unwords (map typeof ss)++"]"
    ListS ss -> "[...]"
    PairS s1 s2 -> "("++typeof s1++","++typeof s2++")"
    MaybeS s -> "Maybe ..."
    --HiddenS s -> "Hidden "++typeof s
    ---
    SigPartS _ -> "Signature part"
    SigS _ -> "Signature"
    VarS _ -> "Variable"
    VarsS _ -> "Variables"
    _ -> init . head . words . show $ s
	 --"???" -- prevents crashing if forget to update typeof 

;class IsSyntax a where
  syn :: a -> Syntax
  out :: Syntax -> a
  synList :: [a] -> Syntax
  outList :: Syntax -> [a]

  synList = ListS. map syn
  outList (ListS ss) = map out ss
  outList stx = error ("outList "++show stx)

;instance IsSyntax Syntax where
  syn = id
  out = id

;instance IsSyntax a => IsSyntax [a] where
  syn = synList
  out = outList

;instance (IsSyntax a,IsSyntax b) => IsSyntax (a,b) where
  syn (x,y) = PairS (syn x) (syn y)
  out (PairS x y) = (out x,out y)
  out stx = error ("(,).out "++show stx)

;instance (IsSyntax a,IsSyntax b,IsSyntax c) => IsSyntax (a,b,c) where
  syn (x,y,z) = PairS (syn x) (PairS (syn y) (syn z))
  out (PairS x (PairS y z)) = (out x,out y,out z)
  out stx = error ("(,,).out "++show stx)

;instance IsSyntax a => IsSyntax (Maybe a) where
  syn = MaybeS . map syn
  out (MaybeS m) = map out m
  out stx = error ("Maybe.out "++show stx)

;syn1 f = syn.f.out
;syn2 f x y = syn $ f (out x) (out y)
;syn3 f x y z = syn $ f (out x) (out y) (out z)
;syn4 f w x y z = syn $ f (out w) (out x) (out y) (out z)
;syn5 f v w x y z = syn $ f (out v) (out w) (out x) (out y) (out z)
;syns f ss = syn $ f $ [out s | s<-ss, notHoleS s]
  where notHoleS HoleS = False
        notHoleS _ = True

;b0 f _ = syn f
;b1 f ~[b1x] = syn1 f b1x
;b2 f ~[b2x,b2y] = syn2 f b2x b2y
;b3 f ~[b3x,b3y,b3z] = syn3 f b3x b3y b3z
;b4 f ~[b4w,b4x,b4y,b4z] = syn4 f b4w b4x b4y b4z
;b5 f ~[b5v,b5w,b5x,b5y,b5z] = syn5 f b5v b5w b5x b5y b5z

--b1' f [b1x'] = syn1 f b1x'
--b1' f xs = error ("b1' "++show xs)
--b2' f [b2x',b2y'] = syn2 f b2x' b2y'
--b2' f xs = error ("b2' "++show xs)

#define ISSYNTAX(c) ;instance IsSyntax (c) where { syn = (c/**/S) ; out (c/**/S x) = x ; out s = error ("?? c.out "++show s) }
#define ISSYNTAXL(c,cs) ;instance IsSyntax (c) where { syn = (c/**/S) ; out (c/**/S x) = x ; out stx = error ("c.out "++show stx); synList = (cs/**/S) ; outList (cs/**/S xs) = xs ; outList stx = error ("cs.outList "++show stx) }

FOLDSYNTAX(ISSYNTAX,ISSYNTAXL)



{- Unused:
#define MAPTOA(c) to/**/c/**/A
#define MAPFROMA(c) from/**/c/**/A

#define ISSYNTAXA(c) instance IsAnnot a => IsSyntax (c a) where syn = (c/**/S) . MAPTOA(c) ; out ((c/**/S) x) = MAPFROMA(c) x ; out s = error ("?? c.out "++typeof s)
#define ISSYNTAXLA(c,cs) instance IsAnnot a => IsSyntax (c a) where syn = (c/**/S) . MAPTOA(c) ; out ((c/**/S) x) = MAPFROMA(c) x ; synList = (cs/**/S) . map (MAPTOA(c)) ; outList ((cs/**/S) xs) = map (MAPFROMA(c)) xs ; outList stx = error ("cs.outList "++show stx)
-}
}
