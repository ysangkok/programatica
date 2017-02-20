module PluginAlfa where

import Operations
import Grammar
import Macros
import ComputeTerm
import Predefined

import qualified AlfaPluginKit as A

-- interpret GF in Alfa. AR 16/3/2000 -- 29/3

abstract2atheory :: Abstract -> [A.DefB]
abstract2atheory (Abstract defs) = map (def2adef defs) (filter okDef defs) where
 okDef (DefDef _ _) = False --- postpone interpreting definitions
 okDef _ = True

def2adef :: [Def] -> Def -> A.DefB
def2adef dd d = case d of
 DefCat cat cont -> mkCatAxiom (ident2avar cat) (context2acontext cont)
 DefType t typ -> A.Type (ident2avar t, (context2acontext co,term2aexp val)) where
                    (co,val) = analyseType typ
 DefFun fun typ -> case findDef dd fun of
                     Just (xx,trm) -> mkFunValue fun' co''' trm' val''' where
                                        val''' = term2aexp val''
                                        trm'   = term2aexp trm
                                        co'''  = context2acontext co''
                                        co''   = [(x, alfaConv xxs a) | 
                                                        (x,(_,a)) <- zip xx co]
                                        val''  = alfaConv xxs val
                                        xxs    = zip (varsOfContext co) xx
                     _  -> mkFunAxiom fun' co' val'
                    where 
                      fun' = ident2avar fun 
                      co'  = context2acontext co
                      val' = term2aexp val
                      (co,val) = analyseType typ

findDef :: [Def] -> Fun -> Maybe ([Ident],Trm)
findDef defs fun = lookup fun defs' where
 defs' = [(f,(xx,t)) | DefDef p t <- defs, 
                       (f,pp) <- analysePatt p, Ok xx <- [mapM getVar pp]]
 analysePatt (PattCons c pp) = [(c,pp)]
 analysePatt _ = []
 getVar (PattVar x) = Ok x
 getVar p = Bad ""

term2aexp :: Type -> A.Exp
term2aexp typ = case typ of
  Prod x a b -> A.EPi ((ident2avar x) A.:- (te a)) (te b)
  Var x -> A.EVar $ ident2avar x
  Cons c -> A.EVar $ ident2avar c
  Literal c s -> A.EString s
---  Literal c s | eqStrIdent c identString -> A.EString s
---  Literal c s | eqStrIdent c identInt -> A.EInt s -- this should be there too...
  App f a -> A.EApp (te f) (te a)
  Abs x b -> A.EAbs ((ident2avar x) A.:- unknownAExp) (te b) ---
  Meta ms -> A.ePreMetaVar --- category information lost
  _ -> A.ePreMetaVar --- should not happen
 where te = term2aexp

context2acontext :: Cont -> A.Context
context2acontext hyps = A.Ctx Nothing (map mkBinding hyps) where
 mkBinding (x,a) = (ident2avar x, term2aexp a)

ident2avar :: Ident -> A.Var
ident2avar (Ident (s,_)) = A.Var s

mkCatAxiom :: A.Var -> A.Context -> A.DefB
mkCatAxiom v cont = A.Axiom (v,(cont, A.ESort (A.Sort "Type")))

mkFunAxiom :: A.Var -> A.Context -> A.Exp -> A.DefB
mkFunAxiom v cont typ = A.Axiom (v,(cont, typ))

mkFunValue :: A.Var -> A.Context -> A.Exp -> A.Exp -> A.DefB
mkFunValue v cont val typ = A.Value (v,(cont, val, typ))

unknownAExp :: A.Exp
unknownAExp = A.ePreMetaVar

-- to go from Alfa back to GF

aexp2term :: AEnv -> A.Exp -> Trm
aexp2term env exp = aet exp where 
  aet e = case e of
    A.ETyped (A.EMeta mv) et -> Meta (MetaSymb (aexp2cat et,mv))
    A.EMeta mv -> Meta (MetaSymb (catOfEMeta env mv, mv)) ---
    A.EVar (A.Var v) -> Var (zIdent v)
    A.EApp f a -> App (aet f) (aet a)
    A.EAbs ((A.Var x) A.:- a) b -> Abs (zIdent x) (aet b)
    A.EAnnot _ e -> aet e
    A.EPi (A.Var x A.:- a) b -> Prod (zIdent x) (aet a) (aet b)
    _ -> Meta (MetaSymb (zIdent (show e),0)) --- should not happen

aexp2cat :: A.Exp -> Cat
aexp2cat e = case valCat (aexp2term (const ([],A.ePreMetaVar)) e) of
  Ok c -> c
  _ -> unknownCat

catOfEMeta env i = case snd (env i) of
  A.EMeta _ -> unknownCat
  e -> aexp2cat e

unknownCat :: Cat
unknownCat = zIdent "C" ---

type AEnv = A.MetaEnv

-- to linearize, you have to first rename variables in the context of a grammar.


