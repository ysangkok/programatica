module A2GSyntax where

import UAbstract
import AbstractOps(eSet,eType,flatPi,flatAbs,flatApp',stripAnnots,noname)
import GSyntax
import AIdent(AIdent,varToGF,consToGF,constrToGF)
import Operations
import qualified Grammar as G
import qualified Macros as M
import qualified SymbolTable as ST
import Update
import HO(apFst)
--import Debug2(trace)

-- Agda/Alfa expressions as GF terms. AR 15/10/1999 -- 26/3/2000

type GGrammar  = ST.AbstractST
type GTerm     = G.Trm
newtype Theory = Theory [DefB]
type SIdent    = String

-- the top-level method, for translating theories, definitions, and expressions

class GGf a where ggf :: GGrammar -> a -> GTerm

instance GGf DefB where ggf g d = gf (GMkDefinition (gDef g d))
instance GGf Exp where 
  ggf g e = case e of
    EMeta int -> G.Meta (G.MetaSymb (M.zIdent "Exp", int))
    _ -> gf (GMkObject (gExp g e))
instance GGf Theory where ggf g (Theory defs) = gf (GMkTheory (gDefs g defs))

stripTopAnnot (Value (x,(ctx,e,t))) = Value (x,(ctx,stripAnnots e,t))
stripTopAnnot d = d

-- the principal functions

gDef :: GGrammar -> DefB -> GDef
gDef gr def =
 case stripTopAnnot def of
   Value (x,(c,b@(ESum []),typ)) -> 
    GDefVal (expConsContext gr x c) (gContext gr c) (gExp gr typ) (gExp gr b)
   Value (x,(c,ESum cc@(_:_),typ)) -> 
    GDefValData (expConsContext gr x c) (gContext gr c) (gExp gr typ)
                (gConstructors gr cc)
   Value (x,(c,b@(ECase _ []),typ)) -> 
    GDefVal (expConsContext gr x c) (gContext gr c) (gExp gr typ) (gExp gr b)
   Value (x,(c,ECase e bb,typ)) -> 
    GDefRec (expConsContext gr x c) (gContext gr c) (gExp gr typ) 
           (gExp gr e) (gBranches gr bb)
   Value (x,(c,b,typ)) -> 
    GDefVal (expConsContext gr x c) (gContext gr c) (gExp gr typ) (gExp gr b)
   Data (x,(cont,constrs)) ->
    GDefData (expConsContext gr x cont) (gContext gr cont) 
             (gExp gr eSet) (gConstructors gr constrs)
   Type (x,(cont,exp)) ->
    gDef gr (Value (x,(cont,exp,eType)))
   Axiom (x,(c,typ)) ->
    GDefAxiom (expConsContext gr x c) (gContext gr c) (gExp gr typ) 
   Package (Var x,(cont,PackageDef decls)) ->
    GDefPackage (mkGCons x) (gContext gr cont) (gDecls gr decls)
   Open (EVar (Var x),openargs) ->
    GDefOpen (mkGCons x) (gVars [n|OpenArg _ n _ _<-openargs]) -- !!
   Binding (x,b) -> 
    GDefBind (expConsContext gr x emptyCtx) (gExp gr b)
   CommentDef s ->
    GDefComment (GString s) 
   _ -> 
    GDefComment (GString "DEFINITION UNRECOGNIZED")

gExp  :: GGrammar -> Exp -> GExp
gExp gr exp =
 case exp of
   EApp f a           -> gApps gr f' a' where (f',a') = termFormA exp
   ESort (Sort "Type")-> GExpCons (GCons "Typ" []) ---
   ESort (Sort s)     -> GExpCons (GCons s []) ---
   EVar v 
    | isConstant gr v -> GExpCons (aaCons v [])
    | otherwise       -> mkGConsExp v
-- ECon (Con c) ee    -> gConApps gr c ee
   ECon c             -> mkConstrElem gr c emptyCtx
   EPi (x:-t) b | x==noname
		      -> GExpFunIndep (gExps gr (t:args)) (gExp gr res)
		  where (args,res) = flatFunIndep b
   EPi a b            -> GExpFun (gBindings gr ax) (gExp gr b)
                                           where 
                                             (ax,b) = prodFormA exp
   EAbs a b           -> GExpAbs (gBindings gr a') (gExp gr b') 
                                           where (a',b') = absFormA exp 
   ESum []            -> GemptySet
   ESum cc            -> GExpSum (gConstructors gr cc)
   ECase e []         -> GemptyCase (gExp gr e)
   ECase e bb         -> GExpCase (gExp gr e) (gBranches gr bb)
   ESig []            -> GemptySig
   ESig sig           -> GExpSig (gLabellings gr sig)
   EStr []            -> GemptyStruct
   EStr [Decl _ []]   -> GemptyStruct -- 6/10/2000 AR
   EStr str           -> GExpStr (gDecls gr str)
   EProj e l          -> GExpProj (gExp gr e) (gLabel l)
   ELet decls e       -> GExpLet (gDecls gr decls) (gExp gr e)
   EMeta int          -> GExpMeta int
   EProofOf e1 (EVar v) | not (isConstant gr v)
                      -> GExpHypothesis (gExp gr e1) (gVar v)
   EProofOf e1 e2     -> GExpProofOf (gExp gr e1) (gExp gr e2)
   ETyped e1 e2       -> GExpTyped (gExp gr e1) (gExp gr e2)
   EAnnot _ e         -> gExp gr e  
   _                  -> mkGConsExp (Var "SetExp") --(show exp))

gDecl :: GGrammar -> ([Var],Exp) -> GDecl
gDecl gr (xx, typ) = GMkDecl (gVars xx) (gExp gr typ)

gConstructor :: GGrammar -> Constructor -> GConstr
gConstructor gr (Constr (c,(ctx,_))) = 
           GMkConstr (mkConstrElem gr c ctx) (gContext gr ctx)

gBranch :: GGrammar -> Branch -> GBranch
gBranch gr (Branch (con,(vars,exp))) = GMkBranch convars (gExp gr exp) where 
 convars = mkConstrElem gr con ctx'
 ctx'    = ctx [(x,EVar (Var "SetExp")) | x <- vars]

gContext :: GGrammar -> Context -> GListDecl
gContext gr (Ctx _ bs) = gBindings gr bs

gBindings gr c =
 case c of
   [] -> GNilDecl
   (x,a):c' -> GConsDecl (gDecl gr xas) (gBindings gr c'') -- mild optimization
                 where 
                   (xas0,c'') = span ((== (ggf gr a)) . (ggf gr) . snd) c
                   xas        = (map fst xas0, a)
--- would use (==a) instead of (== ggf gr a) if Eq were derivable
 
gExps :: GGrammar -> [Exp] -> GListExp
gExps gr = gList (gExp gr) GNilExp GConsExp

gConstructors :: GGrammar -> [Constructor] -> GListConstr
gConstructors gr = gList (gConstructor gr) GNilConstr GConsConstr

gBranches :: GGrammar -> [Branch] -> GListBranch
gBranches gr = gList (gBranch gr) GNilBranch GConsBranch

gVars :: [Var] -> GListVar
gVars = gVars' . map gVar
  where
    gVars' [x]    = GOneVar x
    gVars' (x:xs) = GConsVar x (gVars' xs)

gVar :: Var -> GVar
gVar = GMkVar . varToGF

gVar' :: AIdent -> GVar
gVar' s = GMkVar (GString s)

gLabel :: Label -> GVar
gLabel (Label l) = gVar' l

gDecls :: GGrammar -> Decls -> GListDef --- what is ? 1/3/2000
gDecls gr = (gDefs gr) . decls2defs

gDefs :: GGrammar -> [DefB] -> GListDef
gDefs gr = gList (gDef gr) GNilDef GConsDef

--gLabellings :: Grammar -> [(Label,Exp)] -> GListDecl
gLabellings gr ll = gBindings gr [(Var l,e) | SigField (Label l) e <- ll]
	-- Should take care of SigDef too!!! ---

gApps :: GGrammar -> Exp -> [Exp] -> GExp
gApps gr f xx = 
 case stripAnnots f of
   EVar v
    | isConstant gr v -> GExpCons (aaCons v (map (gExp gr) (extractBind xx)))
   ECon c -> gConApps gr c xx
   _      -> GExpApp (gExp gr f) (gExps gr xx)

gConApps :: GGrammar -> Con -> [Exp] -> GExp
gConApps gr c xx = GExpCons (ccCons c (map (gExp gr) (extractBind xx)))


aaCons = GCons . consToGF
ccCons = GCons . constrToGF

extractBind :: [Exp] -> [Exp]
extractBind ee = 
 case map stripAnnots ee of
   e@(EAbs _ _) : es -> xx ++ [b] ++ extractBind es where
                         (xx0,b) = absFormA e
                         xx = map (EVar . fst) xx0
   e            : es -> e : extractBind es
   []                -> []

expConsContext gr v@(Var s) ctx 
  | isConstant gr v = aaCons v (map mkGConsExp (varsOfAContext gr ctx))
  | otherwise = GVarApp (GString s) ---
                        (gList mkGConsExp GNilExp GConsExp (varsOfAContext gr ctx))


expConContext  gr c ctx = ccCons c (map mkGConsExp (varsOfAContext gr ctx))

{- old:
expContext :: GGrammar -> SIdent -> Context -> GCons
expContext gr s c
  | isConstant gr s = GCons s (map mkGConsExp (varsOfAContext gr c))
  | otherwise =  GVarApp (GString s)
                         (gList mkGConsExp GNilExp GConsExp (varsOfAContext gr c))
-}

varsOfAContext :: GGrammar -> Context -> [Var]
varsOfAContext gr (Ctx _ cont) = [z | (z, _) <- concat (map vv cont)] where
---   vv (x,a) = [(x,a')] where (xx,a') = prodFormA a
   vv (x,a) = filter ((/=noname).fst) xx ++ [(x,a')] where (xx,a') = prodFormA a


mkGCons :: String -> GCons
mkGCons x = GVarApp (GString x) GNilExp

mkGConsExp :: Var -> GExp
mkGConsExp = GExpVar . gVar

mkConstrElem :: GGrammar -> Con -> Context -> GExp
mkConstrElem gr c@(Con s) ctx --- 
  | isConstrConstant gr c = GExpCons (expConContext gr c ctx)
  | otherwise = GExpApp (GExpVar (gVar' s)) 
                        (gList mkGConsExp GNilExp GConsExp (varsOfAContext gr ctx))

decls2defs :: Decls -> [DefB]
decls2defs = concat . (map gDs) where
 gDs (Decl _ defs) = [defb | DefA _ defb <- defs]
 gDs _             = [] ---

isConstant :: GGrammar -> Var -> Bool
isConstant (abs,_) x = 
  isNotError $ lookupTreeEq show M.eqStrIdent (M.zIdent (consToGF x)) abs

isConstrConstant :: GGrammar -> Con -> Bool
isConstrConstant (abs,_) x = 
  isNotError $ lookupTreeEq show M.eqStrIdent (M.zIdent (constrToGF x)) abs

-- use AbstractOps.flatApp instead?
termFormA :: Exp -> (Exp,[Exp])                  
termFormA = flatApp'
--termFormA (EApp g b) = (g', b'++ [b]) where (g',b') = termFormA g 
--termFormA (EAnnot _ e) = termFormA e
--termFormA t          = (t,[])

-- use AbstractOps.flatPi instead?
prodFormA :: Exp -> ([(Var,Exp)],Exp)
prodFormA = flatPi
--prodFormA (EPi a b) = (frTyping a : a', b') where (a',b') = prodFormA b
--prodFormA (EAnnot _ e) = prodFormA e
--prodFormA t         = ([], t)

-- use AbstractOps.flatAbs instead?
absFormA :: Exp -> ([(Var,Exp)],Exp)
absFormA = apFst (map frTyping) . flatAbs
--absFormA (EAbs a b) = (frTyping a : a', b') where (a',b') = absFormA b
--absFormA (EAnnot _ e) = absFormA e
--absFormA t          = ([], t)

frTyping :: Typing -> (Var,Exp)
frTyping (x :- a) = (x,a)

flatFunIndep e =
  case stripAnnots e of
    EPi (x:-t) e' | x==noname -> apFst (t:) (flatFunIndep e')
    _ -> ([],e)

gList :: (e -> g) -> gg -> (g -> gg -> gg) -> [e] -> gg
gList _ nils _     []     = nils
gList g nils conss (a:as) = conss (g a) (gList g nils conss as)

{- -- Apparently unused. /TH 00-04-26
isConstrDef :: DefB -> Bool
isConstrDef def =
 case def of
   Value (Var x,(_,ECon (Con c) _,_)) | x==c -> True
   _ -> False
-}
