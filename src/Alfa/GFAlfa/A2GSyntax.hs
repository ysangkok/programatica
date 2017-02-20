module A2GSyntax where

import UAbstract
import AbstractOps(stripAnnots)
import GSyntax
import Operations
import qualified Types as T
import qualified Grammar as G
import Linearization (linearize)

-- Agda/Alfa expressions as GF terms. AR 15/10/1999 -- 19/11

type Grammar   = G.Grammar
type Term      = T.Term
newtype Theory = Theory [DefB]

-- the main methods

class GGf a where
 ggf :: Grammar -> a -> Term
 lin :: Grammar -> a -> String
 lin g a = linearize g (ggf g a) []

instance GGf DefB where ggf g = gf . (gDef  g)
instance GGf Exp where ggf g = gf . (gExpr g)
instance GGf Theory where ggf g (Theory t) = gf (gDefs g t)

stripTopAnnot (Value (x,(ctx,e,t))) = Value (x,(ctx,stripAnnots e,t))
stripTopAnnot d = d

-- the principal functions

gDef :: Grammar -> DefB -> GDef
gDef gr def =
 case stripTopAnnot def of
   Value (Var x,(c,ECase e bb,typ)) | isInference x -> 
    GDfElim (gVar x) (gContext gr c) (gSet gr typ) (gExpr gr e) (gBranches gr bb)
   Value (Var x,(c,b,typ)) | isInference x -> 
    GDfIntro (gVar x) (gContext gr c) (gSet gr typ) (gElem gr b)
   Value (Var x,(c,b,typ)) | isTheorem x -> --- let the identifier tell...
    GDfThm (elemContext gr x c)  (gContext gr c) (gSet gr typ) (gElem gr b)
   Value (Var x,(c,ESum cc@(_:_),typ)) -> 
    GDfData (setContext gr x c) (gContext gr c) (gConstructors gr cc)
   Value (Var x,(c,ECase e bb,typ)) -> 
    GDfRec (exprContext gr x c) (gContext gr c) (gSet gr typ) 
           (gExpr gr e) (gBranches gr bb)
   Value (Var x,(c,b,typ)) -> 
    GDfExpl (exprContext gr x c) (gContext gr c) (gSet gr typ) (gExpr gr b)
   Data (Var x,(cont,constrs)) ->
    GDfData (setContext gr x cont) (gContext gr cont) (gConstructors gr constrs)
   Type (Var x,(cont,exp)) ->
    gDef gr (Value (Var x,(cont,exp,ESort (Sort "Type"))))
   Axiom (Var x,(cont,exp)) ->
    gDef gr (Value (Var x,(cont,EMeta 0,exp)))
   Package (Var x,(cont,PackageDef decls)) ->
    GDfPackage (gVar x) (gContext gr cont) (gDecls gr decls)
   Open (EVar (Var x),openargs) ->
    GDfOpen (gVar x) (gVars (map fst openargs))
   Binding (Var x,ECase e bb) -> 
    GDfBindRec (exprContext gr x emptyCtx) (gExpr gr e) (gBranches gr bb)
   Binding (Var x,b) -> 
    GDfBindExpl (exprContext gr x emptyCtx) (gExpr gr b)
   CommentDef s ->
    GDfComment (GString s) 
   _ -> 
    GDfComment (GString "DEFINITION UNRECOGNIZED")

gExpr :: Grammar -> Exp -> GExpr
gExpr gr exp =
  case gKind gr exp of
    GKSet  -> GsetExpr  (gSet gr exp)
    GKElem -> GelemExpr (gElem gr exp)

gSet  :: Grammar -> Exp -> GSet
gSet gr exp =
 case exp of
   EApp f a           -> gSetApps gr f' a' where (f',a') = termFormA exp
   ESort (Sort s)     -> GUseBsSet (GBsSet s []) ---
   EVar (Var v) 
    | isConstant gr v -> GUseBsSet (GBsSet v [])
    | otherwise       -> GvarSet (gVar v)
   EPi a b            -> GFun (gBindings gr a') (gSet gr b') 
                                           where (a',b') = prodFormA exp 
   EAbs a b           -> GabstrSet (gBindings gr a') (gSet gr b') 
                                           where (a',b') = absFormA exp 
   ESum []            -> GemptySet
   ESum cc            -> GData (gConstructors gr cc)
   ECase e []         -> GemptyCaseSet (gElem gr e)
   ECase e bb         -> GCaseSet (gElem gr e) (gBranches gr bb)
   ESig []            -> GemptySig
   ESig sig           -> GSig (gLabellings gr sig)
   EProj e (Label l)  -> GProjSet (gElem gr e) (gVar l)
   ELet decls e       -> GLetSet (gDecls gr decls) (gSet gr e)
   EMeta int          -> GMetaSet (GString (show int)) 
   EProofOf e1 e2     -> gSet gr e2 
   ETyped e1 e2       -> gSet gr e1 
   EAnnot _ e         -> gSet gr e  
   _                  -> GvarSet (gVar "SetExp") --(show exp))

gElem :: Grammar -> Exp -> GElem
gElem gr exp =
 case exp of
   EApp f a            -> gElemApps gr f' a' where (f',a') = termFormA exp
   ECon (Con c) ee     -> GAppElem (GconElem (GString c)) (gExprs gr ee) 
   EVar (Var v) 
    | isConstant gr v  -> GUseBsElem (GBsElem v [])
    | otherwise        -> GvarElem (gVar v)
   EAbs a b            -> Gabstract (gBindings gr a') (gElem gr b') 
                                           where (a',b') = absFormA exp 
   ECase e []          -> GemptyCaseElem (gElem gr e)
   ECase e bb          -> GCaseElem (gElem gr e) (gBranches gr bb)
   EStr []             -> GemptyStruct
   EStr str            -> GStruct (gDecls gr str)
   EProj e (Label l)   -> GProjElem (gElem gr e) (gVar l)
   ELet decls e        -> GLetElem (gDecls gr decls) (gElem gr e)
   EMeta int           -> GMetaElem (GString (show int))
   EProofOf e1 e2      -> gElem gr e2 
   ETyped e1 e2        -> gElem gr e1 
   EAnnot _ e          -> gElem gr e
   _                   -> GvarElem (gVar "ElemExp") --(show exp))

gDecl :: Grammar -> ([Var],Exp) -> GDecl
gDecl gr (xx, typ) = GcnDecl (gVars xx) (gSet gr typ)

gConstructor :: Grammar -> Constructor -> GConstructor
gConstructor gr (Constr (Con c,(ctx,_))) = Gconstr (mkConstrElem gr c ctx) (gContext gr ctx)

gBranch :: Grammar -> Branch -> GBranch
gBranch gr (Branch (Con con,(vars,exp))) =
 case gKind gr exp of
   GKSet -> GbranSet  convars (gSet gr exp)
   _     -> GbranElem convars (gElem gr exp)
  where 
   convars = mkConstrElem gr con ctx'
   ctx'     = ctx [(x,EVar (Var "SetExp")) | x <- vars]

gContext :: Grammar -> Context -> GContext
gContext gr (Ctx _ bs) = gBindings gr bs

gBindings gr c =
 case c of
   [] -> GemCont
   (x,a):c' -> GmrCont (gDecl gr xas) (gBindings gr c'') -- mild optimization
                 where 
                   (xas0,c'') = span ((== (ggf gr a)) . (ggf gr) . snd) c
                   xas        = (map fst xas0, a)
--- would use (==a) instead of (== ggf gr a) if Eq were derivable
 
gExprs :: Grammar -> [Exp] -> GExprs
gExprs gr = gList (gExpr gr) GemExpr GmrExpr

gConstructors :: Grammar -> [Constructor] -> GConstructors
gConstructors gr = gList (gConstructor gr) GemCon GmrCon

gBranches :: Grammar -> [Branch] -> GBranches
gBranches gr = gList (gBranch gr) GemBran GmrBran

gVars :: [Var] -> GVars
gVars [Var x]    = GoneVar (Gvar (GString x))
gVars (Var x:xs) = GmrVars (Gvar (GString x)) (gVars xs)

gDecls :: Grammar -> Decls -> GDefs
gDecls gr = (gDefs gr) . decls2defs

gDefs :: Grammar -> [DefB] -> GDefs
gDefs gr = gList (gDef gr) GemDefs GmrDefs

--gLabellings :: Grammar -> [(Label,Exp)] -> GContext
gLabellings gr ll = gBindings gr [(Var l,e) | SigField (Label l) e <- ll]
	-- Should take care of SigDef too!!!


-- auxiliaries

gElemApps :: Grammar -> Exp -> [Exp] -> GElem
gElemApps gr f xx = 
 case stripAnnots f of
   EVar (Var v) 
    | isConstant gr v -> GUseBsElem (GBsElem v (map (gExpr gr) (extractBind xx)))
   _                  -> GAppElem (gElem gr f) (gExprs gr xx)

gSetApps :: Grammar -> Exp -> [Exp] -> GSet
gSetApps gr f xx = 
 case stripAnnots f of
   EVar (Var v) 
    | isConstant gr v -> GUseBsSet (GBsSet v (map (gExpr gr) (extractBind xx)))
   _                  -> GAppSet (gSet gr f) (gExprs gr xx)

extractBind :: [Exp] -> [Exp]
extractBind ee = 
 case map stripAnnots ee of
   e@(EAbs _ _) : es -> xx ++ [b] ++ extractBind es where
                         (xx0,b) = absFormA e
                         xx = map (EVar . fst) xx0
   e            : es -> e : extractBind es
   []                -> []

exprContext :: Grammar -> Ident -> Context -> GExpr 
exprContext gr s c = 
 case gKindIdent gr s of
   GKSet | isConstant gr s -> GsetExpr  (GUseBsSet (setContext gr s c))
   GKSet -> GsetExpr (GAppSet (GvarSet (gVar s))
                      (gList mkGVarExpr GemExpr GmrExpr (varsOfContext gr c))) 
   _     | isConstant gr s -> GelemExpr (GUseBsElem (elemContext gr s c))
   _ ->  GelemExpr (GAppElem (GvarElem (gVar s))
                    (gList mkGVarExpr GemExpr GmrExpr (varsOfContext gr c))) 

elemContext :: Grammar -> Ident -> Context -> GBsElem 
elemContext gr s c = GBsElem s (map mkGVarExpr (varsOfContext gr c))

setContext :: Grammar -> Ident -> Context -> GBsSet
setContext gr s c = GBsSet s (map mkGVarExpr (varsOfContext gr c))

varsOfContext :: Grammar -> Context -> [(String,GKind)]
varsOfContext gr (Ctx _ cont) = 
 [(z,gKind gr typ) | (Var z, typ) <- concat (map vv cont)] where
---   vv (x,a) = [(x,a')] where (xx,a') = prodFormA a
   vv (x,a) = xx ++ [(x,a')] where (xx,a') = prodFormA a

mkGVarExpr :: (String,GKind) -> GExpr
mkGVarExpr (x,GKSet)  = GsetExpr  (GvarSet  (gVar x))
mkGVarExpr (x,GKElem) = GelemExpr (GvarElem (gVar x))

mkConstrElem :: Grammar -> Ident -> Context -> GElem
mkConstrElem gr c ctx =
 if   isConstant gr c 
 then GUseBsElem (elemContext gr c ctx)
 else GAppElem (GconElem (GString c)) 
               (gList id GemExpr GmrExpr (map mkGVarExpr (varsOfContext gr ctx)))

decls2defs :: Decls -> [DefB]
decls2defs = concat . (map gDs) where
 gDs (Decl _ defs) = [defb | DefA _ defb <- defs]
 gDs _             = [] ---

gVar :: String -> GVar
gVar s = Gvar (GString s)

data GKind = GKSet | GKElem deriving Eq

gKind :: Grammar -> Exp -> GKind
gKind gr exp =
 case exp of
   EAnnot _ e -> gKind gr e
   ESort _  -> GKSet
   ESum _   -> GKSet
   ESig _   -> GKSet
   EPi _ b  -> GKSet
   _        -> GKElem --- must be completed

gKindIdent :: Grammar -> Ident -> GKind
gKindIdent gr@(_,(_,_,rules,_)) c =
 case lookup c rules of
   Just (typ,_) | T.valCat typ == T.Cat "BsSet" -> GKSet
   _  -> GKElem --- no lookup in local context hence

isConstant :: Grammar -> Ident -> Bool
isConstant gr@(_,(_,_,rules,_)) c = maybe False (const True) (lookup c rules)

-- use AbstractOps.flatApp instead?
termFormA :: Exp -> (Exp,[Exp])                  
termFormA (EApp g b) = (g', b'++ [b]) where (g',b') = termFormA g 
termFormA (EAnnot _ e) = termFormA e
termFormA t          = (t,[])

-- use AbstractOps.flatPi instead?
prodFormA :: Exp -> ([(Var,Exp)],Exp)
prodFormA (EPi a b) = (frTyping a : a', b') where (a',b') = prodFormA b
prodFormA (EAnnot _ e) = prodFormA e
prodFormA t         = ([], t)

-- use AbstractOps.flatAbs instead?
absFormA :: Exp -> ([(Var,Exp)],Exp)
absFormA (EAbs a b) = (frTyping a : a', b') where (a',b') = absFormA b
absFormA (EAnnot _ e) = absFormA e
absFormA t          = ([], t)

frTyping :: Typing -> (Var,Exp)
frTyping (x :- a) = (x,a)

gList :: (e -> g) -> gg -> (g -> gg -> gg) -> [e] -> gg
gList _ nils _     []     = nils
gList g nils conss (a:as) = conss (g a) (gList g nils conss as)

isTheorem :: Ident -> Bool ---users should be informed...
isTheorem s = take 2 s `elem` ["Th","Le"]

isInference :: Ident -> Bool ---users should be informed...
isInference s = l > 2 && drop (l - 3) s `elem` ["tro","lim"] where l = length s

isConstrDef :: DefB -> Bool
isConstrDef def =
 case def of
   Value (Var x,(_,ECon (Con c) _,_)) | x==c -> True
   _ -> False
