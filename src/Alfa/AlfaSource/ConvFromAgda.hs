{-# COMPILERFLAGS -fallow-redef #-}
module ConvFromAgda(var,con,label,exp,decl,decls,context') where
#ifdef __HBC__
--import Prelude hiding (Floating) -- to hide exp
#else
import Prelude hiding (exp) -- to hide exp
#endif
import qualified Agda
import UAbstract
import UAnnots
import AbstractOps(specialLet)
import AlfModules
import AbstractOps(noname)
import LitSugar(sugarLit)

import Debug2(trace) -- debugging

--- From proof engine to Alfa ---

exps = listG exp

exp :: Agda.CExpr -> Exp
-- Because of HBC's lousy error messages on pattern match failure...
exp = exp_ConvFromAgda
exp_ConvFromAgda e =
    case e of
      Agda.CVar n                  -> pos (idpos n) $ EVar (var n)
      Agda.CStar p 0 _             -> pos p eSet
      Agda.CStar p 1 _             -> pos p eType
      Agda.CStar p n _             -> pos p $ eType' n
      Agda.Clam    args  e         -> absExp (typing args) (exp e)      
      Agda.CUniv   args  e         -> piExp (bind args) (exp e)
      Agda.CArrow _ e1 e2          -> EPi (noname:-exp e1) (exp e2)
      Agda.Clet ds e               -> specialLet (decls ds) (exp e)
      Agda.CProduct p sn           -> pos p $ ESig (concatMap sign sn)
      Agda.CRecord _ p ds          -> pos p $ EStr (str ds)
      Agda.Copen e1 oargs e2       -> EOpen (exp e1) (openargs oargs) (exp e2)
      Agda.CSelect e n             -> EProj (exp e) (label n)
      Agda.CSum sum                -> ESum (esum sum)
#ifdef IAGDA
      Agda.CIndSum _ isum          -> ESum (eisum isum)
#endif
      Agda.Ccase e bs              -> ECase (exp e) (branches bs)
      Agda.CApply e1 es            -> cApp e1 es
      Agda.CConS n                 -> eCon0 (con n)
      Agda.CCon n _                -> eCon0 (con n)
      Agda.CBinOp e1 op e2         -> app (EVar (var op)) [exp e1,exp e2]
      Agda.CMeta p toplevel _vis g -> pos p $ EMeta g
      Agda.CClos [] e              -> exp e
      Agda.CClos bs e              -> EAnnot (InEnv (env bs)) (exp e)
      Agda.Ccomment left cmnt e    -> annot left cmnt (exp e)
      Agda.CPackageType            -> ESort (Sort "Package")
      _                            -> error ("ConvFromAgda.exp "++Agda.ppDebug e)
      --Agda.CApply (Agda.CConS n) es -> sugarLit $ eCon (con n) (args es)
      --Agda.CLit _ _ -> ...
      --Agda.CSelectT e n -> ...
  where
    cApp (Agda.CApply e1 es1) es = cApp e1 (es1++es)
    cApp (Agda.CConS n) es = sugarLit $ eCon (con n) (args es)
    cApp e1 es = app (exp e1) (args es)

    args = map (exp . snd)

    sign (Agda.CSign ns t) = [SigField (label n) (exp t)|n<-ns]
    sign (Agda.CSignDef cdefn) = [SigDef (snd $ defn cdefn)]

    annot left s@('{':'-':'#':r) =
       --trace ("Comment: "++s) $
       case rdAnnot r of
        Just a -> --trace (show a) $
	          EAnnot a
	_ -> --trace ("Unrecognized annotation: "++[c]) $
	     id
    annot left s = eComment {-left-} s -- !!! comment pos is lost!

eSet = ESort (Sort "Set") -- !
eType = ESort (Sort ("Type")) -- !
eType' n = ESort (Sort ('#':show n)) -- !

decls = map decl

--decl :: Agda.CLetDef -> Decl
decl (Agda.CSimple d) = decl' [def d]
decl (Agda.CLetDefComment s) = Comment s
decl (Agda.CMutual ds) = decl' (details $ defs ds)
  where
    details [] = []
    details (d1@(DefA _ (CommentDef s)):d2@(DefA (p,ps,_) def):ds) =
      case rdDetailAnnot s of
	Just d -> DefA (p,ps,Just d) def:details ds
	Nothing -> --trace ("rdDetailAnnot "++show s) $
		   d1:d2:details ds
    details (d:ds) = d:details ds

defs = listG def
def (Agda.CDefComment s) = defA (CommentDef s)
def (Agda.CDef ps cdefn) = DefA ps' d
  where
    (p,d) = defn cdefn
    ps' = (convpos p,props ps,Nothing)

defn d =
    case d of
      Agda.CValue n e ->
	  (idpos n,Binding (var n,exp e))
      Agda.CValueT n cargbs ct ce ->
	  (idpos n,Value (var n,(context cargbs,exp ce,exp ct)))
      Agda.CValueS n cargbs ct (Agda.CClause ce) ->
	  (idpos n,Value (var n,(context cargbs,exp ce,exp ct)))
      Agda.Cdata n cargs _maybekind csum ->
	  --Value (var n,(context' cargs,ESum (esum csum),eSet))
	  (idpos n,Data (var n,(context' cargs,esum csum)))
      Agda.CPackage n cargbs body ->
	  (idpos n,Package (var n,(context cargbs,pbody body)))
      Agda.COpen e oargs -> (Agda.noPosition,Open (exp e,openargs oargs)) -- !!
      Agda.Ctype n cargs e -> (idpos n,Type (var n,(context' cargs,exp e)))
      Agda.CAxiom n cargbs t -> (idpos n,Axiom (var n,(context cargbs,exp t)))
      -- ...
      _ -> error ("ConvFromAgda.defn "++Agda.ppDebug d)

props = map prop
prop p =
  case p of
    Agda.Cprivate -> Private
    Agda.Cpublic -> Public
    Agda.Cabstract -> Abstract
    Agda.Cconcrete -> Concrete

  
pbody b =
  case b of
    Agda.CPackageDef _ _ ds -> PackageDef (decls ds) -- !!
    Agda.CPackageInstance e -> PackageInst (exp e)

openargs (Agda.COpenArgs oas) = map openarg oas

openarg oa =
  case oa of
    Agda.COArg ps n -> openArg ps n Nothing Nothing
    Agda.COArgT ps n e -> openArg ps n (Just (exp e)) Nothing
    Agda.COArgAs ps n1 n2 -> openArg ps n1 Nothing (Just (var n2))
    Agda.COArgAsT ps n1 e n2 -> openArg ps n1 (Just (exp e)) (Just (var n2))
  where
    openArg ps n = OpenArg (props ps) (var n)

branches = listG branch
branch (Agda.CPCon n ps,e) = b (con n) (map (var . cpat) ps) `mapG` exp e
  where
    b n ns e = Branch (n,(ns,e))
    cpat (Agda.CPVar (Agda.CPatT n _)) = n
    cpat (Agda.CPVar (Agda.CPatId n)) = n


esum = listG constr
constr (n,ctx) = Constr (con n,(context' ctx,[]))

#ifdef IAGDA
eisum = listG iconstr
iconstr ((n,ctx),es) = Constr (con n,(context' ctx,exps (map snd es)))
#endif

{-
comment = listG comment1
comment1 (e1,e2) = (,) `mapG` exp e1 `apG` exp e2
-}

str = decls

{- old:
str = listG field
  where
    field (Agda.CSimple (Agda.CDef _ (Agda.CValueT n args t e))) =
      (name n,absExp (typings args) (exp e))
-}

typings :: [Agda.CArg] -> [Typing]
typings = ctx2typings . context
typing :: Agda.CArg -> [Typing]
typing carg = map (uncurry (:-)) (bind carg)

context  = ctx . bindings
context' = ctx . bindings
bindings = concatMap bind

bind :: Agda.CArg -> [Binding]
bind (Agda.CArg ns ct) = [(var n,exp ct)|(_hidden,n)<-ns]

env bs = [(var n,exp e)|(n,e)<-bs]
---

var = Var . Agda.getIdString
con = Con . Agda.getIdString
label = Label . Agda.getIdString

--sort (V3.Sort s) = Sort s

idpos = Agda.getIdPosition

--pos = ePos . convpos
pos p = id -- no use to preserve positions here until ConvToAgda preserves them too !!

convpos (Agda.Position f l c) = Position f l c

--
f `mapG` m =  f m
mf `apG` mx = mf mx
unitG = id
listG = map
