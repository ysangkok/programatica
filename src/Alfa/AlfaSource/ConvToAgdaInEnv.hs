{-# COMPILERFLAGS -fallow-redef #-}
module ConvToAgdaInEnv where
#ifdef __GLASGOW_HASKELL__
import Prelude hiding (exp) -- to hide exp
#else
--import Prelude hiding (Floating) -- to hide exp
#endif
import Agda
import qualified UAbstract as U
import qualified UAnnots as U
--import qualified AlfModules as UM
import AbstractOps(noname,trivialLet,ndStyleLet,printImport)
import LitSugar(desugarChar,desugarString,desugarInt,desugarRat)

import Char(isAlpha)
import Monad(ap)
--import Debug2(trace)

infixl 1 #,<#

#ifdef __HASKELL98__
#define map fmap
#endif

--- From Alfa to proof engine ---

-- ENV is a reader monad wrapping a Bool around the PCM monad from Agda ProofState
type ENV = Env Bool -- :: *->* 

exps :: [U.Exp] -> ENV [CExpr]
exps = mapM exp

-- Because of HBC's lousy error messages on pattern match failure...
exp :: U.Exp -> ENV CExpr
exp = exp_ConvToAgda
exp_ConvToAgda e =
    case e of
      U.EApp e1 e2 -> eApp # exp e1 <# exp e2  -- Inefficient!!
      U.EVar n -> CVar # var n
--    U.ECon n es -> eApply . CConS # con n <# exps es
      U.ECon n    -> CConS # con n
--      U.ELCon n es -> eApply . CConS # con n <# exps es
      U.EAbs (n U.:- t) e -> eAbs # var n <# exp t <# exp e
      U.ELet ds e -> Clet # decls ds <# exp e
      U.ETyped e t -> ifstrip (exp e) (exp (trivialLet t e))
      U.EProofOf t e -> ifstrip (exp e) (exp (ndStyleLet t e))
      U.ECase e bs -> Ccase # exp e <# branches bs
      U.EPi (n U.:- e1) e2 ->
        if n==noname
	then CArrow False # exp e1 <# exp e2
	else ePi # var n <# exp e1 <# exp e2
      U.ESort (U.Sort "Set") -> return (cSet noPosition)
      U.ESort (U.Sort "Type") -> return (cType noPosition)
      U.ESort (U.Sort "Package") -> return CPackageType
      U.ESort (U.Sort ('#':s)) -> return (CStar noPosition n n) where n=read s
      U.ESum d ->
#ifdef IAGDA
                  if isPlainSum d
		  then CSum # esum d
		  else CIndSum [] # eisum d
      U.EEmptyIndSum -> return (CIndSum [] [])
#else
		  CSum # esum d
#endif
      U.EProj e n -> CSelect # exp e <# label n
      U.EOpen e1 oargs e2 ->
        Copen # exp e1 <# openargs oargs <# exp e2
      U.ESig s -> CProduct noPosition # sig s
      U.EStr s -> CRecord [] noPosition # str s
      -- U.ECom c -> ...
      U.EMeta n -> return (CMeta noPosition False (Just False) n)
      U.EAnnot a e -> annot a # exp e
      --U.EExit -> return EExit
      U.EChar c -> exp (desugarChar c)
      U.EString s -> exp (desugarString s)
      U.EInt n -> exp (desugarInt n)
      U.ERat x -> exp (desugarRat x)
      _ -> error ("ConvToAgda.exp "++take 80 (show e))
  where
    ifstrip m1 m2 = do strip <- env; if strip then m1 else m2

    eApp e1 e2 = cApply e1 [(False,e2)]
    eApply e1 es = cApply e1 [(False,e)|e<-es]
    --eAbs n = Clam (False,CArg n)
    --eAbs n t = Clam (False,CArg [n] t)
    eAbs n t = Clam (CArg [(False,n)] t)
    ePi n e1 e2 = CUniv (CArg [(False,n)] e1) e2

    annot a = case U.prAnnot a of
                "" -> id
                s -> Ccomment True s -- !!! fake comment pos!

    -- Flatten applications, for faster pretty printing.
    cApply (CApply e es) as = CApply e (es++as)
    cApply e             as = CApply e as

decls = mapM decl

decl :: U.Decl -> ENV CLetDef
decl (U.Comment cmnt) = return (CLetDefComment cmnt)
decl (U.ImportDecl i) = return (CLetDefComment (printImport i))
decl (U.Decl _ ds) = cMutual . concat # defs ds
  where
    cMutual [d] = CSimple d
    cMutual ds  = CMutual ds

defs :: U.Defs -> ENV [[CDef]]
defs = mapM def
def :: U.Def -> ENV [CDef]
def (U.DefA ps d) =
  case d of
    U.Value (n,(ctx,eval,etype)) ->
        c # pvar n <# context' ctx <# exp etype <# exp eval
      where c n ctx et ev = cdef $ CValueT  n ctx et ev
    U.Binding (n,e) -> c # pvar n <# exp e
      where c n e = cdef $ CValue n e
    U.Package (n,(ctx,b)) ->
        c # pvar n <# context' ctx <# pbody b
      where c n ctx e = cdef $ CPackage n ctx e
    U.Open (e,oargs) ->
        c # exp e <# openargs oargs
      where c e oas = cdef $ COpen e oas
    U.Data (n,(ctx,d)) ->
        c # pvar n <# context' ctx <# esum d
      where c n ctx e = cdef $ Cdata n ctx Nothing e
    U.Type (n,(ctx,e)) -> c # pvar n <# context' ctx <# exp e
      where c n ctx e = cdef $ Ctype n ctx e
    U.Axiom (n,(ctx,e)) -> c # pvar n <# context' ctx <# exp e
      where c n ctx e = cdef $ CAxiom n ctx e
    U.CommentDef s -> return [CDefComment s]
  where
    cdef d = d' ++ [CDef ps' d]
    (pos,ps',d') = defAttrs ps
    pvar = posvar pos

-- A quick hack:
defB d = c . last # def (U.defA d)
  where c (CDef _ d) = d

--com = decls . com2decls

defAttrs (pos,ps,d) = (convpos pos,props ps,detail d)
  where
    detail = maybe [] ((:[]). CDefComment . U.prDetailAnnot)

props = map prop
prop p =
  case p of
    U.Private -> Cprivate
    U.Public -> Cpublic
    U.Abstract -> Cabstract
    U.Concrete -> Cconcrete

convpos (U.Position f l c) = Position f l c

pbody b =
  case b of
    U.PackageDef ds -> CPackageDef [] noPosition # decls ds
    U.PackageInst e -> CPackageInstance # exp e

openargs oas = COpenArgs # mapM openarg oas
openarg (U.OpenArg ups n t n') =
  case (t,n') of
    (Nothing,Nothing) -> COArg    ps # var n 
    (Just t,Nothing)  -> COArgT   ps # var n <# exp t
    (Nothing,Just n2) -> COArgAs  ps # var n <# var n2
    (Just t,Just n2)  -> COArgAsT ps # var n <# exp t <# var n2
  where ps = props ups
branches = mapM branch
branch (U.Branch (n,(ns,e))) = b # con n <# vars ns <# exp e
  where b n ns e = (CPCon n (map (CPVar . CPatId) ns),e)

esum = mapM constr
constr (U.Constr (n,(ctx,cmnt))) = (,) # con n <# context' ctx -- !! layout is lost

#ifdef IAGDA
isPlainSum = all isPlainConstr
  where isPlainConstr (U.Constr (n,(ctx,restr))) = null restr

eisum = mapM iconstr
iconstr :: U.Constructor -> ENV CIndSummand
iconstr (U.Constr (n,(ctx,restr))) =
     b # con n <# context' ctx <# exps restr -- !! layout is lost
  where b c ctx restr = ((c,ctx),map pairFalse restr)
	pairFalse :: a -> (Bool, a)
	pairFalse a = (False, a)
#endif

{-

comment = mapM comment1
comment1 (e1,e2) = (,) # exp e1 <# exp e2

-}

sig = mapM field
  where
    field (U.SigField n e) = CSign . (:[]) # label n <# exp e
    field (U.SigDef defb) = CSignDef # defB defb
--}

str = decls
{- old:
str = mapM field
  where
    field (n,e) = b # name n <# exp e
    b n e = CSimple $ CDef [] $ CValueT n [] (CMeta noPosition (-1)) e
-}

-- PJ: context is not used in the module anymore - only context'
context :: U.Context -> ENV [(Bool, CArg)]
context (U.Ctx layout ps) = mapM bindb ps -- !! layout is lost
  where
    bindb :: U.Binding -> ENV (Bool, CArg)
    bindb b = (,) False # bind b

context' :: U.Context -> ENV [CArg]
context' (U.Ctx layout ps) = binds ps -- !! layout is lost

binds :: [U.Binding] -> ENV [CArg]
binds = mapM bind

bind :: U.Binding -> ENV CArg
bind (n,e) = cArg # var n <# exp e
  where cArg v = CArg [(False,v)]
---

con (U.Con s) = name s
var (U.Var s) = name s
posvar pos (U.Var s) = name' pos s
label (U.Label s) = name s

name = name' noPosition
name' pos s = liftEnv $ sParse' pos pId (fix s)
  where fix s@(c:_) = if c=='_' || isAlpha c then s else "("++s++")"

--sort (U.Sort s) = name (U.Name s) -- ??

vars = mapM var

--
--f # m = m >>= (return . f)
--mf <# mx = mf >>= (# mx)
f # m = map f m
mf <# mx = mf `ap` mx

---------------------------
-- A reader monad:

newtype Env a b = F (a->PCM b)
runEnv (F f) = f
env = F return
liftEnv m = F $ \ e -> m

instance Monad (Env a) where
  return x = F $ \ e -> return x
  F m1 >>= xm2 = F $ \ e -> m1 e >>= \ x -> case xm2 x of F m2 -> m2 e

instance Functor (Env a) where
  f `map` F m = F $ \e -> f `map` m e
