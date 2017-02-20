module TC where

import Operations
import Tokens ()
import Grammar
import ComputeTerm
import SymbolTable
import Macros
import Lookup
import PrGrammar
import Predefined
import Monad (liftM2)
import List (nub)

-- (c) Petri Mäenpää & Aarne Ranta, 1998--2001
-- following Thierry Coquand's paper

-- to export

justTypeCheck :: AbstractST -> Cont -> Trm -> Type -> Err Problems
justTypeCheck st cont trm typ = do
  ps <- checkType (st,0,[],cont) noProblems trm (Closure [] typ)
  return $ problemsOut st ps

justTypeInfer :: AbstractST -> Cont -> Trm -> Err (Type,Problems)
justTypeInfer st cont trm = do
  (typ,ps) <- inferType (st,0,[],cont) noProblems trm
  return (closureSubst typ, problemsOut st ps)

-- an auxiliary, to get rid of closures when exporting
problemsOut st (qs,cs) =
  ([(m, (clsn typ, (i, s, clsnc c)))  | (m,(typ,(i,s,c))) <- qs],
   [(clsn x, clsn y) | (x,y) <- cs])
    where 
      cls = closureSubst
      clsn t = compute st $ cls $ errVal t $ whnf t ---
      clsnc c = [(x,clsn a) | (x,a) <- c]

-- data types

type Metas       = [(MetaSymb,(Type,(Int,Substit,Cont)))]
type Constraint  = (Trm,Trm)
type Constraints = [Constraint]
type Problems    = (Metas, Constraints)

justConstrs cs = return ([],cs)  -- in checkType
justMetas qs   = return (qs,[])
noProblems = ([],[])
unionProblems (qs1,cs1) (qs2,cs2) = (nub (qs1 ++ qs2), nub (cs1 ++ cs2))

justOnlyType, justType :: Type -> Err (Type,Problems)
justOnlyType t = return (t,noProblems) -- in inferType
justType       = justOnlyType . Closure []

type TCEnv  = (Theory, Int, Substit, Cont)
type Theory = AbstractST

vGen :: Int -> Trm
vGen = Var . vvGen
vvGen :: Int -> Ident
vvGen = zIdent . ('#':) . show

checkType :: TCEnv -> Problems -> Trm -> Type -> Err Problems
checkType env@(abstr, int, subst, cont) ps trm typ = case trm of
  Meta s  -> justMetas [(s, (typ, (int,subst,cont)))]
  Abs x t -> do
    typ' <- whnf typ
    case typ' of
      Closure g (Prod y a b) -> do
        let v = x --- the abstracted x is fresh in refinement with fun ...
        checkType (abstr, int+1, (x,Var v):subst, (v,Closure g a):cont) ps 
                  t (Closure ((y, Var v):g) b)
      _ -> prtBad ("product expected instead of" +++ prt typ' +++ "for") trm
  Prod x a b -> do
    typ' <- whnf typ
    case typ' of
      TypeType -> do
        let v = vGen int
        (qs1,cs1) <- checkType env ps a TypeType
        (qs2,cs2) <- checkType 
                       (abstr, int+1, (x,v):subst, (x,Closure subst a):cont) ps
                       b TypeType
        return (qs1 ++ qs2, cs1 ++ cs2)
      _ -> prtBad ("type Type expected instead of" +++ prt typ' +++ "for") trm
  _ -> do
    (typ',(qs',cs')) <- inferType env ps trm 
    eqt <- eqVal int typ typ' 
    return (qs', if eqt then cs' else (typ,typ'):cs')

inferType :: TCEnv -> Problems -> Trm -> Err (Type,Problems)
inferType env@(abstr, int, subst, cont) ps@(qs,cs) trm = case trm of
  Cons c -> do
    info <- lookupAbstract c abstr
    case info of
      IdentFun typ  -> justType typ
      IdentCat args -> justType $ mkProd (args,TypeType,[])
      IdentType _   -> justOnlyType TypeType
  Literal c _ -> justType (Cons c)
  Var x  -> do
    typ <- maybeErr ("unknown var in inferType" +++ prt x) $ lookupVar x cont
    justOnlyType typ  --- in tc, you find a val there
  Meta s -> do
    (typ,_) <- maybeErr ("unknown meta in inferType" +++ prt s) $ lookup s qs
    justType typ  --- justOnlyType ?
  App f c -> do
    (typ, (qs1,cs1)) <- inferType env ps f
    typ' <- whnf typ
    case typ' of
      Closure g (Prod x a b) -> do
        (qs2,cs2) <- checkType env ps c (Closure g a)
        return (Closure ((x, Closure subst c):g) b, (qs1 ++ qs2, cs1 ++ cs2))
      _ -> prtBad ("infer product expected instead of" +++ prt typ' +++ "in") trm
  TypeType -> justOnlyType TypeType -- not justType
  Typed t _ -> inferType env ps t --- the type is not always complete for inference
  _ -> prtBad "impossible to inferType" trm

whnf :: Trm -> Err Trm
whnf v = case v of
   App u w -> do
     f <- whnf u 
     x <- whnf w
     app f x
   Closure env e -> eval env e
   Typed t _     -> whnf t     --- should not occur, however
   _             -> return v 

  where

    eval env e = case e of
      Var x -> maybe (Ok e) Ok $ lookupVar x env --- consequence of checkTyoe Abs...
---      Var x -> maybeErr ("Unknown in eval" +++ prt e) $ lookup x env
      App e1 e2 -> do
        f <- eval env e1 
        x <- eval env e2
        app f x
      Cons _      -> return e
      Literal _ _ -> return e
      TypeType    -> return e
      Typed t _   -> eval env t
      _           -> return $ Closure env e

    app u v = case u of
      Closure env (Abs x e) -> eval ((x,v):env) e
      _                     -> return $ App u v

eqVal :: Int -> Trm -> Trm -> Err Bool
eqVal k t1 t2 = do
  a' <- whnf t1
  b' <- whnf t2
  case (a',b') of
    (TypeType,TypeType) -> return True
    (App f x, App g y)  -> liftM2 (&&) (eqVal k f g) (eqVal k x y) 
    (Cons c,  Cons d)   -> return $ eqStrIdent c d
    (Var c,   Var d)    -> return $ eqStrIdent c d --- needed for vGen
    (Literal c s, Literal d t) -> return $ eqCat c d && s == t 
    (Closure g (Abs x b), Closure d (Abs y c)) -> do
       let v = vGen k
       eqVal (k + 1) (Closure ((x,v):g) b) (Closure ((y,v):d) c)
    (Closure g (Prod x a b), Closure d (Prod y f c)) -> do
       let v = vGen k
       t <- eqVal  k      (Closure        g  a) (Closure        d  f)
       u <- eqVal (k + 1) (Closure ((x,v):g) b) (Closure ((y,v):d) c)
       return $ t && u
    _ -> return False

