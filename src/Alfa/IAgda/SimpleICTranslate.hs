{-|

  Translate ISynType.Exp into CSyntax.Expr.

-}

module SimpleICTranslate where

import ISyntax
import CSyntax
import Position
import PPrint
import AgdaTrace
import Maybe(mapMaybe)

-- This function only translates expressions that had a direct counterpart 
-- in the concrete syntax. The bool indicates if hidden arguments should
-- be removed or not
--  MT: but it's still called with EStructV etc. ... ??
translate        :: Bool -> Exp     -> CExpr 
translateProgram :: Bool -> Program -> CProgram
translateModule  :: Bool -> LetDef  -> CModule
translateLetDef  :: Bool -> LetDef  -> CLetDef
translateOpen    :: Bool -> OpenArg -> COArg
translateProp    :: EProp   -> CProp
(translate , translateLetDef, translateModule, translateOpen)
  = (exp, ldef, modu, oarg) where
  exp remHidden e0 = case e0 of
    EMeta   m pos pai _ _  aut -> CMeta pos pai aut m
    EMetaV  m _ _ _       -> cmeta m
    EVar    x _            -> var x
    EConst  c  _           -> var c
    EConstV c _           -> var c
    ESort   pos (Sort s)  -> CStar pos s s
    EArrow h e1 e2        -> CArrow h (exp remHidden e1) (exp remHidden e2)
    EProd   b e           -> cUniv [arg remHidden b] (exp remHidden e)
    EAbs    b e           -> cLam   [arg remHidden b] (exp remHidden e)
    EApp   h es           -> cApply (exp remHidden  h) (apargs remHidden es)
    EBinOp  e1 o e2       -> CBinOp (exp remHidden e1) (opid o) (exp remHidden e2)
    EDef     ds e         -> Clet  (ldfs remHidden ds) (exp remHidden e)
    EOpen    e1 os e2     -> Copen  (exp remHidden e1) (oargs remHidden os) (exp remHidden e2)
    ESig     pos sds      -> CProduct   pos  (map (csgn remHidden) sds)
    --EStruct  pos ds _     -> CRecord [] pos  (ldfs remHidden ds)
    EStruct pos ds _ _ _ -> CRecord [] pos  (ldfs remHidden ds)
    EPackageType          -> CPackageType
    EProj    e n          -> CSelect (exp remHidden e) n
    EData          cbs    -> CSum    (map (conbnd remHidden) cbs)
    EIndData tel cibs   -> CIndSum (map (arg remHidden) tel) (map (conibnd remHidden) cibs)
    ECon     c   es       -> cApply (CConS c        ) (apargs remHidden es)
    EConF    c e es       -> cApply (CCon  c (exp remHidden e)) (apargs remHidden es)
    ECase    e bres       -> Ccase (exp remHidden e) [(branch remHidden br, exp remHidden e)|(br,e)<- bres]
    EIf eb e1 e2          ->  Cif (exp remHidden eb) (exp remHidden e1) (exp remHidden e2)
    ELiteral p  l          -> CLit p l
    PreMeta               -> cmeta preMetaVar
    EStop    _ e          -> exp remHidden e
    EClos    _ e          -> exp remHidden e -- !!!!!!!
    _ -> trace("warning:translate e0="++ppDebug e0)()`seq` cmeta preMetaVar

  apargs remHidden = mapMaybe (transHidden remHidden)

  transHidden :: Bool -> (Bool,Exp) -> Maybe (Bool,CExpr)
  transHidden remHidden (h,e) = if remHidden && h then Nothing else Just (h,translate remHidden e)
  
  arg :: Bool -> Bind -> CArg
  arg remHidden (b,a) = CArg (map toHiddenId b) (exp remHidden a)
        where toHiddenId :: (Bool,UId) -> (Bool,Id)
              toHiddenId (h,x) = (h,toId x)
  


  ldfs remHidden = map (ldef remHidden)
  ldef remHidden (DSimple d ) = CSimple (cdef remHidden d)
  ldef remHidden (DMutual ds) = CMutual (map (cdef remHidden) ds)
  cdef remHidden d = CDef (props ps) cd where (ps,cd) = defn remHidden d
  defn remHidden (Def blocked _ ps c _ tel a e) = (ps, bdy) where
    bdy = case (a,e) of
      (EPackageType,DExp(Epackage pos ds _ _ _ ))->  pkg (CPackageDef [] pos (ldfs remHidden ds))
      (EPackageType,DExp e1                )-> pkg (CPackageInstance (exp remHidden e1))
      ( _          ,DExp e1                )-> CValueT cc ctel ca   (exp remHidden e1)
      ( _          ,PN                     )-> CAxiom  cc ctel ca
      ( _          ,Native                     )-> CNative  cc  ca
    (cc, ctel, ca, pkg) = (toId c, map (arg remHidden) tel, exp remHidden a, CPackage cc ctel)
  defn remHidden (UnTypedDef _ _ ps c _(DExp e))= (ps, CValue(toId c)(exp remHidden e))
  defn remHidden (DOpen e os)  = ([], COpen(exp remHidden e)(oargs remHidden os))

  oargs remHidden (OpenArgs os _) = COpenArgs (map (oarg remHidden) os)
  oarg remHidden (OpenConst    ps   c  )= COArg  (props ps)   (toId c)
  oarg remHidden (OpenConstT   ps   c a)= COArgT  (props ps)   (toId c) (exp remHidden a)
  oarg remHidden (OpenConstAs  ps x c  )= COArgAs (props ps) x (toId c)
  oarg remHidden (OpenConstAsT ps x c a)= COArgAsT(props ps) x (exp remHidden a)(toId c) -- WHY!!

  conbnd  remHidden (c,tel) = (c, map (arg remHidden) tel)
  conibnd remHidden (cb,es) = (conbnd remHidden cb, apargs remHidden es)

  branch remHidden (CBConM c pas _) = CPCon c $ map (CPVar . cpat) pas  where
    cpat (PArgT x e) = CPatT  (toId x) (exp remHidden e)
    cpat (PArg  x  ) = CPatId (toId x)
  branch _ _ = error "translate:branch"

  csgn remHidden (ESigAbs(xs,a)) = CSign (ids (map snd xs)) (exp remHidden a)
  csgn remHidden (ESigDefn d   ) = CSignDef . snd $ defn remHidden d

  opid  o     = case exp False o of{CVar n -> n; _ -> error"translate:opid"}
  var   x     = CVar  (toId x)


  ids xs = map toId xs
  nohide = (,)False
  cmeta m  = CMeta noPosition True Nothing m
  props  = map translateProp
  -- merge with defn
  modu remHidden (DSimple(Def _ _ ps c _ tel _ (DExp e))) = CModule (toId c) (map (arg remHidden) tel) $
    case e of Epackage pos ds _ _ _-> CPackageDef (props ps) pos (ldfs remHidden ds)
              _                 -> CPackageInstance (exp remHidden e)
  modu _ _ = error "translate:translateModule"

translateProgram remHidden (Program ds) = CProgram$ map (translateModule remHidden) ds
translateProp Eprivate  = Cprivate
translateProp Epublic   = Cpublic
translateProp Eabstract = Cabstract
translateProp Econcrete = Cconcrete
