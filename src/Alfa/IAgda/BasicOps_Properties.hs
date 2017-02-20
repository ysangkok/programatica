module BasicOps_Properties(unique_metas, assertIfDebugging) where
import ISyntax(Exp(..), Bind, LetDef(DSimple, DMutual), Def(Def, UnTypedDef, DOpen),
               Drhs(DExp, PN, Native), OpenArgs, Tel, CaseBranch(CBConM, CBLit),
	       ConBind, IndConBind, ESigDef(ESigAbs, ESigDefn), Decl)
import MetaVars(MetaVar)
import List(nub)

-- First try: check that appMetas generates unique meta variables

-- ghci -cpp -O -package lang BasicOps_Properties.hs

assertIfDebugging :: Monad m => String -> Bool -> m ()
assertIfDebugging message True  = return ()
assertIfDebugging message False = fail ("Assertion failed:" ++ message)


unique_metas :: Exp -> Bool
unique_metas e = test_all_unique (find_all_metas_Exp e)

test_all_unique :: [MetaVar] -> Bool
test_all_unique xs = length xs == length (nub xs)

-- Don't use f_Exp elsewhere - it is just a local abbreviation
f_Exp = find_all_metas_Exp
find_all_metas_Exp, f_Exp :: Exp -> [MetaVar]
find_all_metas_Exp e = case e of
 EMeta     metaVar _Position _Bool _TransClass _Int _Visibility -> [metaVar]
 EProd     bind  exp        -> f_Bind bind ++ f_Exp exp
 EAbs      bind  exp        -> f_Bind bind ++ f_Exp exp
 EArrow    _bool exp1 exp2  -> f_Exp exp1 ++ f_Exp exp2
 EApp      exp boolexps     -> f_Exp exp ++ f_BExps boolexps
 ECon      _id     boolexps ->              f_BExps boolexps
 EConF     _id exp boolexps -> f_Exp exp ++ f_BExps boolexps
 EBinOp    exp1 exp2 exp3   -> f_Exp exp1 ++ f_Exp exp2 ++ f_Exp exp3
 EIf       exp1 exp2 exp3   -> f_Exp exp1 ++ f_Exp exp2 ++ f_Exp exp3
 EDef      letdefs exp      -> f_LetDefs letdefs ++ f_Exp exp
 EOpen     e1 openargs e2   -> f_Exp e1 ++ f_OpenArgs openargs ++ f_Exp e2
 ECase     exp branches     -> f_Exp exp ++ f_CaseBranches branches
 EProj     exp _id          -> f_Exp exp
 EIndData  tel is           -> f_Tel tel ++ f_IndConBinds is
 EData     conbinds         -> f_ConBinds conbinds
 ESig      _pos esigdefs    -> f_ESigDefs esigdefs
 EStruct   _p letdefs _ _ _ -> f_LetDefs letdefs
 Epackage  _p letdefs _ _ _ -> f_LetDefs letdefs

 EMetaV    {} -> error "find_all_metas: EMetaV should not be here"
 EConstV   {} -> error "find_all_metas: EConstV should not be here"
 EStop     {} -> error "find_all_metas: EStop should not be here"
 EClos     {} -> error "find_all_metas: EClos should not be here"

 EVar      {} -> []
 EConst    {} -> []
 ESort     {} -> []
 PreMeta      -> []
 ELiteral  {} -> [] 
 EPackageType -> []

f_Bind = find_all_metas_Bind
find_all_metas_Bind, f_Bind :: Bind -> [MetaVar]
find_all_metas_Bind (_varnames, e) = f_Exp e

f_Decl :: Decl -> [MetaVar]
f_Decl = f_Bind

f_BExps :: [(Bool,Exp)] -> [MetaVar]
f_BExps = concatMap (f_Exp . snd)

f_LetDefs ::  [LetDef] -> [MetaVar]
f_LetDefs = concatMap f_LetDef

f_LetDef :: LetDef -> [MetaVar]
f_LetDef (DSimple def)  = f_Def def
f_LetDef (DMutual defs) = f_Defs defs

f_Defs :: [Def] -> [MetaVar]
f_Defs = concatMap f_Def

f_Def :: Def -> [MetaVar]
f_Def def = case def of
  Def _bool _ _eprops _uid _fcvars tel exp drhs      -> f_Tel tel ++ f_Exp exp ++ f_Drhs drhs
  UnTypedDef _bool1 _bool2 _eprops _uid _fcvars drhs -> f_Drhs drhs
  DOpen exp openargs                                 -> f_Exp exp ++ f_OpenArgs openargs


f_Drhs :: Drhs -> [MetaVar]
f_Drhs drhs = case drhs of 
  DExp exp -> f_Exp exp
  PN       -> []
  Native   -> []

f_Tel :: Tel -> [MetaVar]
f_Tel = concatMap f_Bind

f_CaseBranches :: [(CaseBranch,Exp)] -> [MetaVar]
f_CaseBranches = concatMap helper
  where helper :: (CaseBranch, Exp) -> [MetaVar]
	helper (cb, e) = f_CaseBranch cb ++ f_Exp e

f_CaseBranch :: CaseBranch -> [MetaVar]
f_CaseBranch cb = case cb of
  CBConM _id _patargs _unused -> [] -- *** See "Comment Pattern" below
  CBLit _pos _lit             -> []

f_ConBinds :: [ConBind] -> [MetaVar]
f_ConBinds = concatMap f_ConBind

f_ConBind :: ConBind -> [MetaVar]
f_ConBind (_id, tel) = f_Tel tel

f_ESigDefs :: [ESigDef] -> [MetaVar]
f_ESigDefs = concatMap f_ESigDef

f_ESigDef :: ESigDef -> [MetaVar]
f_ESigDef x = case x of
  ESigAbs decl -> f_Decl decl
  ESigDefn def -> f_Def  def


-- ----------------

f_OpenArgs :: OpenArgs -> [MetaVar]
f_OpenArgs _ = [] -- *** Add correct definition here

f_IndConBinds :: [IndConBind] -> [MetaVar]
f_IndConBinds _ =  [] -- *** Add correct definition here

-- ----------------
-- Comments:
{-
Comment Pattern:

  In ISynType.PatArg an Exp is used where only variables would be expected. 

  This could be added as another check.


----------------

The error cases in find_all_metas_Exp should be moved to a separate
  check för values.


-}

