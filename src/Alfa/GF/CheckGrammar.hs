module CheckGrammar where

import Operations
import Tokens
import Grammar
import PrGrammar
import Lookup
import ComputeTerm
import Macros
import LinTypes
import Linearize
import SymbolTable
import Update
import Predefined
import Tokenize
import LexGrammar (reservedGFWords)
import List (nub,partition)

import qualified TC as T ----
import TypeCheck (chDefinition)

-- AR 4/12/1999 -- 1/4/2000

type LTEnv a = (GrammarST a, Cont)


-- Type signatures have been commented out to avoid a bug in nhc98-1.00 /TH

showCheckGrammar :: Token a => GrammarST a -> Grammar a -> String 
showCheckGrammar st g = case checkGrammar st g of
  [] -> "No errors detected in the grammar."
  ss -> unlines ss

checkGrammar :: Token a => GrammarST a -> Grammar a -> [String]
checkGrammar st@(abs,_) gr@(Grammar (theory,concrete)) =
  titleIfNeeded "Errors in abstract syntax" (checkAbstract abs theory) ++
  titleIfNeeded "Errors in concrete syntax" (checkConcrete st concrete)

checkReservedIds ids = 
 let ids' = filter (\w -> elem w reservedGFWords && w /= "tokenizer") ids in
  if null ids' then [] else 
  "Warning: some identifiers clash with reserved words:" : ids' 
  -- for grammars obtained otherwise than by parsing
  --- should also check bound variables and patterns

checkAbstract :: AbstractST -> Abstract -> [String]
checkAbstract st@(abs,_) (Abstract defs) = 
 let ids = [symid c | (c,d) <- tree2list abs, notD d] in
   checkReservedIds ids ++
   checkUnique ids ++
   concat (map (checkDef st) defs)
    where
      notD (IdentDef _) = False
      notD (IdentData _) = False
      notD _ = True

checkConcrete :: Token a => GrammarST a -> Concrete a -> [String]
checkConcrete st@(abs,cnc@(conc,_)) (Concrete ldefs) = 
 let ids = (map (symid . fst) (tree2list conc)) in
   checkReservedIds ids ++
   checkUnique ids ++
   checkCompleteGrammar abs cnc ++
   concat (map (checkLDef st) ldefs)

--checkCompleteGrammar :: Token a => AbstractST -> ConcreteST a -> [String]
checkCompleteGrammar abs cnc = 
---  checkComplete "linearization type" -- replaced by default {s:Str}, 21/11/2000
---      [f | (f,IdentCat _) <- abs'] [f | (f,IdentLintype _) <- cnc'] ++
  checkComplete "linearization"
      [f | (f,IdentFun _) <- abs'] [f | (f,IdentLin _ _) <- cnc']
   where
     abs' = tree2list (fst abs)
     cnc' = tree2list (fst cnc)
     checkComplete msg sought given = case sought of
       [] -> []
       s:ss -> (if elem s given then id else (("no" +++ msg +++ "of" +++ prt s) :))
                                                        (checkComplete msg ss given)

-- checking abstract

checkDef :: AbstractST -> Def -> [String]
checkDef st def =
 case def of
   DefCat cat cont ->  
     titleIfNeeded ("Errors in cat" +++ prt cat) (checkContext st cont [])
   DefFun fun typ  ->  
     titleIfNeeded ("Errors in fun" +++ prt fun) (checkType st typ [])
   DefDef patt trm ->
     titleIfNeeded ("Errors in"     +++ prt def) (checkDefinition st (patt,trm))
   _ -> [] --- check DefData as well

checkContext :: AbstractST -> Cont -> Cont -> [String]
checkContext _ [] cont0 = []
checkContext st ((x,a):hyps) cont0 = 
  checkType st a cont0 ++ checkContext st hyps (cont0 ++ [(x,a)])

checkType :: AbstractST -> Type -> Cont -> [String]
checkType st typ = checkTerm st typ TypeType

checkTerm :: AbstractST -> Trm -> Type -> Cont -> [String]
checkTerm st trm typ cont = 
  case T.justTypeCheck st cont trm typ of
    Bad s -> [s]
    Ok (_,cs) -> map prConstr cs where prConstr (t1,t2) = prt t1 +++ "=" +++ prt t2

checkDefinition :: AbstractST -> (Patt,Trm) -> [String]
checkDefinition st pt = 
  case chDefinition st pt of
    Bad s -> [s]
    Ok (_,cs) -> map prConstr cs where prConstr (t1,t2) = prt t1 +++ "=" +++ prt t2

-- checking concrete

checkLDef :: Token a => GrammarST a -> LDef a -> [String]
checkLDef st@(_,co) ldef =
 case ldef of
   DefParam par pp ->  
     titleIfNeeded ("Errors in param" +++ prt par) (checkParType co par)
   DefOper f ty t -> 
     titleIfNeeded ("Errors in oper" +++ prt f) (errMsg (checkLType t ty (st,[])))
   DefLType c ty ->
     titleIfNeeded ("Errors in the lintype" +++ prt c) [] ---(checkLinType co ty)
   DefLintype c ty ->
     titleIfNeeded ("Errors in lintype of" +++ prt c) (checkLinType co ty)
   DefLin fun xx t ->
     titleIfNeeded ("Errors in lin of" +++ prt fun)
       (checkUnique xx ++ checkLin st fun xx t) 
   DefVar var cats ->
     titleIfNeeded ("Errors in variable" +++ prt var) [] --- thus no errors...
   DefTokenizer tok ->
     titleIfNeeded ("Errors in tokenizer" +++ prt tok) $ 
        if elem (symid tok) allTokenizers then [] else ["not defined; using mini"]

-- to check that a suggested parametre type is a disjoint union of finite types

--checkParType :: Token a => ConcreteST a -> Ident -> [String]
checkParType st typid = errMsg (lookupTags (Cons typid) st)

checkLin :: Token a => GrammarST a -> Ident -> [Ident] -> LTerm a -> [String]
checkLin gr@(abs,co) fun xx trm = errMsg doCheck where
 doCheck =
   do typ0 <- lookupFun fun abs
      typ1 <- type2ltype typ0
      typ  <- linType typ1 co
      checkLType (mkAbs xx trm) typ (gr,[])

--checkLinType :: Token a => ConcreteST a -> LType -> [String]
checkLinType st typ = case (computeLType st typ) of
 Ok (RecType r) -> checkUnique lbs ++ concat (map chInh ihs ++ map chLin lins) where
   lbs = map fst r
   (lins,ihs) = partition (\ (l@(Label (_,i)),_) -> l == linLabel i) r
   chInh (label,typ) =  
      if isParType typ then [] else 
      ["the type" +++ prt typ +++ "of" +++ prt label +++ "is not a parameter type"]
   chLin (label,typ) =
      if isStrType typ then [] else 
      ["the type" +++ prt typ +++ "of" +++ prt label +++ "is not a string op type"]
   isParType (Cons typ) = case lookupConcrete typ st of
      Ok (IdentTagType _) -> True
      _ -> False
   isParType (RecType r) = all (isParType . snd) r
   isParType _ = False
   isStrType typ = case typ of
      Table args val -> all isParType args && isStrType val
      TypeStr -> True
      _ -> False
 Ok _ -> ["a linearization type must be a record type instead of" +++ prt typ]
 Bad s -> s : ["found when checking the lintype" +++ prt typ]

--- the main underlying algorithms

--inferLType :: Token a => Term a -> LTEnv a -> Err LType
inferLType trm env@(gr@(abs,conc), vars) =
 case trm of
   Cons ident -> lookupLinType ident conc
   Var ident -> lookupErr ident vars
   Literal c _ -> return $ Cons c
   App f x ->
     do fty <- infer f
        fty' <- computeLType conc fty
        (case fty' of
           Prod z arg val -> 
             do checkLType x arg env
                if isWildIdent z then return val else substitute [(z,x)] val
           _ -> prtBad "function lintype expected for the function in" trm)
   Select f x ->
     do fty <- infer f
        (case fty of
           Table args val -> 
             do checkLTypes (zip x args) env
                return val
           _ -> prtBad "table lintype expected for the table in" trm)
   Cases (([p],t):_) -> --- tries to guess (10/10/2000 for GF-Alfa)
     do arg <- infer (patt2term p)
        val <- infer t
        checkLType trm (Table [arg] val) env
   Project term i ->
     do ty <- infer term --- ??
        (case computeLType conc ty of
           Ok (RecType ts) -> lookupErr i ts
           _ -> Bad ("record type expected for" +++ prt term +++ 
                     "instead of" +++ prt ty))
   Tok _  -> return TypeStr
   Concat s1 s2 -> 
     do checkLType s1 TypeStr env
        checkLType s2 TypeStr env
   Glue s1 s2 -> 
     do checkLType s1 TypeStr env
        checkLType s2 TypeStr env
   Strs tt -> return TypeStrs
   Alts _ (t,aa) -> 
     do checkLType t TypeStr env
        mapM (\ (c,v) -> checkLType c TypeStrs env) aa
        mapM (\ (c,v) -> checkLType v TypeStr env) aa
        return TypeStr
   Record r -> do
     let (ls,ts) = unzip r
     ts' <- mapM infer ts
     return $ RecType $ zip ls ts'

   RecType _  -> return TypeType  --- should check the arguments here
   TypeStr    -> return TypeType
   TypeStrs   -> return TypeType
   Prod _ _ _ -> return TypeType
   Table _ _  -> return TypeType

   _ -> prtBad "cannot infer lintype of" trm
  where
    infer = flip inferLType env

--checkLType :: Token a =>  LTerm a -> LType -> LTEnv a -> Err LType
checkLType trm typ env@(gr@(_,conc),cont) =
 case trm of
   Abs x c -> case computeLType conc typ of
     Ok (Prod z a b) | isWildIdent z -> checkLType c b (gr,(x,a):cont) 
     Ok (Prod z a b) -> do
        b' <- substitute [(z,Var x)] b
        checkLType c b' (gr,(x,a):cont) 
     Ok t -> Bad ("product expected for" +++ prt trm +++ "instead of" +++ prt typ)
     Bad s -> Bad (s +++ "found when type checking" +++ prt trm +++ "in" +++prt typ)
   Cases [] ->
     prtBad "found empty list of cases as type of" trm
   Cases cases@((pp,v):cs) -> case computeLType conc typ of
     Ok (Table [Var _] val) -> case cs of -- param type variable: what can we check?
       [] -> checkLType v val env  -- that there is only one branch, of type val
       _ -> prtBad "no case analysis possible for type variable in" typ
     Ok (Table args val) -> (do
       cases' <- expandCases args trm conc     -- checks coverage
       mapM chCase cases'                      -- need not extend env !
       return typ)
             where chCase (_,tr) = checkLType tr val env
     Ok t -> prtBad "table expected for cases instead of" t
     Bad s -> Bad (s +++ "detected when type checking" +++ prt trm)
   Select tab args -> do --- ?? 3/3
     tys <- inferLTypes args env
     checkLType tab (Table tys typ) env
     return typ
   Let dd body -> do --- first check dd !
     checkLocalLDefs dd env $
       checkLType body typ (gr, [(x,t) | (x,t,_) <- dd] ++ cont)
   Record r -> (do
     typ' <- computeLType conc typ
     (case typ' of
        RecType rr -> do
             mapM (checkField r) rr
             return typ
        _ -> prtBad "record type expected in type checking instead of" typ'))
       where checkField rec (lab,typ) = do
               t <- lookupErr lab rec 
               checkLType t typ env  --- so we have subtyping for records !
   _ ->
     do typ0 <- inferLType trm env
        equ  <- eqLType conc typ0 typ 
        if equ then return typ else 
                   Bad ("type of"  +++ prt trm +++ ":" +++ 
                        "found   " +++ prt typ ++++
                        "expected" +++ prt typ0)

--checkLTypes :: Token a => [(LTerm a,LType)] -> LTEnv a -> Err [LType]
checkLTypes tts env = mapM (\ (tr,ty) -> checkLType tr ty env) tts

--inferLTypes :: Token a => [LTerm a] -> LTEnv a -> Err [LType]
inferLTypes terms env = mapM (flip inferLType env) terms

--checkLocalLDefs :: Token a => [LocalDef a] -> LTEnv a -> Err LType -> Err LType
checkLocalLDefs dd env@(st,cont) m = case dd of
  [] -> m
  (f,ty,tr) : ds -> do
    checkLType tr ty env
    checkLocalLDefs ds (st,(f,ty):cont) m

