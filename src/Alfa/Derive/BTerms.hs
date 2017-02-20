module BTerms where
import UAbstract
import UMetaVar
import UAnnots
import UMatch


------------------------------ VARIABLES ------------------------------

mkVarList :: String -> Int -> [Var]
mkVarList prefix count = map (Var.(prefix++).show) [1..count]

mkEVar :: String -> Exp
mkEVar s = EVar (Var s)

mkEVari :: String -> Int ->Exp
mkEVari s i = EVar (Var (s++(show i)))

appVar s = app (mkEVar s)

(?) :: Exp
(?) = EMeta preMetaVar

mkSet :: Exp
mkSet = ESort (Sort "Set")

testExp, testType :: Exp
testExp = EAbs (Var "x" :- mkSet) (?) --(mkEVar "x")
testType = EPi (Var "x" :- mkSet) mkSet

--------------------------- ENVS & CONTEXTS --------------------------

ctxNames :: Context -> [String]
ctxNames (Ctx _ binds) = map (unVar.fst) binds

unVar :: Var -> String
unVar (Var s) = s

---------------------------- DECLARATIONS ----------------------------

defAttrs :: DefAttrs
defAttrs = (noPosition,[],Nothing)

decl1 def = Decl True [def]
valueDecl' :: DefAttrs -> Var -> Context -> Exp -> Exp -> Decl    
valueDecl' attr var ctx val typ = 
          Decl True [DefA attr (Value (var, (ctx, val, typ)))]

valueDecl :: Var -> Context -> Exp -> Exp -> Decl    
valueDecl = valueDecl' defAttrs

simpleValDecl :: Var -> Exp -> Exp -> Decl
simpleValDecl var val typ = Decl True [defA (Value (var, (emptyCtx, val, typ)))]

bindingDecl name value = bindingDecl' defAttrs (Var name) value 

bindingDecl' :: DefAttrs -> Var -> Exp -> Decl    
bindingDecl' attr var val  = 
          Decl True [DefA attr (Binding (var, val))]


decls2Defs :: [Decl] -> [DefB]
decls2Defs = concat . (map decl2Defs)

decl2Defs :: Decl -> [DefB]
decl2Defs (Decl _ defs) = map stripDefA defs
decl2Defs _ = []

stripDefA :: Def -> DefB
stripDefA (DefA _ defb) = defb

unValDefs :: [DefB] -> [(Var,(Context,Exp,Exp))]
unValDefs = concat . (map unValDef)

unValDef :: DefB -> [(Var,(Context,Exp,Exp))]
unValDef (Value r) = [r]
unValDef _ = []

-----------------
testSimpleValDecl :: Decl
testSimpleValDecl = simpleValDecl (Var "simpleValDecl") testExp testType

----------------------------- DATATYPES ------------------------------
mkConstr0 name = Constr (Con name, (emptyCtx,[]))
mkConstr name binds = Constr (Con name, (ctx binds,[]))

dataDecl0 name cons = simpleValDecl (Var name) (ESum cons) mkSet 

testBoolDecl :: Decl
testBoolDecl = dataDecl0 "Bool"
                         [ mkConstr0 "False", mkConstr0 "True"]

testNatDecl = simpleValDecl (Var "Nat") boolData mkSet where
  boolData = ESum cons
  cons = [ mkConstr0 "Zero", mkConstr "S" [(Var "n", mkEVar "Nat")]]

testNatFun1 = valueDecl (Var "testNatFun1") (ctx binds) (?) typ where
  binds = [(Var "n", mkEVar "Nat")]
  typ = mkEVar "Nat"

------------------------------ PATTERNS ------------------------------


conArgTypes :: Constructor -> [Exp]
conArgTypes (Constr (k, (Ctx _ binds, _))) = map snd binds

buildTypeInfo :: Constructors -> TypeInfo
buildTypeInfo cons = [map ti cons] where
  ti (Constr (k, (Ctx _ binds, _))) = (k, length binds)

expandCon :: String -> Constructor -> (Con, [Var])
expandCon prefix (Constr (k, (Ctx _ binds, _))) = 
           (k, mkVarList prefix (length binds))

expandConAsPat :: String -> Constructor -> Pat
expandConAsPat prefix (Constr (k, (Ctx _ binds, _))) = 
           PCon k (map PVar (mkVarList prefix (length binds)))
