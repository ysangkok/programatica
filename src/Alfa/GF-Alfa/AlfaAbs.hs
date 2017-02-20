module AlfaAbs where
import Operations
import Grammar
import SymbolTable

-- this is a machine-generated GF grammar object

grammarAlfaAbs :: AbstractST
grammarAlfaAbs =
  (BT (Ident ("Int", (-2, 0)), IdentCat []) (BT (Ident ("ConsBranch", (67, 0)
  ), IdentFun (Prod (Ident ("_", (67, 1))) (Cons (Ident ("Branch", (17, 0)))) 
  (Prod (Ident ("_", (67, 3))) (Cons (Ident ("ListBranch", (18, 0)))) (Cons (
  Ident ("ListBranch", (18, 0))))))) (BT (Ident ("Branch", (17, 0)), IdentCat 
  []) NT (BT (Ident ("Cons", (8, 0)), IdentCat []) NT NT)) (BT (Ident (
  "ConsConstr", (64, 0)), IdentFun (Prod (Ident ("_", (64, 1))) (Cons (Ident 
  ("Constr", (15, 0)))) (Prod (Ident ("_", (64, 3))) (Cons (Ident (
  "ListConstr", (16, 0)))) (Cons (Ident ("ListConstr", (16, 0))))))) NT (BT (
  Ident ("ConsExp", (61, 0)), IdentFun (Prod (Ident ("_", (61, 1))) (Cons (
  Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (61, 3))) (Cons (Ident (
  "ListExp", (7, 0)))) (Cons (Ident ("ListExp", (7, 0))))))) (BT (Ident (
  "ConsDecl", (59, 0)), IdentFun (Prod (Ident ("_", (59, 1))) (Cons (Ident (
  "Decl", (13, 0)))) (Prod (Ident ("_", (59, 3))) (Cons (Ident ("ListDecl", (
  14, 0)))) (Cons (Ident ("ListDecl", (14, 0))))))) NT (BT (Ident ("ConsDef", 
  (32, 0)), IdentFun (Prod (Ident ("_", (32, 1))) (Cons (Ident ("Def", (11, 0
  )))) (Prod (Ident ("_", (32, 3))) (Cons (Ident ("ListDef", (12, 0)))) (Cons 
  (Ident ("ListDef", (12, 0))))))) NT NT)) (BT (Ident ("ConsVar", (56, 0)), 
  IdentFun (Prod (Ident ("_", (56, 1))) (Cons (Ident ("Var", (9, 0)))) (Prod 
  (Ident ("_", (56, 3))) (Cons (Ident ("ListVar", (10, 0)))) (Cons (Ident (
  "ListVar", (10, 0))))))) NT (BT (Ident ("ExpHypothesis", (48, 0)), IdentFun 
  (Prod (Ident ("_", (48, 1))) (Cons (Ident ("Exp", (6, 0)))) (Prod (Ident (
  "_", (48, 3))) (Cons (Ident ("Var", (9, 0)))) (Cons (Ident ("Exp", (6, 0)))
  )))) (BT (Ident ("ExpCon", (45, 0)), IdentFun (Prod (Ident ("_", (45, 1))) 
  (Cons (Ident ("String", (-1, 0)))) (Cons (Ident ("Exp", (6, 0)))))) (BT (
  Ident ("ExpCase", (44, 0)), IdentFun (Prod (Ident ("_", (44, 1))) (Cons (
  Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (44, 3))) (Cons (Ident (
  "ListBranch", (18, 0)))) (Cons (Ident ("Exp", (6, 0))))))) (BT (Ident (
  "ExpAbs", (39, 0)), IdentFun (Prod (Ident ("_", (39, 1))) (Cons (Ident (
  "ListDecl", (14, 0)))) (Prod (Ident ("_", (39, 3))) (Cons (Ident ("Exp", (6
  , 0)))) (Cons (Ident ("Exp", (6, 0))))))) (BT (Ident ("DefComment", (30, 0)
  ), IdentFun (Prod (Ident ("_", (30, 1))) (Cons (Ident ("String", (-1, 0)))) 
  (Cons (Ident ("Def", (11, 0)))))) (BT (Ident ("DefBind", (27, 0)), IdentFun 
  (Prod (Ident ("_", (27, 1))) (Cons (Ident ("Cons", (8, 0)))) (Prod (Ident (
  "_", (27, 3))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident ("Def", (11, 0))
  ))))) (BT (Ident ("DefAxiom", (26, 0)), IdentFun (Prod (Ident ("_", (26, 1)
  )) (Cons (Ident ("Cons", (8, 0)))) (Prod (Ident ("_", (26, 3))) (Cons (
  Ident ("ListDecl", (14, 0)))) (Prod (Ident ("_", (26, 5))) (Cons (Ident (
  "Exp", (6, 0)))) (Cons (Ident ("Def", (11, 0)))))))) (BT (Ident ("Constr", 
  (15, 0)), IdentCat []) NT (BT (Ident ("Decl", (13, 0)), IdentCat []) NT (BT 
  (Ident ("Def", (11, 0)), IdentCat []) NT NT))) NT) NT) (BT (Ident (
  "DefOpen", (29, 0)), IdentFun (Prod (Ident ("_", (29, 1))) (Cons (Ident (
  "Cons", (8, 0)))) (Prod (Ident ("_", (29, 3))) (Cons (Ident ("ListVar", (10
  , 0)))) (Cons (Ident ("Def", (11, 0))))))) (BT (Ident ("DefData", (24, 0)), 
  IdentFun (Prod (Ident ("_", (24, 1))) (Cons (Ident ("Cons", (8, 0)))) (Prod 
  (Ident ("_", (24, 3))) (Cons (Ident ("ListDecl", (14, 0)))) (Prod (Ident (
  "_", (24, 5))) (Cons (Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (24, 7))) 
  (Cons (Ident ("ListConstr", (16, 0)))) (Cons (Ident ("Def", (11, 0))))))))) 
  NT NT) (BT (Ident ("DefPackage", (28, 0)), IdentFun (Prod (Ident ("_", (28, 
  1))) (Cons (Ident ("Cons", (8, 0)))) (Prod (Ident ("_", (28, 3))) (Cons (
  Ident ("ListDecl", (14, 0)))) (Prod (Ident ("_", (28, 5))) (Cons (Ident (
  "ListDef", (12, 0)))) (Cons (Ident ("Def", (11, 0)))))))) NT (BT (Ident (
  "DefRec", (25, 0)), IdentFun (Prod (Ident ("_", (25, 1))) (Cons (Ident (
  "Cons", (8, 0)))) (Prod (Ident ("_", (25, 3))) (Cons (Ident ("ListDecl", (
  14, 0)))) (Prod (Ident ("_", (25, 5))) (Cons (Ident ("Exp", (6, 0)))) (Prod 
  (Ident ("_", (25, 7))) (Cons (Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (
  25, 9))) (Cons (Ident ("ListBranch", (18, 0)))) (Cons (Ident ("Def", (11, 0
  )))))))))) NT (BT (Ident ("DefValData", (23, 0)), IdentFun (Prod (Ident (
  "_", (23, 1))) (Cons (Ident ("Cons", (8, 0)))) (Prod (Ident ("_", (23, 3))) 
  (Cons (Ident ("ListDecl", (14, 0)))) (Prod (Ident ("_", (23, 5))) (Cons (
  Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (23, 7))) (Cons (Ident (
  "ListConstr", (16, 0)))) (Cons (Ident ("Def", (11, 0))))))))) (BT (Ident (
  "DefVal", (22, 0)), IdentFun (Prod (Ident ("_", (22, 1))) (Cons (Ident (
  "Cons", (8, 0)))) (Prod (Ident ("_", (22, 3))) (Cons (Ident ("ListDecl", (
  14, 0)))) (Prod (Ident ("_", (22, 5))) (Cons (Ident ("Exp", (6, 0)))) (Prod 
  (Ident ("_", (22, 7))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident ("Def", 
  (11, 0))))))))) NT NT) (BT (Ident ("Exp", (6, 0)), IdentCat []) (BT (Ident 
  ("Definition", (4, 0)), IdentCat []) NT NT) NT)))))) (BT (Ident ("ExpApp", 
  (34, 0)), IdentFun (Prod (Ident ("_", (34, 1))) (Cons (Ident ("Exp", (6, 0)
  ))) (Prod (Ident ("_", (34, 3))) (Cons (Ident ("ListExp", (7, 0)))) (Cons (
  Ident ("Exp", (6, 0))))))) NT NT)) NT) (BT (Ident ("ExpFunIndep", (38, 0)), 
  IdentFun (Prod (Ident ("_", (38, 1))) (Cons (Ident ("ListExp", (7, 0)))) (
  Prod (Ident ("_", (38, 3))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident (
  "Exp", (6, 0))))))) (BT (Ident ("ExpFun", (37, 0)), IdentFun (Prod (Ident (
  "_", (37, 1))) (Cons (Ident ("ListDecl", (14, 0)))) (Prod (Ident ("_", (37, 
  3))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident ("Exp", (6, 0))))))) (BT (
  Ident ("ExpCons", (33, 0)), IdentFun (Prod (Ident ("_", (33, 1))) (Cons (
  Ident ("Cons", (8, 0)))) (Cons (Ident ("Exp", (6, 0)))))) NT NT) NT) NT)) (
  BT (Ident ("ExpProofOf", (47, 0)), IdentFun (Prod (Ident ("_", (47, 1))) (
  Cons (Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (47, 3))) (Cons (Ident (
  "Exp", (6, 0)))) (Cons (Ident ("Exp", (6, 0))))))) (BT (Ident ("ExpProj", (
  43, 0)), IdentFun (Prod (Ident ("_", (43, 1))) (Cons (Ident ("Exp", (6, 0))
  )) (Prod (Ident ("_", (43, 3))) (Cons (Ident ("Var", (9, 0)))) (Cons (Ident 
  ("Exp", (6, 0))))))) (BT (Ident ("ExpLet", (42, 0)), IdentFun (Prod (Ident 
  ("_", (42, 1))) (Cons (Ident ("ListDef", (12, 0)))) (Prod (Ident ("_", (42, 
  3))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident ("Exp", (6, 0))))))) NT NT
  ) NT) (BT (Ident ("ExpTyped", (46, 0)), IdentFun (Prod (Ident ("_", (46, 1)
  )) (Cons (Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (46, 3))) (Cons (Ident 
  ("Exp", (6, 0)))) (Cons (Ident ("Exp", (6, 0))))))) (BT (Ident ("ExpSum", (
  41, 0)), IdentFun (Prod (Ident ("_", (41, 1))) (Cons (Ident ("ListConstr", 
  (16, 0)))) (Cons (Ident ("Exp", (6, 0)))))) (BT (Ident ("ExpStr", (40, 0)), 
  IdentFun (Prod (Ident ("_", (40, 1))) (Cons (Ident ("ListDef", (12, 0)))) (
  Cons (Ident ("Exp", (6, 0)))))) (BT (Ident ("ExpSig", (36, 0)), IdentFun (
  Prod (Ident ("_", (36, 1))) (Cons (Ident ("ListDecl", (14, 0)))) (Cons (
  Ident ("Exp", (6, 0)))))) NT NT) NT) NT) (BT (Ident ("ExpVar", (35, 0)), 
  IdentFun (Prod (Ident ("_", (35, 1))) (Cons (Ident ("Var", (9, 0)))) (Cons 
  (Ident ("Exp", (6, 0)))))) NT NT)))))))) (BT (Ident ("String", (-1, 0)), 
  IdentCat []) (BT (Ident ("Set", (68, 0)), IdentFun (Cons (Ident ("Cons", (8
  , 0))))) (BT (Ident ("NilBranch", (66, 0)), IdentFun (Cons (Ident (
  "ListBranch", (18, 0))))) (BT (Ident ("MkBranch", (65, 0)), IdentFun (Prod 
  (Ident ("_", (65, 1))) (Cons (Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (
  65, 3))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident ("Branch", (17, 0)))))
  )) (BT (Ident ("ListBranch", (18, 0)), IdentCat []) NT (BT (Ident (
  "ListConstr", (16, 0)), IdentCat []) NT (BT (Ident ("ListDecl", (14, 0)), 
  IdentCat []) NT (BT (Ident ("ListDef", (12, 0)), IdentCat []) NT (BT (Ident 
  ("ListVar", (10, 0)), IdentCat []) (BT (Ident ("ListExp", (7, 0)), IdentCat 
  []) NT NT) NT))))) (BT (Ident ("MkConstr", (62, 0)), IdentFun (Prod (Ident 
  ("_", (62, 1))) (Cons (Ident ("Exp", (6, 0)))) (Prod (Ident ("_", (62, 3))) 
  (Cons (Ident ("ListDecl", (14, 0)))) (Cons (Ident ("Constr", (15, 0))))))) 
  NT (BT (Ident ("MkDecl", (57, 0)), IdentFun (Prod (Ident ("_", (57, 1))) (
  Cons (Ident ("ListVar", (10, 0)))) (Prod (Ident ("_", (57, 3))) (Cons (
  Ident ("Exp", (6, 0)))) (Cons (Ident ("Decl", (13, 0))))))) NT (BT (Ident (
  "MkVar", (54, 0)), IdentFun (Prod (Ident ("_", (54, 1))) (Cons (Ident (
  "String", (-1, 0)))) (Cons (Ident ("Var", (9, 0)))))) (BT (Ident (
  "MkObject", (21, 0)), IdentFun (Prod (Ident ("_", (21, 1))) (Cons (Ident (
  "Exp", (6, 0)))) (Cons (Ident ("Object", (5, 0)))))) (BT (Ident (
  "MkDefinition", (20, 0)), IdentFun (Prod (Ident ("_", (20, 1))) (Cons (
  Ident ("Def", (11, 0)))) (Cons (Ident ("Definition", (4, 0)))))) NT NT) (BT 
  (Ident ("MkTheory", (19, 0)), IdentFun (Prod (Ident ("_", (19, 1))) (Cons (
  Ident ("ListDef", (12, 0)))) (Cons (Ident ("Theory", (3, 0)))))) NT NT)) NT
  )))) (BT (Ident ("NilConstr", (63, 0)), IdentFun (Cons (Ident ("ListConstr"
  , (16, 0))))) NT (BT (Ident ("NilExp", (60, 0)), IdentFun (Cons (Ident (
  "ListExp", (7, 0))))) (BT (Ident ("NilDecl", (58, 0)), IdentFun (Cons (
  Ident ("ListDecl", (14, 0))))) NT (BT (Ident ("NilDef", (31, 0)), IdentFun 
  (Cons (Ident ("ListDef", (12, 0))))) NT NT)) (BT (Ident ("OneVar", (55, 0))
  , IdentFun (Prod (Ident ("_", (55, 1))) (Cons (Ident ("Var", (9, 0)))) (
  Cons (Ident ("ListVar", (10, 0)))))) (BT (Ident ("Object", (5, 0)), 
  IdentCat []) NT NT) NT)))) NT) (BT (Ident ("Typ", (69, 0)), IdentFun (Cons 
  (Ident ("Cons", (8, 0))))) (BT (Ident ("Theory", (3, 0)), IdentCat []) NT 
  NT) (BT (Ident ("VarApp", (53, 0)), IdentFun (Prod (Ident ("_", (53, 1))) (
  Cons (Ident ("String", (-1, 0)))) (Prod (Ident ("_", (53, 3))) (Cons (Ident 
  ("ListExp", (7, 0)))) (Cons (Ident ("Cons", (8, 0))))))) (BT (Ident ("Var", 
  (9, 0)), IdentCat []) NT NT) (BT (Ident ("emptyCase", (52, 0)), IdentFun (
  Prod (Ident ("_", (52, 1))) (Cons (Ident ("Exp", (6, 0)))) (Cons (Ident (
  "Exp", (6, 0)))))) NT (BT (Ident ("emptyStruct", (51, 0)), IdentFun (Cons (
  Ident ("Exp", (6, 0))))) (BT (Ident ("emptySig", (50, 0)), IdentFun (Cons (
  Ident ("Exp", (6, 0))))) (BT (Ident ("emptySet", (49, 0)), IdentFun (Cons (
  Ident ("Exp", (6, 0))))) NT NT) NT) NT))))), 69)



