module AgdaPrint where
import AlfSyntax
import qualified ConvToAgda as CVA
import qualified Agda
import AlfModules
import UAbstract() -- type synonyms
import AbstractOps(printImport)
--import qualified ForgetMetas(module')
import Utils2(mix)

prSyntax x = prSyntax' . syn $ x
prSyntax1line x = unwords . lines . prSyntax $ x

printModule = printModule' -- . ForgetMetas.module'
  where
    printModule' (Module decls) =
      --printImports imports ++
      printCDecls decls

--printImports = concatMap prImport

printCDecls = flip mix "\n\n" . map printCDecl

--printCDecl (Comment' cmnt) = "{-"++cmnt++"-}"
--printCDecl (Comment' cmnt) = cmnt
--printCDecl (Decl' _ decl) = pr (CVA.decl) decl
printCDecl (ImportDecl imp) = printImport imp
printCDecl d = pr CVA.decl d

prSyntax' syntax =
  case syntax of
    VarS n -> prVar n -- !! puts symbolic ids in ( )...
    ConS n -> prCon n -- !! puts symbolic ids in ( )...
    LabelS n -> prLabel n -- !! puts symbolic ids in ( )...
    ExpS e -> pr CVA.exp e
    DeclS d -> pr CVA.decl d
    DeclsS ds -> pr CVA.decls ds -- !! prints lists with [ , , , ]
    --CDeclS d -> pr CVA.cdecl d
    --CDeclsS ds -> printCDecls ds
    ModuleS m -> printModule m
    --ImportsS is -> printImports is
    ImportS i -> printImport i
    ContextS ctx -> prContext ctx
    --BranchS b -> pr CVA.branch b
    --BranchesS bs -> pr CVA.branches bs -- !!
    --AlfConstructorS (AlfCon c) -> pr CVA.constr c
    --AlfConstructorsS cs -> pr CVA.cons cs
    --TypingS nt -> ...
    --TypingsS nt -> ...
    _ -> typeof syntax

--
--pr c x = case Agda.run (c x) of Right y -> Agda.ppAll (Agda.forgetMetaVars y)
pr c = pr' Agda.ppAll c

pr' pp c x = case Agda.run (c x) of
	       Right y -> pp (Agda.forgetMetaVars y)
	       Left e -> error ("AgdaPrint.hs: "++e)

prVar = prName' CVA.var
prCon = prName' CVA.con
prLabel = prName' CVA.label
prContext =  pr' (Agda.pIText . Agda.ppCArgs Agda.PDAll 10) CVA.context'

prName' conv = pr' Agda.pprId conv

{-
prName' conv x =
  case Agda.run (conv x) of
    Right y -> Agda.pprId y
    --Left _ -> error ("Can't print "++show x)
-}
--prCon = pr CVA.con
