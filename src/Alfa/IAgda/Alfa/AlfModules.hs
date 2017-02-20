module Alfa.AlfModules(module Alfa.AlfModules,Module(..),Import(..),Decls,Decl(..),Comment(..)) where
import Alfa.UAbstract
import Alfa.UAnnots(Position(..),noPosition)
import Alfa.Utils2(splitEitherList,apSnd)
import Char(isSpace)

type Imports = [Import]

moduleImports :: Module -> Imports
moduleImports (Module decls) = [imp | ImportDecl imp<-decls]

appendModuleAnnots (Module cdecls) annots = Module (cdecls++map Comment annots)

splitModuleAnnots :: FilePath -> Module -> (Module,[(Position,String)])
splitModuleAnnots filename (Module decls) = (Module decls', annots)
  where
    (decls',annots) = splitDecls decls

    splitDecls decls = splitEitherList (concatMap isAnnot decls) 
    
    --isAnnot :: Decl -> Either Decl String
    isAnnot (Comment ('{':'-':'#':s)) = [Right (unknownpos,stripend s)]
    isAnnot (Decl ok [DefA as (CommentDef ('{':'-':'#':s))]) = 
      [Right (unknownpos,stripend s)]
    isAnnot (Decl ok [DefA as (Package (n,(ctx,PackageDef decls)))]) =
       {- This extracts annotations from inside packages.
          This is temporary fix
          for the convenience of another program that generates Alfa files.
       -}
         Left (Decl ok [DefA as (Package (n,(ctx,PackageDef decls')))]):
         map Right annots
       where (decls',annots) = splitDecls decls
    isAnnot d = [Left d]

    unknownpos = Position filename r c
      where Position _ r c = noPosition

-- Remove trailing "#-}" or "-}"
stripend :: String -> String
stripend = reverse . stripend' . reverse
  where
    stripend' ('}':'-':s) = stripend'' s
    stripend' (c:s) = if isSpace c then stripend' s else s
    stripend'' ('#':s) = s
    stripend'' s = s

stripcomment = stripcomment' . dropWhile isSpace
  where
    stripcomment' ('{':'-':s) = stripend s
    stripcomment' ('-':'-':s) = s

-- Append a new import declaration after the last import declaration in the
-- module.
appendImport i (Module ds) =
   Module .
   reverse . uncurry (++) .
   apSnd (ImportDecl i:) . break isImport .
   reverse $ ds
 where
   isImport (ImportDecl _) = True
   isImport _ = False
