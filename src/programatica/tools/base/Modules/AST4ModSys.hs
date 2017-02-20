module AST4ModSys where

import qualified HsModule as Hs
import HsModuleMaps()
import HsIdent(getHSName)
import qualified Ents (Ent(Ent))
import DefinedNames(DefinedNames(definedNames))
--import TypedIds (NameSpace(..))

import Relations
import Names
import ModSysAST
import HasBaseName

import Products((><))


--toMod :: DefinedNames QName ds => Hs.HsModuleI QName ds -> Module
toMod (Hs.HsModule s m exp imps ds) 
  = Module {
      modName       = getBaseName m,
      modExpList    = map toExpListEntry `fmap` exp,
      modImports    = toImport `map` imps,
      modDefines    = listToRel (toEnt `map` defs)
    }
    where
    defs            = map (fmap getQualified >< fmap getQualified) 
                    $ definedNames ds
    toEnt (x,y)     = (getHSName x,Ents.Ent (getBaseName m) x y)


-- exports
--toExpListEntry :: Hs.HsExportSpecI QName -> ExpListEntry 
toExpListEntry x =
  case x of
    Hs.EntE i -> EntExp (toEnt getQualified i)
    Hs.ModuleE m -> ModuleExp (getBaseName m)


-- imports 
--toImport :: Hs.HsImportDeclI QName -> Import 
toImport (Hs.HsImportDecl _ m qualified as spec) 
  = Import {
      impSource     = getBaseName m,
      impQualified  = qualified,
      impAs         = getBaseName (maybe m id as),
      impHiding     = hiding,
      impList       = xs
    }
    where
    (hiding, xs) = cvt spec
    cvt Nothing = (True, [])
    cvt (Just (hiding,specs)) = (hiding, toImpListEntry `map` specs)


--toImpListEntry :: Hs.HsImportSpecI QName -> EntSpec Name
toImpListEntry = toEnt id . fmap f
  where
    f = getQualified -- or perhaps signal an error if something is qualified?

--toEnt :: Hs.EntSpec i -> EntSpec i
toEnt unq x =
  case x of
    Hs.Var i           -> Ent i Nothing
    Hs.Abs i           -> Ent i Nothing
    Hs.AllSubs i       -> Ent i (Just AllSubs)
    Hs.ListSubs i js   -> Ent i (Just $ Subs (map (unq.getHSName) js))



