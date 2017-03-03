module ParseProgram where

import HsModule
--import HsConstants(prelude_mod)
import SrcLoc(loc0)
--import HsName
import NamesEntities
import ParseMonad(parseFile,PM)
import UnlitJanus(readHaskellFile)
import ParserOptions(Flags,Flags(prel,cpp,plogic),flags0)

import WorkModule(analyzeModules,WorkModuleI(..))
import PPModules
import TypedIds(NameSpace(..),IdTy(..),namespace)

import ReAssoc(getInfixes)
import ReAssocModule
import ReAssocBaseStruct -- instance for HsModule

import RefsTypes(Module)

import MUtils((@@),( # ), mapFst)
--import Control.Monad(liftM)
import Lift

import Data.Maybe(fromMaybe)

import qualified HasBaseName
import qualified DefinedNames
import qualified ReAssoc
import qualified SourceNames
import qualified HsName
import qualified PosName

-- Parse a program, analyze module dependencies and adjust infix
-- applications in accordance with operator precedences:
--parseProgram :: PM (HsModuleI a b c) -> [FilePath] ->
--                       ([[HsModuleI d e f]],[(Module,WorkModuleI g h)])
parseProgram :: HasBaseName.HasBaseName ModuleName ModName => (ReAssoc.HasInfixDecls QName ds,
                                  ReAssoc.ReAssoc (SourceNames.SN HsName.HsName) ds,
                                  DefinedNames.DefinedNames (SourceNames.SN HsName.HsName) ds) =>
                                 PM (HsModuleI ModuleName QName ds)
                                 -> [[Char]]
                                 -> IO
                                      ([[HsModuleI ModuleName QName ds]],
                                       [(ModuleName, WorkModuleI QName PosName.Id)])
parseProgram files = parseProgram' flags0 files
parseProgram' :: HasBaseName.HasBaseName ModuleName ModName => (DefinedNames.DefinedNames
                                     (SourceNames.SN HsName.HsName) ds,
                                   ReAssoc.ReAssoc (SourceNames.SN HsName.HsName) ds,
                                   ReAssoc.HasInfixDecls QName ds) =>
                                  Flags
                                  -> PM (HsModuleI ModuleName QName ds)
                                  -> [[Char]]
                                  -> IO
                                       ([[HsModuleI ModuleName QName ds]],
                                        [(ModuleName, WorkModuleI QName PosName.Id)])
parseProgram' flags parseMod files =
  rewriteProgram # analyzeFiles' flags parseMod files

analyzeFiles parseMod = analyzeFiles' flags0 parseMod
analyzeFiles' flags parseMod =
  lift . analyzeModules @@ parseSourceFiles (fromMaybe (error "could not parse") (cpp flags)) parseMod

parseSourceFiles cpp parseMod = mapM (parseSourceFile cpp parseMod)

parseSourceFile cpp parseMod path = do
    x <- readHaskellFile path
    y <- parseFile parseMod path x
    return y

rewriteProgram (modss,wms) =
    ((fmap.fmap) reAssocMod modss,wms)
  where
    origOps = (concatMap.map) infixDecls modss
    infixDecls mod = (hsModName mod,mapFst getQualified (getInfixes mod))

    reAssocMod mod = reAssocModule wm origOps mod
       where Just wm = lookup (hsModName mod) wms
