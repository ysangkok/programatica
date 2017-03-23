import TypedIds
import qualified SrcLoc
import qualified HsModule
import WorkModule(WorkModuleI(..))
import HsParser(parse)
import ReAssocBase
import ReAssoc(getInfixes)
import MUtils(collectByFst)--,unlessM
import Data.List(partition,nub)
--import Maybe(fromJust)
import ParseProgram
import ParserOptions
import DefinedNames(definedNames)
import ReAssoc(getInfixes)
import HsIdent(getHSName)
import qualified HsIdent
import NamesEntities
import Ents

import UniqueNames
import NoEq
--import UTF8Util
--import PrettyPrint
import PPU
import MUtils
import TiPNT()
import ScopeNamesBase()
import NameMapsBase()
import NameMaps(Role(..),Context(..), MapNames(..),SeqNames)
import qualified PNT
import UnlitJanus(readHaskellFile)
import ParsedSyntax
import qualified ParsedSyntax
import DirUtils(expand,optCreateDirectory)
import ConvRefsTypes
import Relations() -- for show instances
import HsName(ModuleName(..),HsName(..),Id)
import qualified HsName(ModuleName(..), moduleName, plainModule,isMainModule ,isHierarchical ,sameModuleName ,fakeModule ,noModule ,Id(..) ,HsName(..) ,hsUnQual ,mapHsName ,accHsName ,parseModuleName ,isSymbolOp ,isAlphaOp ,isConOp ,ppInfixOp ,ppInfixName ,isSymbol ,splitQualName ,splitQualName')
import SourceNames (SN(..))
import qualified PropPosSyntax (HsModuleR(..))
import qualified ParseMonad
import qualified HasBaseName(getBaseName,HasBaseName)
import qualified DefinedNames(DefinedNames)
import Relations
import qualified Data.Map.Lazy as Map
import qualified ScopeModule
import qualified IxOutputM
import qualified ScopeNames
import qualified RefsTypes
import qualified MT
import qualified QualNames
import qualified ScopeModule

import TiModule()

import ScopeModule
import PNT(PNT(..))
import PrettyPrint
import OutputM
import HsModule(EntSpec(..),hsModName,ImpSpec)
import qualified ScopeProgram

main = tstModules =<< getPPopts

extractLol :: ParseMonad.PM HsModuleR -> ParseMonad.PM (HsModuleI HsName.ModuleName QName [HsDecl])
extractLol x = fmap extractLol2 x

extractLol2 :: HsModuleI (SN HsName.ModuleName) (SN HsName.HsName) ds -> HsModuleI HsName.ModuleName QName ds
extractLol2 (HsModule a b c d e) = HsModule srcLoc name exportDecl importDecl modDecl
  where
    srcLoc = a :: SrcLoc
    name = HasBaseName.getBaseName b :: HsName.ModuleName
    exportDecl = (case c of
                    Nothing -> Nothing
                    Just a -> Just [newExport (head a)]) :: Maybe [HsExportSpecI HsName.ModuleName QName]
    importDecl = map newImport d :: [HsImportDeclI HsName.ModuleName QName]
    modDecl = e
    newExport :: (HsExportSpecI (SN HsName.ModuleName) (SN HsName.HsName)) -> HsExportSpecI HsName.ModuleName QName
    newExport (ModuleE m) = ModuleE (HasBaseName.getBaseName m)
    newExport (EntE i) = EntE (trans i)
    trans :: EntSpec (SN HsName.HsName) -> EntSpec QName
    trans i = case i of
      Var x -> Var x
      Abs x -> Abs x
      AllSubs x -> AllSubs x
      ListSubs y z -> ListSubs y z
    newImport :: HsImportDeclI (SN HsName.ModuleName) (SN HsName.HsName) -> HsImportDeclI HsName.ModuleName QName
    newImport (HsImportDecl o p q r s) = HsImportDecl t u v x y
      where
        t = o :: SrcLoc
        u = HasBaseName.getBaseName p :: HsName.ModuleName
        v = q :: Bool
        x = fmap HasBaseName.getBaseName r :: Maybe (HsName.ModuleName)
        y = fmap transform s :: Maybe (ImpSpec QName)
        transform :: ImpSpec (SN HsName.HsName) -> ImpSpec QName
        transform (True, x) = (True, fmap trans x)
        transform (False, x) = (False, fmap trans x)

tstModules (o,prog,args) = test flags0 args
  where
    test flags args =
        case args of
          "noprelude":args         -> test flags{prel=False}    args
          "cpp"      :args         -> test flags{cpp=Just cpp1} args
          ('c':'p':'p':'=':s):args -> test flags{cpp=Just s}    args

          "test"     :fs -> tstModules           =<< analyzePrg =<< expand fs
--          "create"   :fs -> do fs' <- expand fs
--                               createInterfaceFiles fs' =<< analyzePrg fs'
          "parse"    :fs -> do
            x <- expand fs
            y <- parsePrg x :: IO ([[HsModuleI HsName.ModuleName QName [HsDecl]]], [(HsName.ModuleName, WorkModuleI QName ParsedSyntax.Id)])
            z <- tstParse y
            return z
          "preparse" :fs -> tstParse'            =<< parseSrc   =<< expand fs
          "preparse0":fs -> const (return ())    =<< parseSrc   =<< expand fs
          "lex"      :fs -> tstLex               =<< readHFiles =<< expand fs
          "unlit"    :fs -> mapM_ putStr         =<< readHFiles =<< expand fs
          "defined"  :fs -> tstDefinedNames      =<< parseSrc   =<< expand fs
          "infixes"  :fs -> tstInfixes           =<< parseSrc   =<< expand fs
--          "scope"    :fs -> tstScope             =<< scopePrg   =<< expand fs
          "xrefs"    :fs -> do
             x <- expand fs
             let upg old = (let li = fmap (\(modName, wrkMod) -> (modName, (wrkMod, []))) (snd old)
                                in (fst old, li))
             y <- (fmap (upg) (parseProgram' flags (extractLol parse) x))
             let w = (ScopeProgram.scopeProgram' y)
             z <- createCrossRefs w
             return z
        --"update":[] -> ...
          _ -> fail "Usage: tstModules [+utf8] [+debug] [cpp[=<cmd>]] [noprelude] (test|create|parse|preparse|lex|unlit|defined|infixes|scope|xrefs) <files>"
      where
        analyzePrg = analyzeFiles' flags parse
        parseSrc = parseSourceFiles (cpp flags) parse
        parsePrg = parseProgram' flags (extractLol parse)

        tstModules = pput . ppAssoc . snd

        tstParse :: (Printable a0, Foldable t0) => (t0 [a0], b0) -> IO ()
        tstParse  = tstParse' . concat . fst
        tstParse' = pput . vcat

        tstDefinedNames = pput . definedNames
        tstInfixes = putStr . unlines . fmap (showl.getInfixes)

        tstScope (mss,_) = pput mss

        readHFiles = mapM readHaskellFile
        tstLex = mapM_ (print.lexerPass0)

    pput x = putStrLn $ ppu o x
--------------------------------------------------------------------------------

--createInterfaceFiles files (modss,amods) =
--    do optCreateDirectory "hi"
--       writeFile "hi/SourceFiles.txt" (unlines files)
--       writeModuleGraphInfo modss
--       mapM_ createInterfaceFile amods
--
--  where
--
--    createInterfaceFile (m,wm) =
--        writeFile (hiFile m) (render . vcat . interface . exports $ wm)
--
----    ent2Pair (m,n,s)         = (m,(n,s))
--    ent2Pair (Ent m n s)         = (m,(n,s))
--
--
--    interface = map iface . collectByFst . map (ent2Pair.snd)
--      where
--        iface (m,allns) = hsModule loc0 m (Just exports) ([],infixes)
--        -- :: HsModuleI (HsIdentI (Src HsName)) [HsDeclI (HsIdentI (Src HsName))]
--          where
--            exports     = toExp `map` oth
--            (subs,oth)  = partition (isSubordinate.snd) allns
--            toExp (n,ty)
--                | isClassOrType ty  = HsModule.EntE (HsModule.ListSubs n elems)
--                | otherwise         = HsModule.EntE (HsModule.Abs n)
--                where
--                elems = [x | (x,idty) <- subs, idty `belongsTo` getHSName n]
--
--            infixes = [Dec (HsInfixDecl loc0 f [qn']) |
--                        mods<-modss, mod<-mods, hsModName mod==m,
--                        (qn,f) <- getInfixes mod,
--                        let n = getQualified qn; -- Because of Prelude.: etc
--                                    qn' = mkUnqual n `asTypeOf` qn
--                        ]
--
--    hiFile (MainModule s) = hiDir++"/"++s++".hi"
--    hiDir = "hi" -- name of directory where interface files are put

--------------------------------------------------------------------------------

createCrossRefs (mss,mrs) =
    do optCreateDirectory "hi"
       writeModuleGraphInfo mss
       mapM_ writeRefs (simplifyRefsTypes mrs)
  where
    writeRefs (m,rs) = writeFile ("hi/"++m++".refs") (showl rs)

--- Module graph info ----------------------------------------------------------

writeModuleGraphInfo mss =
    do writeFile "hi/ModuleSourceFiles.hv" (show mfs)
       writeFile "hi/ModuleSourceFiles.txt" (txt2 mfs)
       writeFile "hi/Modules.txt" (showModuleNames mss)
       writeFile "hi/ModuleGraph.hv" (pp g)
       writeFile "hi/ModuleGraph.txt" (txtn g)
       writeFile "hi/ModuleGraphRev.hv" (pp revg)
       writeFile "hi/ModuleGraphRev.txt" (txtn revg)
  where
    mfs = fmap modfile (concat mss)
    g = moduleGraph mss
    revg = collectByFst [(i,m)|(m,is)<-g,i<-is]

    modfile m = (srcFile m,pp (hsModName m))

    txt2 = unlines . fmap (\(f,m)->m++" "++f)
    txtn = unlines . fmap (\(m,is)->pp m++" "++unwords (fmap pp is))

    -- One strongly connected component per line:
    showModuleNames = unlines . fmap (unwords . fmap (pp . hsModName))

    moduleGraph = fmap moduleImports . concat
    moduleImports m = (hsModName m,nub . fmap hsImpFrom . hsModImports $ m)

--- Pretty printing ------------------------------------------------------------

ppAssoc x = vcat . fmap cbn $ x
    where
    cbn (k,v) = k <> ":" <+> v

--ppResult = ppAssoc . snd

--- Utils ----------------------------------------------------------------------

showl x = unlines . fmap show $ x
