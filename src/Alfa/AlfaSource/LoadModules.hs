module LoadModules(loadModuleK,browseModuleK,importModuleK,abschildpath) where
import Fud
import List(sortBy)
import FilePathUtils(absolute,abschildpath)

import AlfEditKs
import EditMonad(EdM,liftEitherEd)
import AlfEditMonad(AlfEdM,edPE,isImportedEd,moveToImportsEd,dropThisModuleEd,loadAlfStateEd)
import AlfModules
--import FileConv(parseModule)
import ParseModule(parseModule)
import AlfOps(loadParsedModule)
import FileMenuF(libDir)
--import Load

import AlfSyntax(Syntax(..),IsSyntax(..))
import ProofEngine() -- Error

loadModuleK = runEdK . loadModuleEdK

browseModuleK importer filename =
    runEdK $
    do absfilename <- findModule filename [dir,aLibDir]
       sa <- loadModuleEdK' False True absfilename
       return (absfilename,sa)
  where
    dir = pathHead (aFilePath importer)

importModuleK parent filename syntax =
  runEdK (importModuleEdK parent filename syntax)

loadModuleEdK = loadModuleEdK' False False
loadImportedModuleEdK = loadImportedModuleEdK' False
loadImportedModuleEdK' = loadModuleEdK' True

loadModuleEdK' :: Bool -> Bool -> FilePath -> AlfEdKs i o String Syntax
loadModuleEdK' imported browse filename =
    (`handleEdK` \ msg -> errorEdK (filename++":\n"++msg)) $
    do str <- readFileEdK filename
       --let (annots,file) = splitAnnots str
       --m0 <- gKs (parseModule filename str)
       m0 <- edKs (parseModule imported filename str)
       let (m1,annots) = splitModuleAnnots filename m0
	   imports = moduleImports m1
       loadImports browse dir imports -- should get the layout options!!
       badannots <- edKs (loadAlfStateEd filename imported m1 annots)
       let m = appendModuleAnnots m1 badannots
       if browse
         then return (syn m)
	 else echoEdK filename >> edKs (loadParsedModule imported m)
  where
    dir = pathHead (aFilePath filename)
    --ifNotBrowse k = if browse then return () else k

loadImports browse dir = foldr ((>>).loadImport browse dir) (return ())

--loadImport :: Bool -> AFilePath -> Import -> AlfEdKs i o String ()
loadImport browse dir (Import relfilename) =
    do absfilename <- findModule relfilename [dir,aLibDir]
       yes <- edKs (isImportedEd absfilename)
       if yes
        then return ()
        else do loadImportedModuleEdK' browse absfilename
	        edKs (moveToImportsEd absfilename)

findModule file [] = errorEdK ("Can not find imported module: "++file)
findModule file (d:ds) =
  do let absfilename = filePath (absolute d file)
     do readFileEdK absfilename
	return absfilename
      `handleEdK` \ _ -> findModule file ds

importModuleEdK parent absfilename s@(ModuleS m) =
  if i `elem` moduleImports m
  then errorEdK ("Module "++filename++" already imported")
  else do yes <- edKs (isImportedEd absfilename)
          if yes
           then return (ModuleS mod')
           else do edKs dropThisModuleEd
	           loadImportedModuleEdK absfilename
	           edKs $ moveToImportsEd absfilename
	           edKs $ loadParsedModule True mod'
  where
    filename = head . sortByLength . map (`relative` absfilename) $ [dir,aLibDir]
      where
        sortByLength = sortBy cmpLen
	cmpLen p1 p2 = compare (length p1) (length p2)
    dir = pathHead (aFilePath parent)
    --mod' = Module (ImportDecl i:decls) -- !!
    mod' = appendImport i m
    i = Import filename
    --is' = is++[i]

--If an imported file is in the same (or a sub-) directory, use a relative path.
relative dir relfilename =
    if isAbsolute file && isAbsolute dir    -- paranoid!
    then filePath (file `pathRelativeTo` dir)
    else filePath file
  where file = absolute dir relfilename

----

gKs = edKs . edPE . liftEitherEd

---

aLibDir = aFilePath libDir
