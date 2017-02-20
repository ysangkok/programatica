module MakeF(makeF) where
import Prelude hiding (FilePath,IOError)
import Fudgets hiding (Time)
import DependencyF
import ParallelCompileF
import ModuleListF
import DependencyGraph(Graph)
import FileNames
import ModuleIds(ModuleName(..))
import Project
import GetEnv
import ListUtil(chopList,breakAt)
import Utils2(mix)
import HO(apSnd)
import PackedString(unpackPS,packString)
import qualified ContinuationIO as CIO

makeF =
    stubF $
    loopF (vBoxF (moduleListF >+< (parallelCompileF hosts>=^<apSnd hbc))
	   >==<
	   depF)
      >==< emacsF
  where
    depF = dependencyF proj0 batch
    --quitIfBatchF = quitIdF ((batch&&) . allDone)
    proj0 = project0 targets0 srcdirs0 libdirs0 objdir0 ignimps0
    emacsF = if emacs
             then hIOSuccF (CIO.AppendChan CIO.stdout "Ready\n") $
	          Left .(:[]).modName >^=< delayItF >==< inputLinesSP >^^=< stdinF
	     else nullF
    --delayItF = getF $ \ msg -> hIOSuccF (CIO.Sleep 1) $ putF msg delayItF -- !!
    delayItF = idF
    targets0 = map modKind args
    srcdirs0 = paths (argKey "srcdirs" def_srcdirs)
    libdirs0 = paths (argKey "libdirs" "")++complibs0
    complibs0 = paths (getenvdef "" "COMPILER_LIBS")
    ignimps0 = map packString (paths (argKey "ignoreimports" ""))
    objdir0 = packString (getenvdef "." "OBJ_DIR")
    par = argReadKey "par" 1
    batch = argFlag "batch" False
    def_hosts = mix (replicate par "localhost") ":"
    hosts = paths (argKey "hosts" def_hosts)
    hbcflags = getenvdef "" "COMPILERFLAGS"
    ldflags = getenvdef "" "LDFLAGS"
    def_srcdirs = "."
    modName = fst.modKind
    modKind = apSnd fileType.splitExt.baseName.packString

    hbc (p@project,cmd) =
      case cmd of
        Compile lit modname ->
	  unwords ([hucompile,"-c"] ++
		   (if objd/="." then ["-objdir",objd] else []) ++
		   [hbcflags] ++
		   iflags ++
		   [unpackPS (source' lit modname)])
	Link t main objs ->
	  unwords ([hulink,unpackPS (extend t main)]++
	          (if t==Executable then hbcflags:iflags else []) ++
	           map (unpackPS.objectFile (objdir p)) objs ++
		   if t==Executable then [ldflags] else [])
      where
        objd = unpackPS (objdir p)
	iflags = reverse ["-i"++inc | inc<-srcdirs p++libdirs p, inc `notElem` complibs0 ]

paths = filter (not.null) . chopList (breakAt ':')

hucompile = getenvdef "humakehbc" "HUCOMPILE"
hulink = getenvdef "hulink" "HULINK"

emacs = getEnvi "HUMAKE_EMACS" /= Nothing
