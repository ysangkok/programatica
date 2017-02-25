module FuppletFetchF(fuppletFetchF) where
import AllFudgets
import URLFetchF
import Http
import URL(URL) --(..),url2str)
import ParFetchF
import CacheF
import Utils2(mix)
import DialogueIO
import Prelude hiding (IOError)
import IOUtil(getEnvi)
import Data.Maybe(fromMaybe)

default(Int)

fuppletFetchF =
    cacheF (filterRightSP>^^=<
            idRightF logF>==<
	    respToFuppletF>==<
	    (shmsg>^=<parFetchF parallel urlFetchF'))
   where
     shmsg (Left (n,s)) = Left (unwords [show n,s])
     shmsg (Right x) = Right x

urlFetchF' = urlFetchF >=^< req
  where
    req url = HttpReq url HttpGet acceptHdrs
    acceptHdrs = [("Accept","text/"++ttype) | ttype<-["plain","x-haskell"]]

respToFuppletF =
    ltF compileAndLoadF >==< ltF (concatMapF extr)
  where
    extr (url,ans) = extr' ans
      where
        extr' (Left msg) = [Left ("Error: "++msg),Right (url,Nothing)]
	extr' (Right r) = [Right (url,Just (respBody r))]

compileAndLoadF = ioF clK
  where
    clK = getHigh $ \ (url,msg) ->
      let errK =
	      putHigh (Left msg) $
	      putHigh (Right (url,labelF msg)) $
	      clK
	    where msg = "Could not fetch the fupplet from the URL."

	  errMsgK msg e =
	    putHigh (Left msg) $
	    putHigh (Left (show e)) $
	    putHigh (Right (url,labelF msg)) $
	    clK

	  srcK src =
	    createDirK $ \ unique dir ->
	    storeK dir (fuppletFile unique) (fupple unique src) $
	    storeK dir (wrapperFile unique) (wrapper unique) $
	    compileK unique dir errMsgK $
	    loadK unique dir errMsgK $ \ fupplet ->
	    putHigh (Right (url,fupplet)) $
	    clK
      in maybe errK srcK msg

--loadK :: String -> String -> (String->D_IOError->K i o) -> (a->K i o) -> K i o
loadK u dir errK succK =
    sIOerr (DLOpen (dir++"/"++sharedObj u)) errK1 $ \ (DLHandle h) ->
    sIOerr (DLSym h sym) errK2 $ \ (DLVal dlval) ->
    succK (unsafeGetDLValue dlval)
  where
    sym = "C_Wrapper"++u++"$__fuppletF"
    errK1 = errK "Unable to load the fupplet object file"
    errK2 = errK "Unable to find the fupplet in the loaded object file"

compileK u dir errK succK =
    hIOerr (System compileCmd) errK1 (const succK)
  where
    errK1 = errK "Compilation of the fupplet failed"
    compileCmd = mix cmds "&& "
    cmds = map unwords
            [["cd",dir],
             ["hbcxmake",wrapperFile u],
	     [buildshlib,"\"\"",sharedObj u,wrapperObj u,fuppletObj u]]

createDirK cont =
  hIO GetLocalTime $ \ (Dbl t) ->
  let unique = show (round (100.0*t) `mod` 99999999)
      --dir = "/tmp/wwwbFupplet"++unique
      dir = "/tmp/wwwbFupplet_"++user
  in haskellIO (StatusFile dir) $ \ resp ->
     case resp of
       Str ('d':_) -> cont unique dir
       Str s -> error ("Fupplet directory "++dir++" status: "++s)
       _ -> hIOSucc (CreateDirectory dir "0700") $
            cont unique dir

storeK dir file s = hIOSucc (WriteFile (dir++"/"++file) s)

logF =  if fuplog
        then shellF "Fupplet fetch log" $ terminalF defaultFont 24 80
	else nullF

ltF f = merge>^=<idLeftF f
  where
    merge (Left x) = Left x
    merge (Right y) = y

user = fromMaybe "nobody" $ getEnvi "USER"
lmlDir = fromMaybe "/usr/local/lib/lmlc" $ getEnvi "LMLDIR"
buildshlib = lmlDir++"/bin/buildshlib"
wrapperFile u = wrapperBase++u++".hs"
fuppletFile u = fuppletBase++u++".hs"
wrapperObj u = wrapperBase++u++".o"
fuppletObj u = fuppletBase++u++".o"
sharedObj u = "fupplet"++u++".so"
wrapperBase = "Wrapper"
fuppletBase = "Fupplet"

fupple u src =
  case lines src of
    l:ls ->
      case words l of
        "module":name:rest -> unlines (unwords ("module":(name++u):rest):ls)
	_ -> src -- failed to make module name unique!!
    _ -> src -- failed to make module name unique!!

wrapper u =
  unlines [
    "module Wrapper"++u++" where",
    "import Fudgets",
    "import qualified Fupplet"++u++" as Fup",
    "fuppletF = stubF Fup.fuppletF" ]

parallel = argReadKey "parallel" 2 :: Int
fuplog = argFlag "fuplog" False
