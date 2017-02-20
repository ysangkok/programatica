module URL(
  URL(..),Proto(..),Host(..),Port(..),Fragment(..),
  relativeURL,fragmentURL,url2str,joinURL,
  docURL,urlHost,urlPath,fragment,
  sameDoc) where
--import Fudgets (ctrace,Host(..),Port(..))
import Utils2(unmix,mix,(+++))

{-
#ifdef __HASKELL98__
#define map fmap
#endif
-}

data URL = URL (Maybe Proto) (Maybe Host) (Maybe Port) FilePath (Maybe Fragment)
           deriving (Eq,Ord,Show,Read)

type Proto = String
type Host = String
type Port = Int
--type Path = String -- use Prelude.FilePath instead
type Fragment = String

relativeURL path = URL Nothing Nothing Nothing path Nothing
fragmentURL fragment = URL Nothing Nothing Nothing "" (Just fragment)

url2str (URL proto host port path fragment) =
    protostr++hoststr++portstr++path++fragmentstr
  where protostr = opt proto (++":")
        hoststr = opt host ("//"++)
	portstr = opt port ((':':).show)
        fragmentstr = opt fragment ('#':)

	opt m f = maybe "" f m


docURL (URL proto host port path fragment) = URL proto host port path Nothing
sameDoc url1 url2 = docURL url1 == docURL url2
fragment (URL _ _ _ _ fr) = fr
urlHost (URL _ host _ _ _) = host
urlPath (URL _ _ _ path _) = path

joinURL parent child = {-ctrace "joinURL" (parent,child,res)-} res
  where res = joinURL' parent child
joinURL' (URL proto host port ppath pfragment) child =
  case child of
    URL (Just "news") _ _ _ _ -> child
    URL (Just "mailto") _ _ _ _ -> child
    URL (Just "file") _ _ _ _ -> child
    URL cproto chost cport cpath cfragment ->
	URL (cproto +++ proto)
	    (chost +++ host)
	    (if chost==Nothing then cport +++ port else cport)
	    (joinpath' ppath cpath)
	    cfragment
      where
        joinpath' ppath cpath=
	  case (cpath,cfragment) of
	    ("",Just _) -> ppath
	    _ -> joinpath ppath cpath

joinpath ppath cpath =
  case cpath of
    '/':_ -> cpath
    '?':_ -> (fst.break (=='?')) ppath++cpath
    _     -> compresspath (parent ppath++cpath)
  where
    parent = reverse . snd . break (=='/') . reverse

    compresspath = flip mix "/"  . rmdotdot . rmdot . unmix '/'
    rmdot (".":ns@(_:_)) = rmdot ns
    rmdot (n:ns) = n:rmdot ns
    rmdot [] = []

    rmdotdot ("..":ns) = "..":rmdotdot ns -- leading ".." can not be removed
    rmdotdot (n:"..":ns) = rmdotdot ns -- n/=".." thanks to previous line
    rmdotdot (n:ns) = n:rmdotdot ns
    rmdotdot [] = []
