module FileNames(module FileNames,PackedString) where

import qualified PackedString as PS
import PackedString(PackedString,packString,unpackPS)
import ListUtil(assoc)
import GetEnv(getenvdef)

type PFilePath = PackedString
type Extension = PackedString

#ifdef __GLASGOW_HASKELL_
instance Show PackedString where
  showsPrec n ps = showsPrec n (unpackPS ps)
#endif
#ifdef __HBC__
#define reversePS reverse
#define appendPS append
#define nilPS nil
#define mapPS map
#endif

data FileType
  = Source | LitSource | Interface | StdInterface
  | Object | Executable | SharedObject | Archive
  | UnknownType Extension
  deriving (Eq,Ord,Show)

isLinkType t = t `elem` [Executable,SharedObject,Archive]

joinPath d f = d `PS.appendPS` slashPS `PS.appendPS` f -- !!!

dotPS = packString "."
slashPS = packString "/"
hsPS = packString "hs"
lhsPS = packString "lhs"
hiPS = packString hiExt
stdhiPS = packString stdhiExt
oPS = packString "o"
soPS = packString "so"
arPS = packString "a"

hiExt = getenvdef "hi" "HI_EXT"
stdhiExt = getenvdef "hi" "STDHI_EXT"

fileType t=
    assoc id (UnknownType t) types t
  where
    types = [(hsPS,Source),(lhsPS,LitSource),
	     (hiPS,Interface),(stdhiPS,Interface),
	     (oPS,Object),
             (PS.nilPS,Executable),(soPS,SharedObject),(arPS,Archive)]

extension t =
  case t of
    Source -> hsPS
    LitSource -> lhsPS
    Interface -> hiPS
    StdInterface -> stdhiPS
    Object -> oPS
    Executable -> PS.nilPS
    SharedObject -> soPS
    Archive -> arPS

extend Executable p = p
extend t p = p `PS.appendPS` dotPS `PS.appendPS` extension t

source       = extend Source
litSource    = extend LitSource
iface        = extend Interface
stdiface     = extend StdInterface
object       = extend Object
exe          = extend Executable
sharedObject = extend SharedObject
archive      = extend Archive

source' True = litSource
source' False = source

iface' True = stdiface
iface' False = iface

stripExt  = fst.splitExt

splitExt p = case break (=='.') (revUnpack p) of
               (e,'.':n) -> (revPack n, revPack e)
	       _ -> (p,PS.nilPS)

baseName = snd.splitPath

splitPath p = case break (=='/') (revUnpack p) of
                (base,'/':dir) -> (revPack dir, revPack base)
	        _ -> (PS.nilPS,p)

objectFile objdir p =
  case splitPath p of
    (dir,file) -> dir `joinPath` objdir `joinPath` object file

-- For hierachical libraries...
modulePath = PS.mapPS dot2slash
  where
    dot2slash '.' = '/'
    dot2slash c   = c

revPack = PS.reversePS . packString
revUnpack = unpackPS . PS.reversePS

--bla="bla" -- "bla"
