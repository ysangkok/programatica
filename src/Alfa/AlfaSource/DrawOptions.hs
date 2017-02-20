module DrawOptions(module DrawOptions,LayoutDirection(..),DispFunName,DeclDetail(..)) where
import UAnnots(DispFunName,DeclDetail(..),LayoutDirection(..))
import qualified UAbstract as U
import AlfLex(ismyAlphanum)
import Fonts(AlfFonts(..),fontsize)
import Fud(replace,argReadKey,argFlag)
import ListMap(lookupWithDefault)
import List(nub)
import Fixity
import qualified OrdMap as M
import Variables

type Map a b = M.OrdMap a b

class HasName a where
  name :: a -> Name
  nameStr :: a -> String

instance HasName U.Var where name (U.Var s) = Var s; nameStr (U.Var s) = s
instance HasName U.Con where name (U.Con s) = Con s; nameStr (U.Con s) = s
instance HasName U.Label where name (U.Label s) = Name s; nameStr (U.Label s) = s
instance HasName U.Sort where name (U.Sort s) = Name s; nameStr (U.Sort s) = s

instance HasName Name where
  name = id
  nameStr (Name s) = s
  nameStr (Con s) = s
  nameStr (Var s) = s

data Name
  = Name String -- obsolete, retained for backwards compatibility
  | Con String
  | Var String
  deriving (Eq,Ord,Show,Read)

data GlobalArgOptions
  = GlobalArgOptions
	{ hidingOn :: Bool,         -- argument hiding on/off
          argOptions :: [(Name,ArgOptions)]
	}
  deriving (Read)

data ArgOptions
  = ArgOptions
	{ hideCnt :: Int,	-- number of hidden arguments
	  idFixity :: Fixity,
	  displayAs :: Maybe String,
	  bitmapSource :: BitmapSource
	       -- more...
	}
  deriving (Read)
--  deriving (Eq)

data BitmapSource 
  = UseNormalFont
  | UseSymbolFont
  | UseImageFile
  deriving (Eq,Show,Read)

data ProofStyle = UglyProof | NDProof | BrorProof deriving (Eq)

data DrawOptions
  = DrawOptions
	{ fontsOpt :: AlfFonts,
          argOpts  :: GlobalArgOptions,
	  importedArgOpts :: Map Name ArgOptions,
	  compactOn :: Bool,
	  proofStyle :: ProofStyle,
	  hideTrivialLet :: Bool,
	  declDetail :: DeclDetail,
	  unfoldGoals, autoAbstract, onlyRefine, simpleRefine :: Bool,
	  layoutStyle :: LayoutDirection,
	  fontSize :: Int, -- 0..6
	  maxWidth :: Int
	}
--  deriving (Eq)

-- Start values ----------------------------------------------------------
argOptions0 = GlobalArgOptions True []
proofStyle0 = NDProof
drawOptions0 fonts = DrawOptions fonts argOptions0 M.empty True proofStyle0 False declDetail0 False autoAbstract0 onlyRefine0 simpleRefine0 Wide fontsize maxwidth0
defaultNormalArgOptions = ArgOptions 0 Nonfix Nothing UseNormalFont
defaultInfixArgOptions = ArgOptions 0 (Infix NonAssoc 0) Nothing UseNormalFont
-- (Record syntax is not used here, because of the risk of accidentally leaving
-- out a field and causing a run-time error rather than a compile-time error.)

declDetail0 = argReadKey "declview" CompleteDecls
-- Selectors: ------------------------------------------------------------

lookupArgOpts opts x =
    lookupWithDefault (argOptions $ argOpts opts) def1 n
  where
    n = name x
    def1 = M.lookupWithDefault (importedArgOpts opts) def2 n
    def2 = defaultArgOptions x

defaultArgOptions x = defaultArgOptions' . head . nameStr $ x

defaultArgOptions' c =
    if ismyAlphanum c
    then defaultNormalArgOptions
    else defaultInfixArgOptions

-- Update functions: -----------------------------------------------------
#define SET(f) (\ v r -> r { f=v })
#define FieldVar(f) (Vari { set=SET(f), get=f })
#define DefOptVar(f) f/**/Opt = FieldVar(f)

DefOptVar(compactOn)
DefOptVar(layoutStyle)
DefOptVar(proofStyle)
DefOptVar(hideTrivialLet)
DefOptVar(fontSize)
DefOptVar(declDetail)
DefOptVar(unfoldGoals)
DefOptVar(autoAbstract)
DefOptVar(onlyRefine)
DefOptVar(simpleRefine)
DefOptVar(argOpts)
hidingOnOpt = compVar argOptsOpt FieldVar(hidingOn)
idsOptionsOpt = compVar argOptsOpt FieldVar(argOptions)

setHiding name_n d@(DrawOptions {argOpts=a}) =
  d{argOpts=a{argOptions=replace name_n (argOptions a)}}

setArgOptions ao d = d{argOpts=ao}
updImportedArgOpts upd d@(DrawOptions {importedArgOpts=iao}) =
  d{importedArgOpts=upd iao}

mergeHiding imported gao@(GlobalArgOptions _ ht) =
    if imported
    then updImportedArgOpts upd
    else setArgOptions gao
  where upd oldht = foldr M.add oldht ht

-- Defaults:
maxwidth0 = argReadKey "maxwidth" 500 :: Int
autoAbstract0 = argFlag "autoabstract" gfMode
onlyRefine0 = argFlag "onlyrefine" gfMode
simpleRefine0 = argFlag "simplerefine" False
gfMode = argFlag "GF" False -- Used also in GeditOptions
