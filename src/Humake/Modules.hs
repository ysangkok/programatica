module Modules where
import FileNames
import ModuleIds
import Time(ClockTime)
import qualified IntSet as IS
import IntSet(IntSet)
#ifdef __HBC__
#define nilPS nil
#endif
import qualified PackedString as PS (nilPS)
--import Trace(trace)

data Module = Module { modName::ModuleName,
		       modStatus::Status,
		       modPath::PFilePath,
		       modIsLiterate::Bool,
		       modDates::FileDates,
		       modImports::Imports }

{- Finally you can get rid of this kind of boring definitions!
modName    (Module name _ _ _ _) = name
modImports (Module _ _ _ _ imports) = imports
modStatus  (Module  _ status _ _ _) = status
modDates   (Module _ _ _ dates _) = dates
modPath    (Module _ _ path _ _) = path
-}

setStatus m status = m { modStatus=status }
setDates m dates = m { modDates=dates }

type ModuleIdSet = IntSet
mEmpty = IS.empty :: ModuleIdSet
mElem = IS.elem

type Imports = ModuleIdSet -- import list
type References = ModuleIdSet -- where this module is imported

data Status
  = Unknown     -- file dates have not been checked yet
  | Nonexistent -- module does not exist
  | InError     -- object outofdate and compilation errors
  | OutOfDate   -- object file outofdate wrt source or imported interfaces
  | Good        -- object file uptodate wrt source and imported interfaces
  | Library     -- a library module, which is assumed to be uptodate
  deriving (Eq,Ord)

isOk Good = True
isOk Library = True
isOk _ = False

data FileDates
   = FileDates { sourceDate, ifaceDate, objectDate::FileDate }
   deriving (Show)

data FileDate = Never | At ClockTime deriving (Eq,Ord,Show)

oDate = objectDate.modDates

nonexistentModule m =
  Module m Nonexistent PS.nilPS False (FileDates Never Never Never) mEmpty

existingModule m path lit dates@(FileDates s i o) imports =
    Module m status path lit dates (IS.fromList imports)
  where
    status = if s>o then OutOfDate else Good

libModule m path dates = Module m Library path False dates mEmpty

setOutOfDate mod =
  if modStatus mod == InError
  then mod
  else setStatus mod OutOfDate
