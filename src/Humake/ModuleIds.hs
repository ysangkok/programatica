module ModuleIds(ModuleIds,ModuleId,ModuleName,emptyIds,moduleId,moduleIds,moduleName) where
import qualified IntMap as IM -- (IntMap,iEmptyMap,iAddItem,iAssocDef)
import IntMap(IntMap)
import qualified OrdMap as OM
import OrdMap(OrdMap)
import PackedString(PackedString)

data ModuleIds = MI Int (ModuleIdMap ModuleName) (OrdMap ModuleName ModuleId)

--newtype ModuleId = M Int
type ModuleId = Int

type ModuleName = PackedString -- module name

firstId = 0 :: Int
nextId = (+1) :: (Int->Int)

type ModuleIdMap a = IntMap a

emptyIds = MI firstId IM.empty OM.empty

moduleId name ids@(MI next map table) =
    case OM.lookup name table of
      Just id -> (id,ids)
      _       -> ({-M-} next,MI (nextId next)
	                        (IM.add (next,name) map)
				(OM.add (name,{-M-} next) table))

moduleIds [] ids = ([],ids)
moduleIds (n:ns) ids =
  case moduleId n ids of
    (i,ids') ->
      case moduleIds ns ids' of
        (is,ids'') -> (i:is,ids'')

moduleName ({-M-} id) ids@(MI _ map _) =
    IM.lookupWithDefault map e id
  where
    e = error "bug in ModuleIds.moduleName"
