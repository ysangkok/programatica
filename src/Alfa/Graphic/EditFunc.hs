module EditFunc where
import Fud(Gfx,DPath)
import HO(apFst)
import EditMonad
import Annot


newtype EditMenu u s a e
  = EditMenu [(Either String (EditFunc u s a e),(String,Gfx))]

--type Menu u s a e = [(EditFunc u s a e,(String,Gfx))]

--old: type Mod s a = s->a->Either String (s,a)
--old: type Modf s a b = s->a->Either String (s,b)

type Mod s a = a->EdM s String a
type Modf s a b = a->EdM s String b

type Mod2 u s a = Either (Mod s (ADrawing u a)) (Mod s a)

--type Mod3 u a = ADrawing u a -> Mod s (Either (ADrawing u a) a)

data EditFunc u s a e
    -- edits can be local to the selected node
  = EditLocal (Mod2 u s a)

     -- edits can be applied to the parent node and thus affect siblings
     -- (useful when adding/deleting/moving items in a list)
  | EditParent (DPath->Mod2 u s a)

    -- edits can have local effects plus effects on place holders
  | EditPlaceHolders (Mod s [(DPath,a)])
    -- input = selected node + placeholders
    -- output = edited selected node + changed place holders

    -- edits can have global effects
  | EditGlobal (DPath->ADrawing u a->Modf s a [(DPath,a)])
    -- the function will be applied to
    --    the path of the selected node,
    --    the complete drawing (the root)
    -- and produce a modifier that will be applied to the root.

    -- edits can be made by sending a message to an external fudget
    -- atomicity?
  | EditExternal e

    -- Just show a error or success message
  | EditShowMessage (Either String String)

    -- Show a new menu
  | EditShowMenu (EditMenu u s a e)
  --deriving (Show) -- for debugging only

--mapEdFunc = map . apFst . mapEither id
mapEdFunc = map . apFst

editNop = editLocalAbs (pureMod id)
--editError = editLocalAbs . const . errorEd
editError = editShowMessage . Left
editLocalAbs = EditLocal. Right
editLocalDrawing = EditLocal. Left
editParentAbs = EditParent . (Right .)
editPlaceHolders = EditPlaceHolders
editExternal = EditExternal
editShowMessage = EditShowMessage
editShowMenu = EditShowMenu
editGlobal = EditGlobal

pureMod :: (a->a) -> Mod s a
pureMod f = returnEd.f
--old: pureMod f s x = Right (s,f x)
