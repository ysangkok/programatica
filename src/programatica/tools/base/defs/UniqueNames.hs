{-# LANGUAGE UndecidableInstances #-}
module UniqueNames where
import SrcLoc(SrcLoc(..),loc0,HasSrcLoc)
import SrcLocPretty()
import HsName (Id,ModuleName)
import PrettyPrint
import PrettyUtil(ppqIfDebug)
import HasBaseName
--import NoEq
import qualified SrcLoc as L
import Data.Maybe(fromMaybe)

-- Types to decorate identifiers to make them unique

data Orig
  = L Int    -- for unique names generated in the scope pass
  | G ModuleName Id OptSrcLoc -- unique top level names (original names)
  | D Int OptSrcLoc -- for unique variables generated by the type checker
  | S SrcLoc -- for names made unique by their defining occurence in the source
  | Sn Id SrcLoc -- unique by name + a source location
-- | I ModuleName SrcLoc -- for names of instances introduced by the type checker
  | P        -- just for pretty printing
  deriving (Eq,Ord,Show,Read)

newtype OptSrcLoc = N (Maybe SrcLoc) -- deriving (Show)
noSrcLoc = N Nothing
srcLoc = N . Just
optSrcLoc = N
instance Eq  OptSrcLoc where _ == _ = True
instance Ord OptSrcLoc where compare _ _ = EQ
instance Show OptSrcLoc where showsPrec _ _ = id
instance Read OptSrcLoc where readsPrec _ s = [(N Nothing,s)]

data PN i = PN i Orig   deriving (Show,Read)

instance HasSrcLoc (PN i) where srcLoc = fromMaybe loc0 . optLoc'

optLoc = N . optLoc'
optLoc' (PN i o) =
  case o of
    G m n (N optp) -> optp
--  I m p -> Just p
    S p -> Just p
    D n (N optp) -> optp
    _ -> Nothing

class Unique n where unique :: ModuleName -> n -> Orig

class HasOrig n where orig :: n -> Orig
instance HasOrig (PN i) where orig (PN i o) = o

origModule n = fromMaybe err (optOrigModule n)
  where err = error $ "Bug: UniqueNames.origModule "++show n -- hmm

optOrigModule n =
  case orig n of
    G m _ _-> Just m
--  I m _ -> Just m
    _ -> Nothing

instance HasBaseName (PN i) i where getBaseName (PN i _) = i

instance          Eq  (PN i) where PN _ p1==PN _ p2 = p1==p2
instance          Ord (PN i) where compare (PN _ p1) (PN _ p2) = compare p1 p2

instance Functor PN where fmap f (PN i o) = PN (f i) o

x `eqSrc` y = getBaseName x == getBaseName y

---

instance Printable i => Printable (PN i) where
  ppi (PN i o) = i<>o
  wrap (PN i o) = wrap i<>o

instance PrintableOp i => PrintableOp (PN i) where
  isOp (PN i n) = isOp i
  ppiOp (PN i n) = ppiOp i<>n

instance Printable Orig where
  ppi (D n (N s)) = ppi (subnum n)<+>ppqIfDebug s
  ppi (S p) = ppqIfDebug p
  ppi (G m _ _) = ppqIfDebug m
  --ppi (Sn n (SrcLoc f r c)) = "«"<>r<>","<>c<>"»"
  ppi _ = empty

subnum n = ppIfUnicode (subdigs (show n)) n
  where
    subdigs ('-':s) = toEnum 0x208b:subdigs' s
    subdigs s = subdigs' s
    subdigs' = map subdig
    subdig :: Char->Char
    subdig = toEnum . (+0x2050) . fromEnum

