module Alfa.UAnnots where
import MetaVars

-- Source file positions for use in error messages:
data Position
  = Position String Int Int     -- file name, line, column
  deriving (Eq,Show,Read)

noPosition = Position "" (-1) (-1)


-- Quick hack to ingore certain annotations in equality test on expressions:
newtype NoEq a = N a
instance Eq (NoEq a) where _ == _ = True
instance Show a => Show (NoEq a) where showsPrec n (N x) = showsPrec n x
instance Read a => Read (NoEq a) where
  readsPrec n s = [(N x,r)|(x,r)<-readsPrec n s]

was = Was . N
asTextBy = AsTextBy . N
altViewsBy = AltViewsBy . N

data LayoutDirection = Wide | Tall deriving (Eq,Show,Read)

type DispFunName = (String,String) -- (plugin name, disp function name)

data UAnnot exp bindings
  = UnfoldTo exp
  | FoldTo exp
  | Was (NoEq MetaVar)
  | InEnv bindings
  | AsTextBy (NoEq DispFunName)
  | AltViewsBy  (NoEq [DispFunName])
  | LayoutDir LayoutDirection -- Use alternate layout for the annotated expression.
  | EComment String -- Comment added by user
  | EPos (NoEq Position) -- position in text file, as provided by the parser
  | Hidden
--  | Dummy
--  | CharLit
  -- more...
  deriving (Eq,Show,Read)

prAnnot (LayoutDir Wide) = "{-#h#-}"
prAnnot (LayoutDir Tall) = "{-#v#-}"
prAnnot Hidden = "{-#H#-}"
prAnnot (EComment s) = s
prAnnot (AsTextBy (N n)) = "{-#t"++show n++"#-}"
prAnnot (AltViewsBy (N ns)) = "{-#a"++show ns++"#-}"
--prAnnot CharLit = "{-#c#-}"
prAnnot _ = ""

rdAnnot "h#-}" = Just (LayoutDir Wide)
rdAnnot "v#-}" = Just (LayoutDir Tall)
rdAnnot "H#-}" = Just Hidden
rdAnnot ('t':r) =
  case reads r of
    [(n,"#-}")] -> Just (asTextBy n)
    _ -> Just (EComment ("{-#a"++r)) -- !!
rdAnnot ('a':r) =
  case reads r of
    [(ns,"#-}")] -> Just (altViewsBy ns)
    _ -> Just (EComment ("{-#a"++r)) -- !!
rdAnnot _ = Nothing


data DeclDetail
  = JustNames | NamesAndTypes | CompleteDecls
  | DeclAsTextBy DispFunName
  | DeclAltViewsBy [DispFunName]
  deriving (Eq,Show,Read)

prDetailAnnot d = "--#"++prDetail d

prDetail JustNames = "N"
prDetail NamesAndTypes = "S"
prDetail CompleteDecls = "C"
prDetail (DeclAsTextBy n) = "t"++show n
prDetail (DeclAltViewsBy ns) = "a"++show ns

rdDetailAnnot ('-':'-':'#':s) = rdDetail s
rdDetailAnnot _ = Nothing

rdDetail "N" = Just JustNames
rdDetail "S" = Just NamesAndTypes
rdDetail "C" = Just CompleteDecls
rdDetail ('t':s) =
  case reads s of
    (n,_):_ -> Just (DeclAsTextBy n)
    _ -> Nothing
rdDetail ('a':s) =
  case reads s of
    (ns,_):_ -> Just (DeclAltViewsBy ns)
    _ -> Nothing
rdDetail _ = Nothing
