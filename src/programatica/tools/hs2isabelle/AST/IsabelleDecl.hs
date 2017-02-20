module IsabelleDecl where
import IsabelleType(Type,Sort,TypePattern,DomainType,showSort,showInstance)
import IsabelleTerm(HBind(..))
import IsabelleProp(PredDecl,PropDecl)
import Mixfix
import Char(isAlpha)

-- For top level declarations:

data IsaDecl
  = IsaTypes TypePattern Type
  | IsaDomain [DomainType]
  | IsaConsts [TypeSig]
  | IsaInfix String [String]
  | IsaFixrec [HBind]
  | IsaTypeSig [String] Type
  | IsaClass String Sort [HBind]
  | IsaInstance Type [HBind]
  | IsaPredDecl PredDecl
  | IsaPropDecl PropDecl
  | IsaComment String

data Module = Module (String, [String]) [IsaDecl]

data TypeSig = TypeSig String Type

------------------------------------------------------------

showHBind x = case x of
    HBindPat e -> showLine e
    HBindFun ms -> showAll (map showLine ms)
  where
    showLine = showIndentLine . showQuotes . showHBrackets . shows

showFixity f i = showIndentLine $
  showQuotes (showString ("_hs_" ++ encode_op i)) .
  showString (" :: hs_qop" ++ f) .
  showSpace . showParens (showQuotes (showString i))

showTranslation (TypeSig name _)
  | isAlpha (head name) = id
  | otherwise =
    showString "translations\n" .
    showIndentLine (
      showString "\"_HsQ (_HsVarSym _hs_" . showString (encode_op name) .
      showString ")\" == " . showQuotes (showString ("hs_" ++ encode_op name)))

instance Show TypeSig where
  showsPrec _ (TypeSig name typ) = if isAlpha (head name)
    then showString name . showString " :: " . showType typ
    else showString ("hs_" ++ encode_op name) . showString " :: " . showType typ
    where
      showType = showQuotes . showHBrackets . shows

instance Show IsaDecl where
  showsPrec p x = case x of
    IsaTypes lhs rhs ->
      mixfix "types _ = _\n"
        [shows lhs, showQuotes (showHBrackets (shows rhs))] 1000 p
    IsaDomain ds ->
      showString "domain\n" .
      showSep (showString "and\n") (map (showIndentLine . shows) ds)
    IsaConsts sigs ->
      showString "consts\n" . showAll (map (showIndentLine . shows) sigs) .
      showAll (map showTranslation sigs)
    IsaFixrec bs ->
      showString "fixrec\n" . showSep (showString "and\n") (map showHBind bs)
    IsaTypeSig is t -> id -- showLR "(*\n" "\n*)" (shows (is, t))
    IsaClass c s _ ->
      showString "axclass " . showString c . showString " < " .
      showSort s . showString "\n"
    IsaInstance t _ ->
      showString "instance " . showInstance t . showString " ..\n"
    IsaPropDecl d -> shows d
    IsaPredDecl d -> shows d
    IsaInfix f is ->
      showString "syntax\n" . showAll (map (showFixity f) is)
    IsaComment s -> showLR "(*\n" "*)\n" (showString s)

instance Show Module where
  showsPrec p (Module (name,imports) decls) =
    showString "theory " . showString name .
    showString "\nimports " . showSpaceSep (map showString imports) .
    showString "\nbegin\n\n" .
    showSep (showString "\n") (map shows decls) .
    showString "\nend\n"
  
------------------------------------------------------------
-- for testing
{-
a' = tVar "a"
b' = tVar "b"
c' = tVar "c"
d' = tVar "d"
-}
--a' = TVar "a" (Sort ["cpo"])

{-
domain 'a tree = Leaf (value :: "'a") | Node (children :: "'a forest")
and 'a forest = Empty | Trees (head :: "'a tree") (tail :: "'a forest")
-}
{-
tree_forest :: IsaDecl
tree_forest = IsaDomain [tree, forest]
  where
    a_tree = TType "tree" [a']
    a_forest = TType "forest" [a']
    tree = DomainType a_tree [leaf, node]
    leaf = DomainCons "Leaf" [DomainArg Lazy "value" a']
    node = DomainCons "Node" [DomainArg Lazy "children" a_forest]
    forest = DomainType a_forest [empty, trees]
    empty = DomainCons "Empty" []
    trees = DomainCons "Trees"
      [DomainArg Lazy "" a_tree, DomainArg Strict "" a_forest]
-}