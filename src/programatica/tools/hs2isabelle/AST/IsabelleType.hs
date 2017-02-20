module IsabelleType where
import Maybe (fromMaybe)
import Mixfix

type Class = String
data Sort = Sort [Class] | DefaultSort

data Type
  = TVar String Sort
  | TType String [Type]

data TypePattern = TypePattern Type

-- Datatype declarations

data Strictness = Lazy | Strict
data DomainArg = DomainArg Strictness String Type
data DomainCons = DomainCons String [DomainArg]
data DomainType = DomainType TypePattern [DomainCons]

------------------------------------------------------------

tVar a = TVar a DefaultSort

addClass :: Class -> Sort -> Sort
addClass c DefaultSort = Sort [c]
addClass c (Sort cs) = Sort (c:cs)

addConstraint :: (String, Class) -> Type -> Type
addConstraint x t = fromMaybe t (addC x t)
  where
    addC (a,c) (TVar b s)
      | a == b    = Just (TVar b (addClass c s))
      | otherwise = Nothing
    addC x (TType n ts) = fmap (TType n) (addC' x ts)
    addC' x [] = Nothing
    addC' x (t:ts) = case addC x t of
      Nothing -> fmap (t :) (addC' x ts)
      Just t' -> Just (t' : ts)

addContext :: [(String, Class)] -> Type -> Type
addContext cs t = foldr addConstraint t cs

tApp (TType s ts) t = TType s (ts ++ [t])
tApp t _ = error ("non-exhaustive pattern: tApp " ++ show t)

foldr1' :: (a -> a -> a) -> a -> [a] -> a
foldr1' f z [] = z
foldr1' f z xs = foldr1 f xs

tFun t t' = TType "->" [t,t']
tList t = TType "[]" [t]

checkTp :: Type -> ()
checkTp (TType c ts) = checkTa ts
  where checkTa [] = ()
        checkTa (TVar _ _ : ts) = checkTa ts

------------------------------------------------------------

showSort :: Sort -> ShowS
showSort s = case s of
  DefaultSort -> showString "pcpo"
  Sort [x] -> showString x
  Sort xs -> showQuotes (showBraces (showCommaSep (map showString xs)))

showInstance :: Type -> ShowS
showInstance (TType c [TType s ts]) =
  showString (name s) . showString " :: " . showCs ts . showString c
  where
    name "[]" = "List"
    name "->" = "\"->\""
    name ('(':x) = "Tuple" ++ show (length x)
    name s = s
    showCs [] = id
    showCs xs = showTuple (map showC xs) . showSpace
    showC (TVar _ s) = showSort s

instance Show Sort where
  showsPrec p DefaultSort = id
  showsPrec p (Sort xs) = showString "::" . case xs of
    [x] -> showString x
    xs -> showBraces (showCommaSep (map showString xs))

instance Show Type where
  showsPrec p x = case x of
    TVar a s -> showChar '\'' . showString a . shows s
    -- special case for function space
    TType "->" [t,t'] ->
      mixfix "_ \\<rightarrow> _" [showsPrec 1 t, showsPrec 0 t'] 0 p
    -- special case for list constructor
    TType "[]" [t] -> showSquares (shows t)
    -- special case for tuple constructors
    TType ('(':_) ts -> showParens (showCommaSep (map shows ts))
    TType c [] -> showString c
    TType c ts -> showParen (p>0)
      (showSpaceSep (showString c : map (showsPrec 1) ts))

instance Show TypePattern where
  showsPrec _ (TypePattern x) = case x of
    TType s [] -> showString s
    TType s [t] -> shows t . showSpace . showString s
    TType s ts -> showParens (showCommaSep (map shows ts)) . showSpace . showString s
    _ -> shows x

instance Show DomainArg where
  showsPrec _ (DomainArg str name t) = case (str,name) of
    (Strict, "") -> show_t
    (Strict, s)  -> showParens (showString s . showString "::" . show_t)
    (Lazy,   "") -> showParens (showString "lazy " . show_t)
    (Lazy,   s)  -> showParens
      (showString "lazy " . showString s . showString "::" . show_t)
    where show_t = showQuotes (showHBrackets (shows t))

instance Show DomainCons where
  showsPrec _ (DomainCons s []) = showString s
  showsPrec _ (DomainCons s args) =
    showString s . showSpace . showSpaceSep (map shows args)

instance Show DomainType where
  showsPrec _ (DomainType t cons) =
    shows t . showString " = " . showBarSep (map shows cons)

