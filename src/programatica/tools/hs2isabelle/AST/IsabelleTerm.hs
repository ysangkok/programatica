module IsabelleTerm where
import IsabelleType
import Mixfix
import Char(isAlpha)

-- LCF terms in HOLCF
data Term
  = HVar String
  | HInt Integer
  | HStr String -- string literal
  | HApp Term Term
  | HInfix Term String Term
  | HLSection Term String
  | HRSection String Term
  | HAbs [Pattern] Term
  | HLet [HBind] Term
  | HIte Term Term Term -- if-then-else
  | HCase Term [HAlt]
  | HDo HStmt
  | HTuple [Term]
  | HList [Term]
  | HParen Term
  | HRecord Term [HField]
  | HConstrain Term Type
  -- | HMixfix String [Int] Int [Term]
  | HWildPat
  | HAsPat String Pattern
  | HLazyPat Pattern

type Pattern = Term

data HAlt = HAlt Pattern Term

data HMatch = HMatch Pattern Term

data HBind
  = HBindPat HMatch
  | HBindFun [HMatch]
  | HBindSig String Type

data HField = HField String Term

data HStmt
  = HGenerator Pattern Term HStmt
  | HQualifier Term HStmt
  | HLetStmt [HBind] HStmt
  | HLast Term

------------------------------------------------------------

head_of_Term :: Term -> String
head_of_Term (HApp t _) = head_of_Term t
head_of_Term (HInfix _ s _) = s
head_of_Term (HVar s) = s

-- hIte = HMixfix "If _ then _ else _ fi" [0,0,0] 60
hApps t ts = foldl HApp t ts
hCon c ts = hApps (HVar c) ts

isHVar (HVar _) = True
isHVar _ = False

hAbs p (HAbs ps t) = HAbs (p:ps) t
hAbs p t = HAbs [p] t

hAbss ps t = foldr hAbs t ps

nomatch = HVar "UU"

head_of_HMatch :: HMatch -> String
head_of_HMatch (HMatch lhs rhs) = head_of_Term lhs

head_of_HBind :: HBind -> String
head_of_HBind (HBindPat m) = head_of_HMatch m
head_of_HBind (HBindFun (m:ms)) = head_of_HMatch m
head_of_HBind (HBindSig i t) = i

------------------------------------------------------------

instance Show Term where
  showsPrec p x = case x of
    HVar s -> showString s
    HInt n -> shows n -- mixfix "Def _" [shows n] 999 p
    HStr s -> showLR "\'\'" "\'\'" (showString s)
    -- HMixfix s ps p0 ts -> mixfix s (zipWith ($) (map showsPrec ps) ts) p0 p
    HApp t t' -> mixfix "_ _" [showsPrec 999 t, showsPrec 1000 t'] 999 p

    HInfix t o t'
      | isAlpha (head o) -> mixfix "_ `_` _" [showsPrec 900 t, showString o, showsPrec 900 t'] 0 p
      | otherwise -> mixfix "_ _ _" [showsPrec 900 t, showString o, showsPrec 900 t'] 0 p
    HLSection t o -> mixfix "(_ _)" [shows t, showString o] 1000 p
    HRSection o t -> mixfix "(_ _)" [showString o, shows t] 1000 p
    HAbs ps t -> mixfix "\\\\ _ -> _"
                    [showSpaceSep (map (showsPrec 1000) ps), showsPrec 10 t] 10 p
    HLet ms t ->
      mixfix "let {_} in _" [showSemiSep (map shows ms), shows t] 10 p
    HIte b x y ->
      mixfix "if _ then _ else _" [shows b, shows x, shows y] 60 p
    HCase t bs ->
      mixfix "case _ of {_}" [shows t, showSemiSep (map shows bs)] 10 p
    HDo stmts -> mixfix "do {\n    _}\n  " [shows stmts] 10 p
    HTuple ts -> showParens (showCommaSep (map shows ts))
    HList [] -> showString "[]"
    HList ts -> showLR "[" "]" (showCommaSep (map shows ts))
    HParen t -> showParens (shows t)
    HRecord e fs ->
      mixfix "_ {_}" [showsPrec 200 e, showCommaSep (map shows fs)] 100 p
    HConstrain x t -> mixfix "_::_" [showsPrec 4 x, shows t] 3 p
    --
    HWildPat -> showString "_"
    HAsPat x t -> mixfix "_ @ _" [showString x, showsPrec 10 t] 10 p
    HLazyPat t -> mixfix "~ _" [showsPrec 1000 t] 1000 p
{-
    HAbs ps t -> mixfix "\\<Lambda> _. _"
                    [showSpaceSep (map (showsPrec 1000) ps), showsPrec 10 t] 10 p
    HApp t t' -> mixfix "_\\<cdot>_" [showsPrec 999 t, showsPrec 1000 t'] 999 p
    HInfix t o t' -> mixfix "_ `_ _" [showsPrec 900 t, showString o, showsPrec 900 t'] 0 p
    HParen t -> showParens (shows t)
    HTuple ts -> showAngles (showCommaSep (map shows ts))
    HList [] -> showString "[:]"
    HList ts -> showLR "[:" ":]" (showCommaSep (map shows ts))
    HIte b x y -> mixfix "If _ then _ else _ fi" [shows b, shows x, shows y] 60 p
    HInt n -> shows n -- mixfix "Def _" [shows n] 999 p
    HStr s -> showLR "\'\'" "\'\'" (showString s)
    -- HMixfix s ps p0 ts -> mixfix s (zipWith ($) (map showsPrec ps) ts) p0 p
    HLet ms t -> mixfix "Letrec _ in _" [showCommaSep (map shows ms), shows t] 10 p
    HCase t alts ->
            mixfix "Case _ of _" [shows t, showBarSep (map shows alts)] 10 p
    HConstrain x t -> mixfix "_::_" [showsPrec 4 x, shows t] 3 p
    --
    HWildPat -> showString "_"
    HAsPat x t -> mixfix "_ \\<as> _" [showString x, showsPrec 10 t] 10 p
    HLazyPat t -> mixfix "\\<lazy> _" [showsPrec 1000 t] 1000 p
-}

instance Show HAlt where
  showsPrec p (HAlt lhs rhs) =
    mixfix "_ -> _" [shows lhs, shows rhs] 10 p

instance Show HMatch where
  showsPrec p (HMatch lhs rhs) =
    mixfix "_ = _" [shows lhs, shows rhs] 0 p

instance Show HBind where
  showsPrec p (HBindPat e) = showsPrec p e
  showsPrec p (HBindFun ms) = showBarSep (map shows ms)
  showsPrec p (HBindSig i t) = shows i . showString " :: " . shows t

instance Show HStmt where
  showsPrec p (HGenerator pat expr stmts) =
    mixfix "_ <- _;\n    _" [shows pat, shows expr, shows stmts] 10 p
  showsPrec p (HQualifier expr stmts) =
    mixfix "_;\n    _" [shows expr, shows stmts] 10 p
  showsPrec p (HLetStmt binds stmts) =
    mixfix "let _;\n    _"
      [showBraces (showSemiSep (map shows binds)), shows stmts] 10 p
  showsPrec p (HLast expr) = shows expr

instance Show HField where
  showsPrec p (HField s e) = mixfix "_ = _" [showString s, shows e] 10 p
