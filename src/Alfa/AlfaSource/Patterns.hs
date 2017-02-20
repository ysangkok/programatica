module Patterns(Pat(..),Match(..),Match'(..),compactBranch) where
import UAbstract
import AbstractOps(splitAnnots,attachAnnots)
import USubstitute(substitute)

type Pat = Exp

type Match = ((Con,[Var]),Match')

data Match'
  = Simple (Pat,Exp)
  | Nested (Exp->Exp) Var [Match]

compactBranch :: Branch -> Match
compactBranch = compactBranch' id []

compactBranch' s vs (Branch (con,(vars,e))) =
  ((con,vars),
  case splitAnnots e of
    (a,ECase (EVar x) bs) | x `elem` vs' ->
        Nested (attachAnnots a) x (map (compactBranch' s' vs'') bs)
      where s' p = substitute p x (s pat)
                     -- substitute layered pattern x@pat' instead of just pat'!
	    vs' = (vars++vs)
	    vs'' = filter (x/=) vs'
    _ -> Simple (s pat,e))
  where pat = eCon con (map EVar vars)
