module AlfAbstract where
import UAbstract

data Goal = Goal MetaVar Exp deriving (Show)
data Solution = Solution MetaVar Exp deriving (Show)
data Constraint = Constraint Exp Exp deriving (Show)
type Constraints = [Constraint]
data Assumption = Assumption Var Exp deriving (Show)
type Assumptions = [Assumption]
