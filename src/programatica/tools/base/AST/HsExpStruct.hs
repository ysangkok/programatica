module HsExpStruct where

import SrcLoc
import HsIdent
import HsLiteral
import HsGuardsStruct(HsAlt)
import HsFieldsStruct

-------- Expressions --------------------------------------------------------

data EI i e p ds t c
    = HsId (HsIdentI i) -- collapsing HsVar and HsCon
    | HsLit SrcLoc HsLiteral
    | HsInfixApp e (HsIdentI i) e
    | HsApp e e
    | HsNegApp SrcLoc e
    | HsLambda [p] e
    | HsLet ds e
    | HsIf e e e
    | HsCase e [HsAlt e p ds]
    | HsDo (HsStmt e p ds)
    | HsTuple [e]
    | HsList [e]
    | HsParen e
    | HsLeftSection e (HsIdentI i)
    | HsRightSection (HsIdentI i) e
    | HsRecConstr SrcLoc i (HsFieldsI i e) -- qcon { fbind1, ..., fbindn }
    | HsRecUpdate SrcLoc e (HsFieldsI i e) -- exp_<qcon> { fbind1, ..., fbindn }
    | HsEnumFrom e
    | HsEnumFromTo e e
    | HsEnumFromThen e e
    | HsEnumFromThenTo e  e e
    | HsListComp (HsStmt e p ds)
    | HsExpTypeSig SrcLoc e c t
    --------------------------------
    | HsAsPat i e   -- pattern only
    | HsWildCard         -- ditto
    | HsIrrPat e         -- ditto
      deriving (Eq, Show)

data HsStmt e p ds
    = HsGenerator SrcLoc p e (HsStmt e p ds)
    | HsQualifier e (HsStmt e p ds)
    | HsLetStmt ds (HsStmt e p ds)
    | HsLast e
      deriving (Eq, Show) 
