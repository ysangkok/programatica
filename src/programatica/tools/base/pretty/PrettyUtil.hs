module PrettyUtil where

--import SrcLoc
import PrettyPrint
import PrettySymbols(imp,lq,rq)


-- Pretty prints where declaration lists, also used in multiple places.
--ppWhere :: [Doc] -> Doc
ppWhere [] = empty
ppWhere ds = sep [kw "where",letNest (layout ds)]

-- Pretty prints contexts, if they are present
ppContext []  = empty
ppContext [c] = c <+> imp
ppContext cs  = ppiFTuple cs <+> imp

ppqIfDebug s = ppIfDebug (lq<>s<>rq)
