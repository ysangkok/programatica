module HsGuardsUtil() where

import SrcLoc
import HsGuardsStruct

instance HasSrcLoc (HsAlt e p ds) where
    srcLoc (HsAlt s _ _ _) = s


