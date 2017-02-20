module RemMetaVarsThomas(module RemMetaVarsThomas,HasMetaVars) where
import SwapMetaVars
import MetaVars(preMetaVar)

remMetaVars s = swapMetaVars s (repeat preMetaVar)

remMetaVarsLetDef d = remMetaVars d -- for backwards compatibility


-- bla bla
