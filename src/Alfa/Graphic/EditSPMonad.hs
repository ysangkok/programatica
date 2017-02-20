module EditSPMonad where
import SPLib

infixr $>,$$

($$) = thenSPms
($>) = bindSPms

type EditMonad i o s a = SPms i o s a

unitEd = unitSPms :: (a->EditMonad i o s a)
loadEd = loadSPms :: (EditMonad i o s s)
storeEd = storeSPms :: (s -> EditMonad i o s ())

putEd = putSPms
getEd = getSPms

mapEd f e = e $> (unitEd.f)
