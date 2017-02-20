{-+
Instance declarations in the source code are assigned names and
added to the instance database, which gets used during context reduction.
-}
module TiInstanceDB(
  IDB,Instance,InstEntry(..),emptyIdb,extendIdb,classInstances,findInst,findInst',
  addInstKind,instType)
 where
import TiTypes(Type,Pred,Subst(..),Types(..),funT,HsIdentI(..),Typing(..),
	       Kinded,kinded,unQual,forall')
import TiSolve()
import Unification(match,unify)
import Data.Maybe(mapMaybe,isJust)
--import HsIdent -- hmm
import PrettyPrint
import SpecialNames
import TiDefinedNames(definedTypeName,optDefinedTypeName)
import FiniteMap
import MUtils(( # ),mapPartition)
--import Debug.Trace(trace) -- debug

{-+
The instance database is simpliy a list of instances. An instance like

    #instance (Show a,Show b) => Show (Either a b)#

might be represented in the database as

    #(Show (Either a b),(inst_Show_Either,[Show a,Show b]))#
-}

type Instance i = (Pred i,InstEntry i)
data InstEntry i = IE i [Kinded i] [Pred i] deriving (Eq,Show,Read)

instClass (hd,_) = definedTypeName hd
instHead (ih,_) = ih
--instName (_,(n,_)) = n

newtype IDB i = Idb (FiniteMap i [Instance i]) --deriving Show

instType (c,IE v gs ctx) = HsVar v:>:forall' gs (unQual (funT (ctx++[c])))

addInstKind ks (c,(i,ctx)) = (c,IE i (kinded ks (tv (c,ctx))) ctx)

emptyIdb = Idb emptyFM
--extendIdb1 inst (Idb idb) = Idb (inst:idb)
extendIdb insts (Idb idb) = Idb (addListToFM_C (++) idb cinsts)
  where
    cinsts = [(instClass i,[i])|i<-insts]

--namesIdb (Idb idb) = [dn|(_,(dn,_))<-idb]

classInstances (Idb idb) = lookupWithDefaultFM idb []

findInst idb = findInst' True idb
findInst' delayIfOverlap (Idb idb) pred =
    --trace (pp debugmsg)
    pick
  where
    {-
    debugmsg =
       "findInst "<+>pred $$
       nest 4 (vcat [
       "Applicable now:  "<+>some (map (fst.fst) nowInsts),
       "Applicable later:"<+>some (map fst laterInsts),
       "Pick:            "<+>some pick,
       "Other: "<+> if null laterInsts
                    then some (map fst otherInsts)
                    else ppi (length otherInsts - length laterInsts)])
      where
        some xs = length xs <+> vcat (take 5 xs)
    -}
    pick = map instantiate (if delayIfOverlap
			    then handleOverlapping
			    else nowInsts)
    (otherInsts,nowInsts) = mapPartition matchInst insts
      where
        matchInst inst =
              maybe (Left inst) (Right . (,) inst)
                $ match [(instHead inst,pred)]

	-- Instances in the same class, or...
	insts = maybe allInsts (lookupWithDefaultFM idb []) $
		  optPredClass pred
	  where
	    -- Used when looking for an instance in an unknown class!
	    allInsts = concat (eltsFM idb)

    laterInsts = mapMaybe laterInst otherInsts
      where laterInst inst = const inst # unify [(pred,instHead inst)]


    instantiate ((ip,IE dn gs ips),s) = 
        ((dn,su ips),((gs,su ip),S s))
      where su x = apply (S s) x

    -- Support for overlapping instances:

    handleOverlapping =
      if null laterInsts
      then findMostSpecific nowInsts
      else []
    
    findMostSpecific is = filter isMostSpecific is
      where
        isMostSpecific i = all (i `mst`) is
        (i1,_) `mst` (i2,_) = i1 `moreSpecificThan` i2

    i1 `moreSpecificThan` i2 = isJust (match [(instHead i2,instHead i1)])

optPredClass p = optDefinedTypeName p

----
instance Show i => Show (IDB i) where
  showsPrec _ (Idb insts) = shows (fmToList insts)

instance (IsSpecialName i,Printable i) => Printable (IDB i) where
  ppi (Idb insts) = vcat [pinst i|is<-eltsFM insts,i<-is]

pinst (t,IE dn gs ctx) = dn<+>"= instance"<+>ctx<+>"=>"<+>t

instance Printable i => Printable (InstEntry i)
