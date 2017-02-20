module OrdSet2(module OrdSet,module OrdSet2) where
import Prelude hiding (elem,length,null)
import OrdSet

delete n = fromList . filter (/=n) . toList
difference s1 s2 = foldr delete s1 (toList s2)
