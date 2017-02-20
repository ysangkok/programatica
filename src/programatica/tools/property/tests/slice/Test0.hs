module Test0 where
import qualified QC_combinators as QC
import QC_prelude as Prelude
import qualified Prelude
import Prelude
import List ()
import Ix (inRange)
import PreludeProps ()
assert_RevSumInt
    =   QC.forAll "xs"
            ((\ xs ->
                  (sum (reverse xs)) QC.=== (sum xs)) :: [Int] -> QC.F)
 