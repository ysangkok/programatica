module ProofmonadOps where
import UAbstract
import HO(apFst)
import Monad(ap)
import qualified ProofEngine as PE
import EditMonad

-- For debugging:
--import Debug2
--import AlfPrint
--import AlfSyntax

default (Int)

#ifdef __HASKELL98__
#define map fmap
#endif

refineSmartG g n = PE.refine g n >>= PE.give g
refineExactG g n = PE.refineExact g n >>= PE.give g

tacCaseG g e = PE.case' g e >>= PE.give g
tacOpenG g e = PE.open g e >>= PE.give g

tacCaseOrOpenG g e =
  tacCaseG g e `handleEd` \ err1 ->
  tacOpenG g e `handleEd` \ err2 ->
  errorEd (unlines [err1,err2])
    -- Do something to create a better error message when both fails!

-- Create abstractions:
abstract1G k = 
  do es <- PE.intro k
     case [ e | e@(EAbs _ _) <- es] of
       e:_ -> do ms <- PE.give k e
                 case lookup k ms of
		   Just (EAbs x (EMeta g)) -> return (x,g)
		   _ -> err
       _ -> err
  where err = errorEd "abstract1G, not a function type"

abstractAllG = abstractAllG' 0
  where 
    abstractAllG' n g =
      if n>20 -- arbitrary limit to avoid looping on infinite function types
      then errorEd "Infinite (arity>20) function type?"
      else caseG (abstract1G g)
                 (\(x,g')->
		  map (apFst (x:)) (abstractAllG' (n+1) g'))
		 (return ([],g))

caseG m ok err = caseEd m ok (const err)
