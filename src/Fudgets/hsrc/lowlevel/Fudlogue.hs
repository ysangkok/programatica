module Fudlogue(fudlogue1) where
import CmdLineEnv(argFlag)
import Fudget
import Splogue
import TagEvents
import FudVersion
import qualified ContinuationIO as C

--fudlogue1 :: (F a b) -> C.Dialogue
fudlogue1 mainF =
    if argFlag "version" False
    then showVersion
    else splogue mainSP
  where
    mainSP = tagEventsSP mainF

    showVersion =
      C.appendChan C.stdout ("Fudget library " ++ version ++ "\n") C.abort $
      C.done
