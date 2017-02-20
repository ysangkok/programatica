module DebugK where
import AllFudgets
import ContinuationIO(stdout)

debugK = if dbg
         then \ s -> appendChanK stdout (s++"\n")
	 else const id
  where dbg = argFlag "gdebug" False
