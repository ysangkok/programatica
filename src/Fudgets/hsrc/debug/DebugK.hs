module DebugK where
import ContinuationIO(stdout)
import Io(appendChanK)
import CmdLineEnv(argFlag)

debugK x = (if dbg
         then \ s -> appendChanK stdout (s++"\n")
	 else const id) x
  where dbg = argFlag "debugk" False
