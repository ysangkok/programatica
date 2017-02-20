module CompileF(compileF) where
import AllFudgets
--import BufferButtonF
import Char

compileF host = loopThroughRightF (ioF (compileK host)) (shF host)

shF host =
    fromLeft>^=<(absF inputLinesSP>+<stdoutF)>==<
    startupF startcmds (subProcessF shell >=^< (++"\n"))
  where
    (shell,startcmds) =
      if host=="localhost"
      then ("sh",[])
      else ("ssh "++host++" sh",["cd \""++cwd++"\""])

compileK host = loop
  where
    loop =
      putHRL (host++" idle") $
      getH $ either (const loop) $ \ (n,cmd) ->
      putHRL (host++" compiling "++n) $
      (if quiet then id else echoK cmd) $
      putHL (cmd ++ " ; echo +-+ $?\n") $
      getResultK $ \ res ->
      putHRR (n,res) loop

getResultK = waitForMsg ans
  where
    ans (High (Left ('+':'-':'+':' ':s))) | all isDigit s = Just (s=="0")
    ans _ = Nothing

putHRR = putH. Right. Right
putHRL = putH. Right. Left
putHL = putH. Left
putH = putK. High
getH k = getK $ message (const $ getH k) k

--isSuccess Success = True
--isSuccess _ = False

quiet = argFlag "s" False
cwd = argKey "cwd" "`pwd`"
