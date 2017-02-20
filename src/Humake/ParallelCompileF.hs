module ParallelCompileF where
import Fudgets
import ParServerF
import CompileF

parallelCompileF hosts = 
    filterRightSP>^^=< idRightF statusDispF >==< 
    loopThroughRightF (absF ctrlSP0) (parServerF hosts compileF)
  where
    n = length hosts
    toDisp = Right. Left
    toServer = Left
    out = Right. Right
    ctrlSP0 = ctrlSP' ([]::[String])

    ctrlSP' queue =
        putSP (toDisp (0,unwords ([show (length queue),"waiting:"]++waiting))) $
        ctrlSP queue
      where waiting = drop n (reverse queue)

    ctrlSP queue =
        getSP $ either (either msgSP resultSP) cmdSP
      where
        same = ctrlSP queue

        cmdSP cmd@(n,_) =
	  if n `elem` queue
	  then same
	  else putSP (toServer cmd) $
	       ctrlSP' (n:queue)

	msgSP msg = putSP (toDisp msg) same

	resultSP res@(n,_) =
	  putSP (out res) $
	  ctrlSP' (filter (/=n) queue)

    statusDispF = textF' pm >=^< pre
      where pm = setSizing Static .
                 setInitText ("":hosts)
            --pre (0,s) = changeItems 0 [s]
            --pre (n,s) = changeItems n [unwords [show n,s]]
            pre (n,s) = changeItems n [s]
