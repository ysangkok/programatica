module NntpIO(nntpIOF,getCmd,getNntp) where
import Fudgets

nntpIOF = loopThroughRightF (absF nntpSP0) commF

commF = (linesSP `serCompSP` filterSP (/='\r') `serCompSP` concSP) >^^=<
        (openSocketF nntpHost nntpPort $ transceiverF)

nntpHost = argKey "nntphost" "news"
nntpPort = read (argKey "nntpport" "119")

nntpSP0 =
  getNntp' $ \start ->
  putResp [start] $
  nntpSP

nntpSP =
   getCmd $ \cmd ->
   putNntp cmd $
   takeNntpResp cmd $ \resp ->
   putResp resp $
   nntpSP

putNntp cmd = putSP (Left (cmd++"\n")) -- outputs a command to the nntp server
putResp resp = putSP (Right resp)

getCmd = waitForSP iscmd
  where iscmd (Right s) = Just s
        iscmd _       = Nothing

getNntp' = waitForSP isnntp
  where isnntp (Left s) = Just s
        isnntp _       = Nothing

getNntp = waitForSP isnntp
  where isnntp (Left s) = Just s
        isnntp _       = Nothing

takeNntpResp cmd cont =
    getNntp' $ \ r ->
    if isMultiLine (words cmd) (words r)
    then getMultiLineResp [] $ \rs ->
         cont (r:stripDots rs)
    else cont [r]
  where stripDot ('.':cs) = cs
	stripDot cs       = cs
	stripDots = map stripDot

getMultiLineResp ls cont =
  getNntp' $ \line ->
  if line=="."
  then cont (reverse ls)
  else getMultiLineResp (line:ls) cont

isMultiLine (cmd:args) (ind:_) =
  ind `elem` ["100",
              "215",
              "220","221","222",
	      "230","231"]

{-
spyF f = teeF id "OUT: " >==< f >==< teeF id "IN: "

teeF show prefix = ioF teeK
  where
    teeK =
      getK $ \msg ->
      case msg of
	Low _ -> teeK
	High msg -> appendChanK stderr (prefix++show msg++"\n") $
		    putsK [High msg] $
		    teeK
-}
