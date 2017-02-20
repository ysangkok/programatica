module Pop(popF,PopClientSP,PopClientF,doPopCmd,putPopCmd,putPop,getPop) where
import AllFudgets

type PopClientF i o = PopClient F i o
type PopClientSP i o = PopClient SP i o

type PopClient sp i o = sp (PopClientInput i) (PopClientOutput o)

type PopClientOutput a = Either PopCmd a
type PopClientInput a = Either PopResp a
type PopCmd = String
type PopResp = String

popF clientF = loopThroughRightF clientF (popSpyF popIOF)

popIOF :: F PopCmd PopResp
popIOF = (inputLinesSP `preMapSP` filter (/='\r')) >^^=<
	 (openSocketF pophost popport $ transceiverF)

doPopCmd cmd = putPopCmd cmd . takePopResp cmd

putPop = putSP . Left
putPopCmd cmd = putPop (cmd++"\n") -- outputs a command to the pop server

getPop = waitForSP ispop
  where ispop (Left s) = Just s
        ispop _       = Nothing

takePopResp cmd cont =
    getPop $ \ r ->
    if isMultiLine (words cmd) (words r)
    then getMultiLineResp [] $ \rs ->
         cont (r:stripDots rs)
    else cont [r]
  where stripDot ('.':cs) = cs
	stripDot cs       = cs
	stripDots = map stripDot

getMultiLineResp ls cont =
  getPop $ \line ->
  if line=="."
  then cont (reverse ls)
  else getMultiLineResp (line:ls) cont

isMultiLine (cmd:args) (ind:_) =
  ind=="+OK" &&
  (cmd=="TOP" || cmd=="RETR" || cmd=="CAPA" || cmd=="LIST" && null args)

pophost = argKey "pophost" "cs.chalmers.se"
popport = argReadKey "popport" 110 :: Int

popSpyF = if argFlag "popspy" False then spyF else id
