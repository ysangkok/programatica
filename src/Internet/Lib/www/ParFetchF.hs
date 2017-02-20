module ParFetchF where
--import Fudgets

parFetchF n serverF = loopThroughRightF (absF ctrlSP0) serversF
  where
    ns = [1..n]::[Int]
    serversF = listF [(i,serverF)|i<-ns]

    ctrlSP0 = ctrlSP ns

    ctrlSP [] =
      getLeftSP $ \ msg ->
      case msg of
        (s,Left msg) -> putSP (Right (Left (s,msg))) $ ctrlSP []
	(s,Right ans) -> putSP (Right (Right ans)) $ ctrlSP [s]
    ctrlSP servers@(s:ss) =
      getSP $ \ msg ->
      case msg of
        Right req -> putSP (Left (s,req)) $ ctrlSP ss
	Left (s ,Left msg) -> putSP (Right (Left (s,msg))) $ ctrlSP servers
	Left (s',Right ans) -> putSP (Right (Right ans)) $ ctrlSP (s':servers)
