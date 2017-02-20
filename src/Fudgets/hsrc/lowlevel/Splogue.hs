module Splogue(splogue) where
import Command
import Event
import Loopthrough
import Path(Path(..))
import SP(SP)
import ShowFailure
import Sockets
import Spinterp
import Spops
import Tables2
import Xtypes
import Utils(part)
import CmdLineEnv(argFlag)
import Exceptions
import DialogueIO hiding (IOError)
import ContinuationIO(appendChan,stderr,exit)

splogue :: SP (Path, Response) (Path, Request) -> Dialogue
splogue mainSP input' =
    optWatchDisplay (runSP (m resps) ais) input'
  where m resps' = getAiSP (loopThroughRightSP (tagRequestsSP resps') mainSP)
        (ais, resps) = part isai input
	  where
            isai (AsyncInput _) = True
            isai _ = False
 -- hack for removing watchDisplay responses. No other signal handlers allowed
 -- for the moment!
        input = filter noSigAct input'
	  where
	    noSigAct (SigActResp _) = False
	    noSigAct _ = True

tagRequestsSP = tagRequests dtable0
tagRequests dtable resps =
  getSP $ \msg ->
  case msg of
    Left (path', cmd) ->
      case cmd of
	Select ds -> let dtable' = updateDe path' ds dtable
		     in putSP (Right (Select (listDe dtable'))) $
			-- The response to Select is not propagated!
			checkErr resps (tagRequests dtable')
	XCommand _ -> putSP (Right cmd) $
		      case resps of
			-- The response is to an XCommand is always Success.
		        _:resps -> tagRequests dtable resps
	_ -> putSP (Right cmd) $
	     case resps of
	       resp : resps' -> putSP (Left (path', resp)) $
				tagRequests dtable resps'
    Right ai@(AsyncInput (d, i)) ->
      putSP (Left (lookupDe dtable d, ai)) $
      tagRequests dtable resps

checkErr (resp : resps) cont =
    case resp of
      Success -> cont resps
      Failure ioerr -> error ("IOerror: " ++ showFailure ioerr)

getAiSP = interpSP putSP (stepSP [GetAsyncInput]) nullSP
--getAiSP = interpSP putSP (step1SP GetAsyncInput) nullSP
--step1SP y xsp = putSP y (GetSP xsp)

optWatchDisplay =
  if argFlag "watchdisplay" False
  then watchDisplay
  else dontWatchDisplay

dontWatchDisplay qs rs = qs

--{-
--watchDisplay closes ONE display only
watchDisplay (q:qs) ~(r:rs) = q:case r of
        XResponse (DisplayOpened disp@(Display dn)) -> 
	   if dn == 0 then qs 
           else (sigCatch excError ("Fail: "++) $
	        sigCatch excInterrupt (const "Interrupt") $
		const (watch1 qs)) rs

           where watch1 (q:qs) = case q of
		    Exit _ -> closeit:q:qs
		    _ -> q:watch1 qs
		 watch1 [] = [closeit]
                 closeit = XCommand (noDisplay,noWindow,CloseDisplay disp)

		 sigCatch :: Exception -> (String -> String) -> 
		             Dialogue -> Dialogue
		 sigCatch e showE c = sigAction e (SACatch 
		                (\e -> appendChan stderr (showE e++"\n") exit $
				       const [closeit])) exit
			        (const c)
        _ -> watchDisplay qs rs

--}
