module Exceptions(sigAction, module HandleException, FailCont)
   where
import Prelude hiding (IOError)
import DialogueIO
import ContinuationIO(FailCont)
import HandleException(Exception,excError, excInterrupt, excTerminate, excHangup, excPipe, excArithmetic)

sigAction :: Exception -> SigAct -> FailCont -> 
             (SigAct -> Dialogue) -> Dialogue
sigAction sig act fail succ resps =
    SigAction sig act : sigActDispatch fail succ resps where

     sigActDispatch fail succ (resp:resps) = 
        case resp of
	     SigActResp val -> succ val resps
	     Failure msg -> fail msg resps

