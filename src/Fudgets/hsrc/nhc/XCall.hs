module XCall(doXCall,doSCall) where
import P_IO_data(Request,Response)
import DIO
import IO(stderr,hPutStr)

primDoXCall primitive 1 :: Request -> Response -- function with side effect
primDoSCall primitive 1 :: Request -> Response -- function with side effect

ioPrim :: (Request->Response) -> Request -> IO Response
ioPrim prim req = 
  IO ( \ world ->
	   let res = prim req
           in res `seq` Right res)


doXCall :: Request -> IO Response
doXCall req =
  do --ePutStrLn ("XCall: "++show req)
     resp <- ioPrim primDoXCall req
     --ePutStrLn ("XCall result: "++show resp)
     return resp


doSCall :: Request -> IO Response
doSCall req =
  do --ePutStrLn ("SCall: "++show req)
     resp <- ioPrim primDoSCall req
     --ePutStrLn ("SCall result: "++show resp)
     return resp

ePutStrLn s =
  do hPutStr stderr s
     hPutStr stderr "\n"
