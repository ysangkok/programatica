module XCall(doXCall,doSCall) where
import P_IO_data(Request,Response)
--import DIO
import IO(stderr,hPutStr)

--primDoXCall primitive 1 :: Request -> Response -- function with side effect
--primDoSCall primitive 1 :: Request -> Response -- function with side effect

{-primitive-}
doXCall :: Request -> IO Response
doXCall = undefined
{-primitive-}
doSCall :: Request -> IO Response
doSCall = undefined

ePutStrLn s =
  do hPutStr stderr s
     hPutStr stderr "\n"
