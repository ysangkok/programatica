module HandleException where

type Exception = Int

foreign import excError :: Exception
foreign import excInterrupt :: Exception
foreign import excTerminate :: Exception
foreign import excHangup :: Exception
foreign import excPipe :: Exception
foreign import excArithmetic :: Exception
