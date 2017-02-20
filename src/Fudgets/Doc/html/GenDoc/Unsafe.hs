module Unsafe where
import IO
import UnsafePerformIO

unsafeOpenFile filename =
  unsafePerformIO $
  (readFile filename >>= return.Right) `catch` return.Left. const "readFile failed"
