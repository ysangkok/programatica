module IOUtil(getEnvi, progName, progArgs) where
-- Some utilities that are a little dirty, but not very.

import System.IO.Error(tryIOError)
import System.Environment(getEnv,getProgName,getArgs)
import UnsafePerformIO(unsafePerformIO)

getEnvi :: String -> Maybe String
getEnvi s = either (const Nothing) Just $ unsafePerformIO $ tryIOError (getEnv s)

progName :: String
progName = unsafePerformIO getProgName

progArgs :: [String]
progArgs = unsafePerformIO getArgs
