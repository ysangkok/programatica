module IOUtil(getEnvi, progName, progArgs) where
-- Some utilities that are a little dirty, but not very.

import IO(try)
import System(getEnv,getProgName,getArgs)
import IOExts(unsafePerformIO)

getEnvi :: String -> Maybe String
getEnvi s = either (const Nothing) Just $ unsafePerformIO $ try (getEnv s)

progName :: String
progName = unsafePerformIO getProgName

progArgs :: [String]
progArgs = unsafePerformIO getArgs
