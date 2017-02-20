module IOUtil(getEnvi, progName, progArgs) where
-- Some utilities that are a little dirty, but not very.
--import PrimGetProgName(cGetProgName)
--import PrimGetEnv(cGetEnv)
--import PrimGetArgs(cGetArgs)
--import CString
--import PackedString(unpackPS)

import IO(try)
import System(getEnv,getProgName,getArgs)
import UnsafePerformIO(unsafePerformIO)

getEnvi :: String -> Maybe String
getEnvi s = either (const Nothing) Just $ unsafePerformIO $ try (getEnv s)

progName :: String
progName = unsafePerformIO getProgName

progArgs :: [String]
progArgs = unsafePerformIO getArgs

{-old:
getEnvi :: String -> Maybe String
getEnvi = nonempty . unpackPS . cGetEnv . toCString
  where nonempty "" = Nothing
        nonempty s  = Just s

progName :: String
progName = unpackPS cGetProgName

progArgs :: [String]
progArgs = map unpackPS cGetArgs
-}
