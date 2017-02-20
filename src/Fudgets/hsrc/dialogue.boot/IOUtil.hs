module IOUtil(getEnvi, progName, progArgs) where
-- Some utilities that are a little dirty, but not very.

getEnvi :: String -> Maybe String
getEnvi = undef

progName :: String
progName = undef

progArgs :: [String]
progArgs = undef

undef = error "dialogue.boot/IOUtil.hs"
