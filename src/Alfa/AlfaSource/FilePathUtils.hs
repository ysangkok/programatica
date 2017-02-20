module FilePathUtils where
import Fud(compactPath,joinPaths,pathHead,aFilePath,filePath)

abschildpath parent relchild = filePath (absolute dir relchild)
  where dir = pathHead (aFilePath parent)

absolute dir = compactPath . joinPaths dir . aFilePath
