module FileMenuF(unnamed,fileNamePopupF,libDir,popup) where
import Fudgets
import ContribFudgets(filePickPopupF',startDir,aFilePath,popup)
--import KeyMenuF
import DialogueIO hiding (IOError)

-- The file menu is now defined in AlfaMenuBar.
-- This module defined the file selection popup window.

--fileNamePopupF = inputPopupF "File" filePickF Nothing
fileNamePopupF = filePickPopupF' extraButtons
  where extraButtons = ([(const (aFilePath libDir),"l","Library")]
                     ++ if argFlag "cwd_button" True
                          then [(const (aFilePath "."),"c", "Cwd")]
                          else [])

unnamed = startDir ++ "/unnamed"

libDir = argKey "libdir" "/usr/local/lib/Alfa/Library/"
