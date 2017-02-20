module UnicodeF where
import Fudgets

import Operations

import Greek (mkGreek)
import Arabic (mkArabic)
import Hebrew (mkHebrew)
import Russian (mkRussian, mkRusKOI8)

-- AR 12/4/2000, 18/9/2001 (added font parameter)

fudlogueWrite :: FontId -> (String -> String) -> Maybe String -> IO ()
fudlogueWrite fn trans mbstr = 
  fudlogue $
  shellF "GF Unicode Output" (writeF fn trans mbstr >+< quitButtonF)

writeF fn trans Nothing = writeOutputF fn >==< mapF trans >==< writeInputF fn
writeF fn trans (Just str) = startupF [trans str] (displaySizeP (writeOutputF fn))

displaySizeP = placerF (spacerP (sizeS (Point 440 500)) verticalP)

writeOutputF fn = 
  moreF' (setFont fn)
    >==< 
  justWriteOutputF

justWriteOutputF =
  mapF (concat . map (cutLine1 72) . filter (/=[]) . map mkUnicode . lines)

writeInputF fn = stringInputF' (setShowString mkUnicode . setFont fn)

mkUnicode s = case s of
 '/':'/':cs -> mkGreek   (remClosing cs)
 '/':'+':cs -> mkHebrew  (remClosing cs)
 '/':'-':cs -> mkArabic  (remClosing cs)
 '/':'_':cs -> mkRussian (remClosing cs)
 '/':'*':cs -> mkRusKOI8 (remClosing cs)
 _      -> s

remClosing cs 
 | lcs > 1 && last cs == '/' = take (lcs-2) cs
 | otherwise = cs
    where lcs = length cs

