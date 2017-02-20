module GetTime where
import HaskellIO
import MkClockTime
import FudgetIO
import NullF()
import Time(toUTCTime)
import DialogueIO hiding (IOError)

getTime cont = hIO GetTime $ \ (Dbl t) -> cont (mkClockTime t)

getLocalTime cont =
  hIO GetLocalTime $ \ (Dbl t) ->
  hIO (H_GetTimeZone t)  $ \ (GetTimeZoneResp dst tz dt) ->
  cont ((toUTCTime (mkClockTime t)){ctIsDST=dst,ctTZName=tz,ctTZ=dt})
