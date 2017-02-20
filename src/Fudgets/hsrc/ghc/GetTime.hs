module GetTime where
import HaskellIO(hIO)
import DialogueIO

getTime cont = hIO GetTime $ \ (ClockTime t) -> cont t

getLocalTime cont = hIO GetLocalTime $ \ (CalendarTime t) -> cont t
