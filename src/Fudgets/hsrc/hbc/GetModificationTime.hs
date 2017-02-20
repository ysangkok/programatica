module GetModificationTime where
import HaskellIO(hIOerr)
import MkClockTime(mkClockTime)
import DialogueIO

getModificationTime path err cont =
    hIOerr (StatusFile path) err $ \ (Str s) ->
    cont (mkClockTime (read (words s!!10)))
