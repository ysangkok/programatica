module GetModificationTime where
import HaskellIO
import DialogueIO

getModificationTime path err cont =
    hIOerr (GetModificationTime path) err $ \ (ClockTime t) ->
    cont t
