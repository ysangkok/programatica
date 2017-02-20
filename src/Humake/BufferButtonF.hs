module BufferButtonF where
import Fudgets

bufferButtonF lbl = bufferButtonF' standard lbl

bufferButtonF' pm lbl = bufferSP >^^=< idRightF (buttonF' pm lbl) >=^< Right
  where
    bufferSP = getSP $ either (const bufferSP) bufferSP1
    bufferSP1 x = getSP $ either put bufferSP1
      where put _ = putSP x (bufferSP1 x)
