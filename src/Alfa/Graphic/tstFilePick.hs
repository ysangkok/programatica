import Fudgets
import FilePickPopupF(filePickPopupF)

main = fudlogue $ shellF "tst filePickPopupF" tstF

tstF = displayF>=^<show
       >==< filePickPopupF
       >=^< mf
       >==< inputDoneSP >^^=< stringF
  where
    mf "" = ((),Nothing)
    mf s = ((),Just s)
