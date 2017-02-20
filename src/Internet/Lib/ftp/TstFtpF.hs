module Main where
import AllFudgets
import FtpF

main = fudlogue (simpleShellF "TstFtpF" [] Nothing tstF)

tstF = (displayF aLeft Nothing defaultFont "" >#+<
         (5,LAbove,moreF Nothing>=^<lines)) >==<
       ftpF >#==<
       (5,LBelow,ftpreq>^=<(inputDoneSP>^^=<stringF "" Nothing))

ftpreq s =
  case words s of
    [host,path] -> ((host,21),path)
