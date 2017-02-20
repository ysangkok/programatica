module Main where
import AllFudgets
import FtpReceiverF

main = fudlogue (simpleShellF "TstFtp" [] Nothing tstF)
--main = fudlogue tstF

tstF = outF >==< ftpDataF

--outF = stdoutF >=^< (stripEither.aEither (++"\n") id)

outF = displayF aLeft Nothing defaultFont "" >#+<
       (5,LAbove, moreF Nothing>=^<lines)
{-
-}
