module Main where
import AllFudgets
import FtpControlF

main = fudlogue (simpleShellF "TstFtp" [] Nothing tstF)

tstF = moreF Nothing >==<
       startupF [High (FtpConnect host 21)] ftpControlF >#==<
       (5,LBelow,FtpCommand>^=<(inputDoneSP>^^=<stringF "" Nothing))

host = argKey "host" "ftp"
