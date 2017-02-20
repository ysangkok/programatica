module Main where
import AllFudgets
import NntpIO

main = fudlogue (simpleShellF "TstNntpIO" [] Nothing tstF)

tstF = moreF Nothing >==< nntpIOF >#==< (5,LBelow,inputDoneSP>^^=<stringF "" Nothing)
