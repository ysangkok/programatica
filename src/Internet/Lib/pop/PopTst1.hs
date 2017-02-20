module Main where
import AllFudgets
import Pop

main = X (simpleShellF "Pop Test" [] Nothing popTst)

popTst = (outF >==< popF) >#==< (5,LBelow,inF)

inF = (++"\n") >^=< inputDoneF >==< stringF "" Nothing

outF = moreF Nothing >=^^< ( (\(n,s)->show n:s) `postMapSP` zipSP [1..])
