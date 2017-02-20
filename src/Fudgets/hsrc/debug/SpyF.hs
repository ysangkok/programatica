module SpyF where
import Fudget
import CompOps
import IoF(ioF)
import StdIoUtil(appendChanK)
--import EitherUtils
import NullF(getK,putK{-,F,K-})
--import FudgetIO
import ContinuationIO(stderr)

spyF f = teeF show "OUT: " >==< f >==< teeF show "IN: "

teeF show prefix = ioF teeK
  where
    teeK =
      getK $ \msg ->
      case msg of
	Low _ -> teeK
	High msg -> appendChanK stderr (prefix++show msg++"\n") $
		    putK (High msg) $
		    teeK

