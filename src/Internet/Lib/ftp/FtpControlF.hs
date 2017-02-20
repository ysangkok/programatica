module FtpControlF(ftpControlF,FtpRequest(..),FtpResponse(..)) where
import AllFudgets hiding (transceiverF)
import TransceiverF
import Char(isDigit)

data FtpRequest = FtpConnect Host Port
                | FtpCommand String

type FtpResponse = [String]

ftpControlF :: F FtpRequest FtpResponse
ftpControlF = groupRepliesSP >^^=< transceiverF >=^< pre
  where pre (FtpConnect host port) = Left (host,port)
        pre (FtpCommand s) = Right (s++"\n")

groupRepliesSP =
    groupReplyLinesSP [] `serCompSP`
    linesSP `serCompSP`
    concSP `preMapSP`
    filter (/='\r')
    

groupReplyLinesSP acc =
    getSP $ \ s ->
    if islast s
    then putSP (reverse (s:acc)) $
         groupReplyLinesSP []
    else groupReplyLinesSP (s:acc)
  where islast (d1:d2:d3:' ':_) = all isDigit [d1,d2,d3]
        islast _ = False
