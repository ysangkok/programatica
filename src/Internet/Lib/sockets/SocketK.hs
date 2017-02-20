module SocketK where
import AllFudgets
import DialogueIO hiding (IOError)
import PackedString(packString)

-- deselects all descriptors and closes socket
closeSocketK socket cont =
  selectK [] $ -- no good if the same fudget listens to more than one socket!
  sIOsucc (CloseSocket socket) $
  cont

closeLSocketK lsocket cont =
  selectK [] $ -- no good if the same fudget listens to more than one socket!
  sIOsucc (CloseLSocket lsocket) $
  cont

openLSocketK port err = openLSocketErrF port err
openSocketK host port err = openSocketErrF host port err

selectK descs = select descs -- no response!

writeSocketK socket s =
--    hIOSucc (SocketRequest (WriteSocket socket s))
    writeSocketPSK socket (packString s)

writeSocketPSK socket ps cont =
    sIO (WriteSocketPS socket ps) $
      \ (Wrote n) -> cont {-n-}

getLSocketName lsocket = sIOstr (GetLSocketName lsocket) 
getSocketName  socket  = sIOstr (GetSocketName socket)
