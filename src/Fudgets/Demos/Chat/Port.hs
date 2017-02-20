module Port where

import Fudgets
import TypedSockets

data ClientMsgs = I_am UserName | Msg Msg deriving (Show,Read)
type UserName = String
type Msg = String

port :: TPort ClientMsgs (UserName,Msg)
port = tPort (argReadKey "port" 8888)
