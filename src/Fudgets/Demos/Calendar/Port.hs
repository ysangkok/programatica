module Port where

import Fudgets
import TypedSockets

type SymTPort a = TPort a a

port :: SymTPort ((String,Int),String)
port = tPort (argReadKey "port" 8888)
