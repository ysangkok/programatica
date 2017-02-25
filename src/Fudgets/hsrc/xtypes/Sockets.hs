 module Sockets(DLValue,module Sockets) where
import Event(XEvent)
import Xtypes(WindowId,Display)
import PackedString(ByteString)
import DLValue

-- NOTE: Matching definitions for the C world appear in xlib/sockets.h --------

data SocketRequest = OpenLSocket Port                -- -> SR LSocket
                   | OpenSocket Host Port            -- -> SR Socket
                   | WriteSocket Socket String       -- -> SR Success
                   | CloseSocket Socket              -- -> SR Success
                   | CloseLSocket LSocket            -- -> SR Success
                   | GetStdinSocket                  -- -> SR Socket
                   | CreateTimer Int Int             -- -> SR Timer
                   | DestroyTimer Timer              -- -> Success
		   | GetLSocketName LSocket          -- -> Str
		   | GetSocketName Socket            -- -> Str
		   | StartProcess String Bool Bool Bool -- -> ProcessSockets
		   | DLOpen String                   -- -> SR DLHandle
		   | DLClose DLHandle		     -- -> Success
                   | DLSym DLHandle String           -- -> SR DLValue
		   | OpenFileAsSocket String String  -- -> SR Socket
		                   -- name,  mode  (as in fopen(name,mode))
		   | WriteSocketPS Socket ByteString -- -> SR Wrote
                   | GetStdoutSocket                 -- -> SR Socket
                     deriving (Eq, Ord, Show)

data SocketResponse = LSocket LSocket
                    | Socket Socket
                    | Timer Timer
		    | ProcessSockets (Maybe Socket) (Maybe Socket) (Maybe Socket)
                    | DLHandle DLHandle
                    | DLVal DLValue
		    | Wrote Int
                      deriving Show --(Eq, Ord, Text)

type Port = Int
type Host = String
type Peer = Host

newtype Socket = So Int deriving (Eq,Ord,Show)
newtype LSocket = LSo Int deriving (Eq,Ord,Show)

newtype Timer = Ti Int deriving (Eq,Ord,Show)

type AsyncInput = (Descriptor, AEvent)

data Descriptor
  = LSocketDe LSocket
  | SocketDe Socket
  | OutputSocketDe Socket
  | TimerDe Timer
  | DisplayDe Display 
  deriving (Eq, Ord, Show)

data AEvent
  = SocketAccepted Socket Peer
  | SocketRead String
  | SocketWritable
  | TimerAlarm
  | XEvent (WindowId, XEvent) 
  deriving (Show)

type DLHandle = Int
