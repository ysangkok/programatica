module FudgetsPrelude(AsyncInput(..),Descriptor,SocketRequest,SocketResponse,XDisplay(..)
                     ,XRequest,XCommand,XResponse,AEvent,Display,WindowId,XWId(..),XDisplay(..)
                     ) where
import Command(XRequest,XCommand)
import Event(XResponse)
import Sockets(AsyncInput(..),Descriptor,AEvent,SocketRequest,SocketResponse)
import Xtypes(WindowId,Window(..),Display,XWId(..),XDisplay(..))
