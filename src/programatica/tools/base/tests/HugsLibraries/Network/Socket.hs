{-# LINE 1 "Socket.hsc" #-}
{-# LINE 2 "Socket.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The "Network.Socket" module is for when you want full control over
-- sockets.  Essentially the entire C socket API is exposed through
-- this module; in general the operations follow the behaviour of the C
-- functions of the same name (consult your favourite Unix networking book).
--
-- A higher level interface to networking operations is provided
-- through the module "Network".
--
-----------------------------------------------------------------------------


{-# LINE 23 "Socket.hsc" #-}


{-# LINE 27 "Socket.hsc" #-}


{-# LINE 29 "Socket.hsc" #-}

{-# LINE 30 "Socket.hsc" #-}

{-# LINE 31 "Socket.hsc" #-}


{-# LINE 39 "Socket.hsc" #-}

-- In order to process this file, you need to have CALLCONV defined.

module Network.Socket (

    -- * Types
    Socket(..),		-- instance Eq, Show
    Family(..),		
    SocketType(..),
    SockAddr(..),
    SocketStatus(..),
    HostAddress,
    ShutdownCmd(..),
    ProtocolNumber,
    PortNumber(..),

    -- * Socket Operations
    socket,		-- :: Family -> SocketType -> ProtocolNumber -> IO Socket 

{-# LINE 58 "Socket.hsc" #-}
    socketPair,         -- :: Family -> SocketType -> ProtocolNumber -> IO (Socket, Socket)

{-# LINE 60 "Socket.hsc" #-}
    connect,		-- :: Socket -> SockAddr -> IO ()
    bindSocket,		-- :: Socket -> SockAddr -> IO ()
    listen,		-- :: Socket -> Int -> IO ()
    accept,		-- :: Socket -> IO (Socket, SockAddr)
    getPeerName,	-- :: Socket -> IO SockAddr
    getSocketName,	-- :: Socket -> IO SockAddr


{-# LINE 68 "Socket.hsc" #-}
	-- get the credentials of our domain socket peer.
    getPeerCred,         -- :: Socket -> IO (CUInt{-pid-}, CUInt{-uid-}, CUInt{-gid-})

{-# LINE 71 "Socket.hsc" #-}

    socketPort,		-- :: Socket -> IO PortNumber

    socketToHandle,	-- :: Socket -> IOMode -> IO Handle

    sendTo,		-- :: Socket -> String -> SockAddr -> IO Int
    recvFrom,		-- :: Socket -> Int -> IO (String, Int, SockAddr)
    
    send,		-- :: Socket -> String -> IO Int
    recv,		-- :: Socket -> Int    -> IO String
    recvLen,            -- :: Socket -> Int    -> IO (String, Int)

    inet_addr,		-- :: String -> IO HostAddress
    inet_ntoa,		-- :: HostAddress -> IO String

    shutdown,		-- :: Socket -> ShutdownCmd -> IO ()
    sClose,		-- :: Socket -> IO ()

    -- ** Predicates on sockets
    sIsConnected,	-- :: Socket -> IO Bool
    sIsBound,		-- :: Socket -> IO Bool
    sIsListening,	-- :: Socket -> IO Bool 
    sIsReadable,	-- :: Socket -> IO Bool
    sIsWritable,	-- :: Socket -> IO Bool

    -- * Socket options
    SocketOption(..),
    getSocketOption,     -- :: Socket -> SocketOption -> IO Int
    setSocketOption,     -- :: Socket -> SocketOption -> Int -> IO ()

    -- * File descriptor transmission

{-# LINE 103 "Socket.hsc" #-}
    sendFd,              -- :: Socket -> CInt -> IO ()
    recvFd,              -- :: Socket -> IO CInt

      -- Note: these two will disappear shortly
    sendAncillary,       -- :: Socket -> Int -> Int -> Int -> Ptr a -> Int -> IO ()
    recvAncillary,       -- :: Socket -> Int -> Int -> IO (Int,Int,Int,Ptr a)


{-# LINE 111 "Socket.hsc" #-}

    -- * Special Constants
    aNY_PORT,		-- :: PortNumber
    iNADDR_ANY,		-- :: HostAddress
    sOMAXCONN,		-- :: Int
    sOL_SOCKET,         -- :: Int

{-# LINE 118 "Socket.hsc" #-}
    sCM_RIGHTS,         -- :: Int

{-# LINE 120 "Socket.hsc" #-}
    maxListenQueue,	-- :: Int

    -- * Initialisation
    withSocketsDo,	-- :: IO a -> IO a
    
    -- * Very low level operations
     -- in case you ever want to get at the underlying file descriptor..
    fdSocket,           -- :: Socket -> CInt
    mkSocket,           -- :: CInt   -> Family 
    			-- -> SocketType
			-- -> ProtocolNumber
			-- -> SocketStatus
			-- -> IO Socket

    -- * Internal

    -- | The following are exported ONLY for use in the BSD module and
    -- should not be used anywhere else.

    packFamily, unpackFamily,
    packSocketType,
    throwSocketErrorIfMinus1_

) where


{-# LINE 146 "Socket.hsc" #-}
import Hugs.Prelude
import Hugs.IO ( openFd )

{-# CBITS HsNet.c initWinSock.c ancilData.c winSockErr.c #-}

{-# LINE 151 "Socket.hsc" #-}

import Data.Word ( Word8, Word16, Word32 )
import Foreign.Ptr ( Ptr, castPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import Foreign.C.Error
import Foreign.C.String ( withCString, peekCString, peekCStringLen, castCharToCChar )
import Foreign.C.Types ( CInt, CUInt, CChar, CSize )
import Foreign.Marshal.Alloc ( alloca, allocaBytes )
import Foreign.Marshal.Array ( peekArray, pokeArray0 )
import Foreign.Marshal.Utils ( with )

import System.IO
import Control.Monad ( liftM, when )
import Data.Ratio ( (%) )

import qualified Control.Exception
import Control.Concurrent.MVar


{-# LINE 179 "Socket.hsc" #-}

-----------------------------------------------------------------------------
-- Socket types

-- There are a few possible ways to do this.  The first is convert the
-- structs used in the C library into an equivalent Haskell type. An
-- other possible implementation is to keep all the internals in the C
-- code and use an Int## and a status flag. The second method is used
-- here since a lot of the C structures are not required to be
-- manipulated.

-- Originally the status was non-mutable so we had to return a new
-- socket each time we changed the status.  This version now uses
-- mutable variables to avoid the need to do this.  The result is a
-- cleaner interface and better security since the application
-- programmer now can't circumvent the status information to perform
-- invalid operations on sockets.

data SocketStatus
  -- Returned Status	Function called
  = NotConnected	-- socket
  | Bound		-- bindSocket
  | Listening		-- listen
  | Connected		-- connect/accept
    deriving (Eq, Show)

data Socket
  = MkSocket
	    CInt	         -- File Descriptor
	    Family				  
	    SocketType				  
	    ProtocolNumber	 -- Protocol Number
	    (MVar SocketStatus)  -- Status Flag

mkSocket :: CInt
	 -> Family
	 -> SocketType
	 -> ProtocolNumber
	 -> SocketStatus
	 -> IO Socket
mkSocket fd fam sType pNum stat = do
   mStat <- newMVar stat
   return (MkSocket fd fam sType pNum mStat)

instance Eq Socket where
  (MkSocket _ _ _ _ m1) == (MkSocket _ _ _ _ m2) = m1 == m2

instance Show Socket where
  showsPrec n (MkSocket fd _ _ _ _) = 
	showString "<socket: " . shows fd . showString ">"


fdSocket :: Socket -> CInt
fdSocket (MkSocket fd _ _ _ _) = fd

type ProtocolNumber = CInt

-- NOTE: HostAddresses are represented in network byte order.
--       Functions that expect the address in machine byte order
--       will have to perform the necessary translation.
type HostAddress = Word32

----------------------------------------------------------------------------
-- Port Numbers
--
-- newtyped to prevent accidental use of sane-looking
-- port numbers that haven't actually been converted to
-- network-byte-order first.
--
newtype PortNumber = PortNum Word16 deriving ( Eq, Ord )

instance Show PortNumber where
  showsPrec p pn = showsPrec p (portNumberToInt pn)

intToPortNumber :: Int -> PortNumber
intToPortNumber v = PortNum (htons (fromIntegral v))

portNumberToInt :: PortNumber -> Int
portNumberToInt (PortNum po) = fromIntegral (ntohs po)

foreign import ccall unsafe "HsNet.h ntohs" ntohs :: Word16 -> Word16
foreign import ccall unsafe "HsNet.h htons" htons :: Word16 -> Word16
--foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import ccall unsafe "HsNet.h htonl" htonl :: Word32 -> Word32

instance Enum PortNumber where
    toEnum   = intToPortNumber
    fromEnum = portNumberToInt

instance Num PortNumber where
   fromInteger i = intToPortNumber (fromInteger i)
    -- for completeness.
   (+) x y   = intToPortNumber (portNumberToInt x + portNumberToInt y)
   (-) x y   = intToPortNumber (portNumberToInt x - portNumberToInt y)
   negate x  = intToPortNumber (-portNumberToInt x)
   (*) x y   = intToPortNumber (portNumberToInt x * portNumberToInt y)
   abs n     = intToPortNumber (abs (portNumberToInt n))
   signum n  = intToPortNumber (signum (portNumberToInt n))

instance Real PortNumber where
    toRational x = toInteger x % 1

instance Integral PortNumber where
    quotRem a b = let (c,d) = quotRem (portNumberToInt a) (portNumberToInt b) in
		  (intToPortNumber c, intToPortNumber d)
    toInteger a = toInteger (portNumberToInt a)

instance Storable PortNumber where
   sizeOf    _ = sizeOf    (undefined :: Word16)
   alignment _ = alignment (undefined :: Word16)
   poke p (PortNum po) = poke (castPtr p) po
   peek p = PortNum `liftM` peek (castPtr p)

-----------------------------------------------------------------------------
-- SockAddr

-- The scheme used for addressing sockets is somewhat quirky. The
-- calls in the BSD socket API that need to know the socket address
-- all operate in terms of struct sockaddr, a `virtual' type of
-- socket address.

-- The Internet family of sockets are addressed as struct sockaddr_in,
-- so when calling functions that operate on struct sockaddr, we have
-- to type cast the Internet socket address into a struct sockaddr.
-- Instances of the structure for different families might *not* be
-- the same size. Same casting is required of other families of
-- sockets such as Xerox NS. Similarly for Unix domain sockets.

-- To represent these socket addresses in Haskell-land, we do what BSD
-- didn't do, and use a union/algebraic type for the different
-- families. Currently only Unix domain sockets and the Internet family
-- are supported.

data SockAddr		-- C Names				
  = SockAddrInet
	PortNumber	-- sin_port  (network byte order)
	HostAddress	-- sin_addr  (ditto)

{-# LINE 317 "Socket.hsc" #-}
  | SockAddrUnix
        String          -- sun_path

{-# LINE 320 "Socket.hsc" #-}
  deriving (Eq)


{-# LINE 327 "Socket.hsc" #-}
type CSaFamily = (Word16)
{-# LINE 328 "Socket.hsc" #-}

{-# LINE 329 "Socket.hsc" #-}

-- we can't write an instance of Storable for SockAddr, because the Storable
-- class can't easily handle alternatives.


{-# LINE 334 "Socket.hsc" #-}
pokeSockAddr p (SockAddrUnix path) = do
	((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((1) :: CSaFamily)
{-# LINE 336 "Socket.hsc" #-}
	let pathC = map castCharToCChar path
	pokeArray0 0 (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p) pathC
{-# LINE 338 "Socket.hsc" #-}

{-# LINE 339 "Socket.hsc" #-}
pokeSockAddr p (SockAddrInet (PortNum port) addr) = do
	((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p ((2) :: CSaFamily)
{-# LINE 341 "Socket.hsc" #-}
	((\hsc_ptr -> pokeByteOff hsc_ptr 2)) p port
{-# LINE 342 "Socket.hsc" #-}
	((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p addr	
{-# LINE 343 "Socket.hsc" #-}

peekSockAddr p = do
  family <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 346 "Socket.hsc" #-}
  case family :: CSaFamily of

{-# LINE 348 "Socket.hsc" #-}
	(1) -> do
{-# LINE 349 "Socket.hsc" #-}
		str <- peekCString (((\hsc_ptr -> hsc_ptr `plusPtr` 2)) p)
{-# LINE 350 "Socket.hsc" #-}
		return (SockAddrUnix str)

{-# LINE 352 "Socket.hsc" #-}
	(2) -> do
{-# LINE 353 "Socket.hsc" #-}
		addr <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 354 "Socket.hsc" #-}
		port <- ((\hsc_ptr -> peekByteOff hsc_ptr 2)) p
{-# LINE 355 "Socket.hsc" #-}
		return (SockAddrInet (PortNum port) addr)

-- size of struct sockaddr by family

{-# LINE 359 "Socket.hsc" #-}
sizeOfSockAddr_Family AF_UNIX = 110
{-# LINE 360 "Socket.hsc" #-}

{-# LINE 361 "Socket.hsc" #-}
sizeOfSockAddr_Family AF_INET = 16
{-# LINE 362 "Socket.hsc" #-}

-- size of struct sockaddr by SockAddr

{-# LINE 365 "Socket.hsc" #-}
sizeOfSockAddr (SockAddrUnix _)   = 110
{-# LINE 366 "Socket.hsc" #-}

{-# LINE 367 "Socket.hsc" #-}
sizeOfSockAddr (SockAddrInet _ _) = 16
{-# LINE 368 "Socket.hsc" #-}

withSockAddr :: SockAddr -> (Ptr SockAddr -> Int -> IO a) -> IO a
withSockAddr addr f = do
 let sz = sizeOfSockAddr addr
 allocaBytes sz $ \p -> pokeSockAddr p addr >> f (castPtr p) sz

withNewSockAddr :: Family -> (Ptr SockAddr -> Int -> IO a) -> IO a
withNewSockAddr family f = do
 let sz = sizeOfSockAddr_Family family
 allocaBytes sz $ \ptr -> f ptr sz

-----------------------------------------------------------------------------
-- Connection Functions

-- In the following connection and binding primitives.  The names of
-- the equivalent C functions have been preserved where possible. It
-- should be noted that some of these names used in the C library,
-- \tr{bind} in particular, have a different meaning to many Haskell
-- programmers and have thus been renamed by appending the prefix
-- Socket.

-- Create an unconnected socket of the given family, type and
-- protocol.  The most common invocation of $socket$ is the following:
--    ...
--    my_socket <- socket AF_INET Stream 6
--    ...

socket :: Family 	 -- Family Name (usually AF_INET)
       -> SocketType 	 -- Socket Type (usually Stream)
       -> ProtocolNumber -- Protocol Number (getProtocolByName to find value)
       -> IO Socket	 -- Unconnected Socket

socket family stype protocol = do
    fd <- throwSocketErrorIfMinus1Retry "socket" $
		c_socket (packFamily family) (packSocketType stype) protocol

{-# LINE 406 "Socket.hsc" #-}
    socket_status <- newMVar NotConnected
    return (MkSocket fd family stype protocol socket_status)

-- Create an unnamed pair of connected sockets, given family, type and
-- protocol. Differs from a normal pipe in being a bi-directional channel
-- of communication.


{-# LINE 414 "Socket.hsc" #-}
socketPair :: Family 	          -- Family Name (usually AF_INET)
           -> SocketType 	  -- Socket Type (usually Stream)
           -> ProtocolNumber      -- Protocol Number
           -> IO (Socket, Socket) -- unnamed and connected.
socketPair family stype protocol = do
    allocaBytes (2 * sizeOf (1 :: CInt)) $ \ fdArr -> do
      rc <- throwSocketErrorIfMinus1Retry "socketpair" $
		  c_socketpair (packFamily family)
			       (packSocketType stype)
			       protocol fdArr
      [fd1,fd2] <- peekArray 2 fdArr 
      s1 <- mkSocket fd1
      s2 <- mkSocket fd2
      return (s1,s2)
  where
    mkSocket fd = do

{-# LINE 433 "Socket.hsc" #-}
       stat <- newMVar Connected
       return (MkSocket fd family stype protocol stat)

foreign import ccall unsafe "HsNet.h socketpair"
  c_socketpair :: CInt -> CInt -> CInt -> Ptr CInt -> IO CInt

{-# LINE 439 "Socket.hsc" #-}

-----------------------------------------------------------------------------
-- Binding a socket
--
-- Given a port number this {\em binds} the socket to that port. This
-- means that the programmer is only interested in data being sent to
-- that port number. The $Family$ passed to $bindSocket$ must
-- be the same as that passed to $socket$.	 If the special port
-- number $aNY\_PORT$ is passed then the system assigns the next
-- available use port.
-- 
-- Port numbers for standard unix services can be found by calling
-- $getServiceEntry$.  These are traditionally port numbers below
-- 1000; although there are afew, namely NFS and IRC, which used higher
-- numbered ports.
-- 
-- The port number allocated to a socket bound by using $aNY\_PORT$ can be
-- found by calling $port$

bindSocket :: Socket	-- Unconnected Socket
	   -> SockAddr	-- Address to Bind to
	   -> IO ()

bindSocket (MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \ status -> do
   if status /= NotConnected 
    then
     ioError (userError ("bindSocket: can't peform bind on socket in status " ++
	   show status))
    else do
     withSockAddr addr $ \p_addr sz -> do
       status <- throwSocketErrorIfMinus1Retry "bind" $ c_bind s p_addr (fromIntegral sz)
       return Bound

-----------------------------------------------------------------------------
-- Connecting a socket
--
-- Make a connection to an already opened socket on a given machine
-- and port.  assumes that we have already called createSocket,
-- otherwise it will fail.
--
-- This is the dual to $bindSocket$.  The {\em server} process will
-- usually bind to a port number, the {\em client} will then connect
-- to the same port number.  Port numbers of user applications are
-- normally agreed in advance, otherwise we must rely on some meta
-- protocol for telling the other side what port number we have been
-- allocated.

connect :: Socket	-- Unconnected Socket
	-> SockAddr 	-- Socket address stuff
	-> IO ()

connect sock@(MkSocket s _family _stype _protocol socketStatus) addr = do
 modifyMVar_ socketStatus $ \currentStatus -> do
  if currentStatus /= NotConnected 
   then
    ioError (userError ("connect: can't peform connect on socket in status " ++
	  show currentStatus))
   else do
    withSockAddr addr $ \p_addr sz -> do

     let  connectLoop = do
	     r <- c_connect s p_addr (fromIntegral sz)
	     if r == -1
		 then do 

{-# LINE 505 "Socket.hsc" #-}
	       	       err <- getErrno
		       case () of
			 _ | err == eINTR       -> connectLoop
			 _ | err == eINPROGRESS -> connectBlocked
--			 _ | err == eAGAIN      -> connectBlocked
			 otherwise              -> throwErrno "connect"

{-# LINE 522 "Socket.hsc" #-}
       	         else return r

	  connectBlocked = do 

{-# LINE 528 "Socket.hsc" #-}
	   err <- getSocketOption sock SoError
	   if (err == 0)
	   	then return 0
	   	else do ioError (errnoToIOError "connect" 
	   			(Errno (fromIntegral err))
	   			Nothing Nothing)

     connectLoop
     return Connected

-----------------------------------------------------------------------------
-- Listen
--
-- The programmer must call $listen$ to tell the system software that
-- they are now interested in receiving data on this port.  This must
-- be called on the bound socket before any calls to read or write
-- data are made.

-- The programmer also gives a number which indicates the length of
-- the incoming queue of unread messages for this socket. On most
-- systems the maximum queue length is around 5.  To remove a message
-- from the queue for processing a call to $accept$ should be made.

listen :: Socket  -- Connected & Bound Socket
       -> Int 	  -- Queue Length
       -> IO ()

listen (MkSocket s _family _stype _protocol socketStatus) backlog = do
 modifyMVar_ socketStatus $ \ status -> do
   if status /= Bound 
     then
      ioError (userError ("listen: can't peform listen on socket in status " ++
	    show status))
     else do
      throwSocketErrorIfMinus1Retry "listen" (c_listen s (fromIntegral backlog))
      return Listening

-----------------------------------------------------------------------------
-- Accept
--
-- A call to `accept' only returns when data is available on the given
-- socket, unless the socket has been set to non-blocking.  It will
-- return a new socket which should be used to read the incoming data and
-- should then be closed. Using the socket returned by `accept' allows
-- incoming requests to be queued on the original socket.

accept :: Socket			-- Queue Socket
       -> IO (Socket,			-- Readable Socket
	      SockAddr)			-- Peer details

accept sock@(MkSocket s family stype protocol status) = do
 currentStatus <- readMVar status
 okay <- sIsAcceptable sock
 if not okay
   then
     ioError (userError ("accept: can't perform accept on socket (" ++ (show (family,stype,protocol)) ++") in status " ++
	 show currentStatus))
   else do
     let sz = sizeOfSockAddr_Family family
     allocaBytes sz $ \ sockaddr -> do

{-# LINE 596 "Socket.hsc" #-}
       with (fromIntegral sz) $ \ ptr_len -> do
	 new_sock <- 

  {-# LINE 602 "Socket.hsc" #-}
			  (c_accept s sockaddr ptr_len)

  {-# LINE 606 "Socket.hsc" #-}

  {-# LINE 607 "Socket.hsc" #-}
	 addr <- peekSockAddr sockaddr
	 new_status <- newMVar Connected
	 return ((MkSocket new_sock family stype protocol new_status), addr)


{-# LINE 621 "Socket.hsc" #-}

-----------------------------------------------------------------------------
-- sendTo & recvFrom

sendTo :: Socket	-- (possibly) bound/connected Socket
       -> String	-- Data to send
       -> SockAddr
       -> IO Int	-- Number of Bytes sent

sendTo (MkSocket s _family _stype _protocol status) xs addr = do
 withSockAddr addr $ \p_addr sz -> do
   withCString xs $ \str -> do
     liftM fromIntegral $

{-# LINE 638 "Socket.hsc" #-}
	c_sendto s str (fromIntegral $ length xs) 0{-flags-} 
			p_addr (fromIntegral sz)

recvFrom :: Socket -> Int -> IO (String, Int, SockAddr)
recvFrom sock@(MkSocket s _family _stype _protocol status) nbytes
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recvFrom")
 | otherwise   = 
  allocaBytes nbytes $ \ptr -> do
    withNewSockAddr AF_INET $ \ptr_addr sz -> do
      alloca $ \ptr_len -> do
      	poke ptr_len (fromIntegral sz)
        len <- 

{-# LINE 654 "Socket.hsc" #-}
        	   c_recvfrom s ptr (fromIntegral nbytes) 0{-flags-} 
				ptr_addr ptr_len
        let len' = fromIntegral len
	if len' == 0
	 then ioError (mkEOFError "Network.Socket.recvFrom")
	 else do
   	   flg <- sIsConnected sock
	     -- For at least one implementation (WinSock 2), recvfrom() ignores
	     -- filling in the sockaddr for connected TCP sockets. Cope with 
	     -- this by using getPeerName instead.
	   sockaddr <- 
		if flg then
		   getPeerName sock
		else
		   peekSockAddr ptr_addr 
           str <- peekCStringLen (ptr,len')
           return (str, len', sockaddr)

-----------------------------------------------------------------------------
-- send & recv

send :: Socket	-- Bound/Connected Socket
     -> String	-- Data to send
     -> IO Int	-- Number of Bytes sent
send (MkSocket s _family _stype _protocol status) xs = do
 withCString xs $ \str -> do
   liftM fromIntegral $

{-# LINE 685 "Socket.hsc" #-}
	c_send s str (fromIntegral $ length xs) 0{-flags-} 

recv :: Socket -> Int -> IO String
recv sock l = recvLen sock l >>= \ (s,_) -> return s

recvLen :: Socket -> Int -> IO (String, Int)
recvLen sock@(MkSocket s _family _stype _protocol status) nbytes 
 | nbytes <= 0 = ioError (mkInvalidRecvArgError "Network.Socket.recv")
 | otherwise   = do
     allocaBytes nbytes $ \ptr -> do
        len <- 

{-# LINE 700 "Socket.hsc" #-}
        	   c_recv s ptr (fromIntegral nbytes) 0{-flags-} 
        let len' = fromIntegral len
	if len' == 0
	 then ioError (mkEOFError "Network.Socket.recv")
	 else do
	   s <- peekCStringLen (ptr,len')
	   return (s, len')

-- ---------------------------------------------------------------------------
-- socketPort
--
-- The port number the given socket is currently connected to can be
-- determined by calling $port$, is generally only useful when bind
-- was given $aNY\_PORT$.

socketPort :: Socket		-- Connected & Bound Socket
	   -> IO PortNumber	-- Port Number of Socket
socketPort sock@(MkSocket _ AF_INET _ _ _) = do
    (SockAddrInet port _) <- getSocketName sock
    return port
socketPort (MkSocket _ family _ _ _) =
    ioError (userError ("socketPort: not supported for Family " ++ show family))


-- ---------------------------------------------------------------------------
-- getPeerName

-- Calling $getPeerName$ returns the address details of the machine,
-- other than the local one, which is connected to the socket. This is
-- used in programs such as FTP to determine where to send the
-- returning data.  The corresponding call to get the details of the
-- local machine is $getSocketName$.

getPeerName   :: Socket -> IO SockAddr
getPeerName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
     throwSocketErrorIfMinus1Retry "getPeerName" $ c_getpeername s ptr int_star
     sz <- peek int_star
     peekSockAddr ptr
    
getSocketName :: Socket -> IO SockAddr
getSocketName (MkSocket s family _ _ _) = do
 withNewSockAddr family $ \ptr sz -> do
   with (fromIntegral sz) $ \int_star -> do
     throwSocketErrorIfMinus1Retry "getSocketName" $ c_getsockname s ptr int_star
     peekSockAddr ptr

-----------------------------------------------------------------------------
-- Socket Properties

data SocketOption
    = DummySocketOption__

{-# LINE 754 "Socket.hsc" #-}
    | Debug         {- SO_DEBUG     -}

{-# LINE 756 "Socket.hsc" #-}

{-# LINE 757 "Socket.hsc" #-}
    | ReuseAddr     {- SO_REUSEADDR -}

{-# LINE 759 "Socket.hsc" #-}

{-# LINE 760 "Socket.hsc" #-}
    | Type          {- SO_TYPE      -}

{-# LINE 762 "Socket.hsc" #-}

{-# LINE 763 "Socket.hsc" #-}
    | SoError       {- SO_ERROR     -}

{-# LINE 765 "Socket.hsc" #-}

{-# LINE 766 "Socket.hsc" #-}
    | DontRoute     {- SO_DONTROUTE -}

{-# LINE 768 "Socket.hsc" #-}

{-# LINE 769 "Socket.hsc" #-}
    | Broadcast     {- SO_BROADCAST -}

{-# LINE 771 "Socket.hsc" #-}

{-# LINE 772 "Socket.hsc" #-}
    | SendBuffer    {- SO_SNDBUF    -}

{-# LINE 774 "Socket.hsc" #-}

{-# LINE 775 "Socket.hsc" #-}
    | RecvBuffer    {- SO_RCVBUF    -}

{-# LINE 777 "Socket.hsc" #-}

{-# LINE 778 "Socket.hsc" #-}
    | KeepAlive     {- SO_KEEPALIVE -}

{-# LINE 780 "Socket.hsc" #-}

{-# LINE 781 "Socket.hsc" #-}
    | OOBInline     {- SO_OOBINLINE -}

{-# LINE 783 "Socket.hsc" #-}

{-# LINE 784 "Socket.hsc" #-}
    | TimeToLive    {- IP_TTL       -}

{-# LINE 786 "Socket.hsc" #-}

{-# LINE 787 "Socket.hsc" #-}
    | MaxSegment    {- TCP_MAXSEG   -}

{-# LINE 789 "Socket.hsc" #-}

{-# LINE 790 "Socket.hsc" #-}
    | NoDelay       {- TCP_NODELAY  -}

{-# LINE 792 "Socket.hsc" #-}

{-# LINE 793 "Socket.hsc" #-}
    | Linger        {- SO_LINGER    -}

{-# LINE 795 "Socket.hsc" #-}

{-# LINE 798 "Socket.hsc" #-}

{-# LINE 799 "Socket.hsc" #-}
    | RecvLowWater  {- SO_RCVLOWAT  -}

{-# LINE 801 "Socket.hsc" #-}

{-# LINE 802 "Socket.hsc" #-}
    | SendLowWater  {- SO_SNDLOWAT  -}

{-# LINE 804 "Socket.hsc" #-}

{-# LINE 805 "Socket.hsc" #-}
    | RecvTimeOut   {- SO_RCVTIMEO  -}

{-# LINE 807 "Socket.hsc" #-}

{-# LINE 808 "Socket.hsc" #-}
    | SendTimeOut   {- SO_SNDTIMEO  -}

{-# LINE 810 "Socket.hsc" #-}

{-# LINE 813 "Socket.hsc" #-}

socketOptLevel :: SocketOption -> CInt
socketOptLevel so = 
  case so of

{-# LINE 818 "Socket.hsc" #-}
    TimeToLive   -> 0
{-# LINE 819 "Socket.hsc" #-}

{-# LINE 820 "Socket.hsc" #-}

{-# LINE 821 "Socket.hsc" #-}
    MaxSegment   -> 6
{-# LINE 822 "Socket.hsc" #-}

{-# LINE 823 "Socket.hsc" #-}

{-# LINE 824 "Socket.hsc" #-}
    NoDelay      -> 6
{-# LINE 825 "Socket.hsc" #-}

{-# LINE 826 "Socket.hsc" #-}
    _            -> 1
{-# LINE 827 "Socket.hsc" #-}

packSocketOption :: SocketOption -> CInt
packSocketOption so =
  case so of

{-# LINE 832 "Socket.hsc" #-}
    Debug         -> 1
{-# LINE 833 "Socket.hsc" #-}

{-# LINE 834 "Socket.hsc" #-}

{-# LINE 835 "Socket.hsc" #-}
    ReuseAddr     -> 2
{-# LINE 836 "Socket.hsc" #-}

{-# LINE 837 "Socket.hsc" #-}

{-# LINE 838 "Socket.hsc" #-}
    Type          -> 3
{-# LINE 839 "Socket.hsc" #-}

{-# LINE 840 "Socket.hsc" #-}

{-# LINE 841 "Socket.hsc" #-}
    SoError       -> 4
{-# LINE 842 "Socket.hsc" #-}

{-# LINE 843 "Socket.hsc" #-}

{-# LINE 844 "Socket.hsc" #-}
    DontRoute     -> 5
{-# LINE 845 "Socket.hsc" #-}

{-# LINE 846 "Socket.hsc" #-}

{-# LINE 847 "Socket.hsc" #-}
    Broadcast     -> 6
{-# LINE 848 "Socket.hsc" #-}

{-# LINE 849 "Socket.hsc" #-}

{-# LINE 850 "Socket.hsc" #-}
    SendBuffer    -> 7
{-# LINE 851 "Socket.hsc" #-}

{-# LINE 852 "Socket.hsc" #-}

{-# LINE 853 "Socket.hsc" #-}
    RecvBuffer    -> 8
{-# LINE 854 "Socket.hsc" #-}

{-# LINE 855 "Socket.hsc" #-}

{-# LINE 856 "Socket.hsc" #-}
    KeepAlive     -> 9
{-# LINE 857 "Socket.hsc" #-}

{-# LINE 858 "Socket.hsc" #-}

{-# LINE 859 "Socket.hsc" #-}
    OOBInline     -> 10
{-# LINE 860 "Socket.hsc" #-}

{-# LINE 861 "Socket.hsc" #-}

{-# LINE 862 "Socket.hsc" #-}
    TimeToLive    -> 2
{-# LINE 863 "Socket.hsc" #-}

{-# LINE 864 "Socket.hsc" #-}

{-# LINE 865 "Socket.hsc" #-}
    MaxSegment    -> 2
{-# LINE 866 "Socket.hsc" #-}

{-# LINE 867 "Socket.hsc" #-}

{-# LINE 868 "Socket.hsc" #-}
    NoDelay       -> 1
{-# LINE 869 "Socket.hsc" #-}

{-# LINE 870 "Socket.hsc" #-}

{-# LINE 871 "Socket.hsc" #-}
    Linger	  -> 13
{-# LINE 872 "Socket.hsc" #-}

{-# LINE 873 "Socket.hsc" #-}

{-# LINE 876 "Socket.hsc" #-}

{-# LINE 877 "Socket.hsc" #-}
    RecvLowWater  -> 18
{-# LINE 878 "Socket.hsc" #-}

{-# LINE 879 "Socket.hsc" #-}

{-# LINE 880 "Socket.hsc" #-}
    SendLowWater  -> 19
{-# LINE 881 "Socket.hsc" #-}

{-# LINE 882 "Socket.hsc" #-}

{-# LINE 883 "Socket.hsc" #-}
    RecvTimeOut   -> 20
{-# LINE 884 "Socket.hsc" #-}

{-# LINE 885 "Socket.hsc" #-}

{-# LINE 886 "Socket.hsc" #-}
    SendTimeOut   -> 21
{-# LINE 887 "Socket.hsc" #-}

{-# LINE 888 "Socket.hsc" #-}

{-# LINE 891 "Socket.hsc" #-}

setSocketOption :: Socket 
		-> SocketOption -- Option Name
		-> Int		-- Option Value
		-> IO ()
setSocketOption (MkSocket s _ _ _ _) so v = do
   with (fromIntegral v) $ \ptr_v -> do
     throwErrnoIfMinus1_ "setSocketOption" $
	 c_setsockopt s (socketOptLevel so) (packSocketOption so) ptr_v 
	    (fromIntegral (sizeOf v))
     return ()


getSocketOption :: Socket
		-> SocketOption  -- Option Name
		-> IO Int	 -- Option Value
getSocketOption (MkSocket s _ _ _ _) so = do
   alloca $ \ptr_v ->
     with (fromIntegral (sizeOf (undefined :: CInt))) $ \ptr_sz -> do
       throwErrnoIfMinus1 "getSocketOption" $
	 c_getsockopt s (socketOptLevel so) (packSocketOption so) ptr_v ptr_sz
       fromIntegral `liftM` peek ptr_v



{-# LINE 916 "Socket.hsc" #-}
-- | Returns the processID, userID and groupID of the socket's peer.
--
-- Only available on platforms that support SO_PEERCRED on domain sockets.
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred sock = do
  let fd = fdSocket sock
  let sz = (fromIntegral (12))
{-# LINE 923 "Socket.hsc" #-}
  with sz $ \ ptr_cr -> 
   alloca       $ \ ptr_sz -> do
     poke ptr_sz sz
     throwErrnoIfMinus1 "getPeerCred" $
       c_getsockopt fd (1) (17) ptr_cr ptr_sz
{-# LINE 928 "Socket.hsc" #-}
     pid <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) ptr_cr
{-# LINE 929 "Socket.hsc" #-}
     uid <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) ptr_cr
{-# LINE 930 "Socket.hsc" #-}
     gid <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) ptr_cr
{-# LINE 931 "Socket.hsc" #-}
     return (pid, uid, gid)

{-# LINE 933 "Socket.hsc" #-}


{-# LINE 935 "Socket.hsc" #-}
-- sending/receiving ancillary socket data; low-level mechanism
-- for transmitting file descriptors, mainly.
sendFd :: Socket -> CInt -> IO ()
sendFd sock outfd = do
  let fd = fdSocket sock

{-# LINE 945 "Socket.hsc" #-}
  c_sendFd fd outfd

{-# LINE 947 "Socket.hsc" #-}
   -- Note: If Winsock supported FD-passing, thi would have been 
   -- incorrect (since socket FDs need to be closed via closesocket().)
  c_close outfd
  return ()
  
recvFd :: Socket -> IO CInt
recvFd sock = do
  let fd = fdSocket sock
  theFd <- 

{-# LINE 960 "Socket.hsc" #-}
         c_recvFd fd
  return theFd


sendAncillary :: Socket
	      -> Int
	      -> Int
	      -> Int
	      -> Ptr a
	      -> Int
	      -> IO ()
sendAncillary sock level ty flags datum len = do
  let fd = fdSocket sock
  _ <-

{-# LINE 978 "Socket.hsc" #-}
     c_sendAncillary fd (fromIntegral level) (fromIntegral ty)
     			(fromIntegral flags) datum (fromIntegral len)
  return ()

recvAncillary :: Socket
	      -> Int
	      -> Int
	      -> IO (Int,Int,Ptr a,Int)
recvAncillary sock flags len = do
  let fd = fdSocket sock
  alloca      $ \ ptr_len   ->
   alloca      $ \ ptr_lev   ->
    alloca      $ \ ptr_ty    ->
     alloca      $ \ ptr_pData -> do
      poke ptr_len (fromIntegral len)
      _ <- 

{-# LINE 998 "Socket.hsc" #-}
	    c_recvAncillary fd ptr_lev ptr_ty (fromIntegral flags) ptr_pData ptr_len
      len <- fromIntegral `liftM` peek ptr_len
      lev <- fromIntegral `liftM` peek ptr_lev
      ty  <- fromIntegral `liftM` peek ptr_ty
      pD  <- peek ptr_pData
      return (lev,ty,pD, len)
foreign import ccall unsafe "HsNet.h sendAncillary"
  c_sendAncillary :: CInt -> CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

foreign import ccall unsafe "HsNet.h recvAncillary"
  c_recvAncillary :: CInt -> Ptr CInt -> Ptr CInt -> CInt -> Ptr (Ptr a) -> Ptr CInt -> IO CInt

foreign import ccall unsafe "HsNet.h sendFd" c_sendFd :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "HsNet.h recvFd" c_recvFd :: CInt -> IO CInt


{-# LINE 1014 "Socket.hsc" #-}


{-
A calling sequence table for the main functions is shown in the table below.

\begin{figure}[h]
\begin{center}
\begin{tabular}{|l|c|c|c|c|c|c|c|}d
\hline
{\bf A Call to} & socket & connect & bindSocket & listen & accept & read & write \\
\hline
{\bf Precedes} & & & & & & & \\
\hline 
socket &	&	  &	       &	&	 &	& \\
\hline
connect & +	&	  &	       &	&	 &	& \\
\hline
bindSocket & +	&	  &	       &	&	 &	& \\
\hline
listen &	&	  & +	       &	&	 &	& \\
\hline
accept &	&	  &	       &  +	&	 &	& \\
\hline
read   &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
write  &	&   +	  &	       &  +	&  +	 &  +	& + \\
\hline
\end{tabular}
\caption{Sequence Table for Major functions of Socket}
\label{tab:api-seq}
\end{center}
\end{figure}
-}

-- ---------------------------------------------------------------------------
-- OS Dependent Definitions
    
unpackFamily	:: CInt -> Family
packFamily	:: Family -> CInt

packSocketType	:: SocketType -> CInt

-- | Address Families.
--
-- This data type might have different constructors depending on what is
-- supported by the operating system.
data Family
	= AF_UNSPEC	-- unspecified

{-# LINE 1063 "Socket.hsc" #-}
	| AF_UNIX	-- local to host (pipes, portals

{-# LINE 1065 "Socket.hsc" #-}

{-# LINE 1066 "Socket.hsc" #-}
	| AF_INET	-- internetwork: UDP, TCP, etc

{-# LINE 1068 "Socket.hsc" #-}

{-# LINE 1069 "Socket.hsc" #-}
        | AF_INET6	-- Internet Protocol version 6

{-# LINE 1071 "Socket.hsc" #-}

{-# LINE 1074 "Socket.hsc" #-}

{-# LINE 1077 "Socket.hsc" #-}

{-# LINE 1080 "Socket.hsc" #-}

{-# LINE 1083 "Socket.hsc" #-}

{-# LINE 1086 "Socket.hsc" #-}

{-# LINE 1089 "Socket.hsc" #-}

{-# LINE 1092 "Socket.hsc" #-}

{-# LINE 1095 "Socket.hsc" #-}

{-# LINE 1096 "Socket.hsc" #-}
	| AF_SNA	-- IBM SNA

{-# LINE 1098 "Socket.hsc" #-}

{-# LINE 1099 "Socket.hsc" #-}
	| AF_DECnet	-- DECnet

{-# LINE 1101 "Socket.hsc" #-}

{-# LINE 1104 "Socket.hsc" #-}

{-# LINE 1107 "Socket.hsc" #-}

{-# LINE 1110 "Socket.hsc" #-}

{-# LINE 1111 "Socket.hsc" #-}
	| AF_APPLETALK	-- Apple Talk

{-# LINE 1113 "Socket.hsc" #-}

{-# LINE 1114 "Socket.hsc" #-}
	| AF_ROUTE	-- Internal Routing Protocol 

{-# LINE 1116 "Socket.hsc" #-}

{-# LINE 1119 "Socket.hsc" #-}

{-# LINE 1122 "Socket.hsc" #-}

{-# LINE 1125 "Socket.hsc" #-}

{-# LINE 1128 "Socket.hsc" #-}

{-# LINE 1131 "Socket.hsc" #-}

{-# LINE 1134 "Socket.hsc" #-}

{-# LINE 1135 "Socket.hsc" #-}
	| AF_X25	-- CCITT X.25

{-# LINE 1137 "Socket.hsc" #-}

{-# LINE 1138 "Socket.hsc" #-}
	| AF_AX25

{-# LINE 1140 "Socket.hsc" #-}

{-# LINE 1143 "Socket.hsc" #-}

{-# LINE 1146 "Socket.hsc" #-}

{-# LINE 1147 "Socket.hsc" #-}
	| AF_IPX	-- Novell Internet Protocol

{-# LINE 1149 "Socket.hsc" #-}

{-# LINE 1152 "Socket.hsc" #-}

{-# LINE 1155 "Socket.hsc" #-}

{-# LINE 1158 "Socket.hsc" #-}

{-# LINE 1161 "Socket.hsc" #-}

{-# LINE 1164 "Socket.hsc" #-}

{-# LINE 1167 "Socket.hsc" #-}

{-# LINE 1170 "Socket.hsc" #-}

{-# LINE 1173 "Socket.hsc" #-}

{-# LINE 1176 "Socket.hsc" #-}

{-# LINE 1179 "Socket.hsc" #-}

{-# LINE 1182 "Socket.hsc" #-}

{-# LINE 1185 "Socket.hsc" #-}

{-# LINE 1188 "Socket.hsc" #-}

{-# LINE 1191 "Socket.hsc" #-}

{-# LINE 1194 "Socket.hsc" #-}

{-# LINE 1197 "Socket.hsc" #-}

{-# LINE 1200 "Socket.hsc" #-}

{-# LINE 1203 "Socket.hsc" #-}

{-# LINE 1206 "Socket.hsc" #-}

{-# LINE 1209 "Socket.hsc" #-}

{-# LINE 1212 "Socket.hsc" #-}
	deriving (Eq, Ord, Read, Show)

------ ------
			
packFamily f = case f of
	AF_UNSPEC -> 0
{-# LINE 1218 "Socket.hsc" #-}

{-# LINE 1219 "Socket.hsc" #-}
	AF_UNIX -> 1
{-# LINE 1220 "Socket.hsc" #-}

{-# LINE 1221 "Socket.hsc" #-}

{-# LINE 1222 "Socket.hsc" #-}
	AF_INET -> 2
{-# LINE 1223 "Socket.hsc" #-}

{-# LINE 1224 "Socket.hsc" #-}

{-# LINE 1225 "Socket.hsc" #-}
        AF_INET6 -> 10
{-# LINE 1226 "Socket.hsc" #-}

{-# LINE 1227 "Socket.hsc" #-}

{-# LINE 1230 "Socket.hsc" #-}

{-# LINE 1233 "Socket.hsc" #-}

{-# LINE 1236 "Socket.hsc" #-}

{-# LINE 1239 "Socket.hsc" #-}

{-# LINE 1242 "Socket.hsc" #-}

{-# LINE 1245 "Socket.hsc" #-}

{-# LINE 1248 "Socket.hsc" #-}

{-# LINE 1251 "Socket.hsc" #-}

{-# LINE 1252 "Socket.hsc" #-}
	AF_SNA -> 22
{-# LINE 1253 "Socket.hsc" #-}

{-# LINE 1254 "Socket.hsc" #-}

{-# LINE 1255 "Socket.hsc" #-}
	AF_DECnet -> 12
{-# LINE 1256 "Socket.hsc" #-}

{-# LINE 1257 "Socket.hsc" #-}

{-# LINE 1260 "Socket.hsc" #-}

{-# LINE 1263 "Socket.hsc" #-}

{-# LINE 1266 "Socket.hsc" #-}

{-# LINE 1267 "Socket.hsc" #-}
	AF_APPLETALK -> 5
{-# LINE 1268 "Socket.hsc" #-}

{-# LINE 1269 "Socket.hsc" #-}

{-# LINE 1270 "Socket.hsc" #-}
	AF_ROUTE -> 16
{-# LINE 1271 "Socket.hsc" #-}

{-# LINE 1272 "Socket.hsc" #-}

{-# LINE 1275 "Socket.hsc" #-}

{-# LINE 1278 "Socket.hsc" #-}

{-# LINE 1281 "Socket.hsc" #-}

{-# LINE 1284 "Socket.hsc" #-}

{-# LINE 1287 "Socket.hsc" #-}

{-# LINE 1290 "Socket.hsc" #-}

{-# LINE 1291 "Socket.hsc" #-}
	AF_X25 -> 9
{-# LINE 1292 "Socket.hsc" #-}

{-# LINE 1293 "Socket.hsc" #-}

{-# LINE 1294 "Socket.hsc" #-}
	AF_AX25 -> 3
{-# LINE 1295 "Socket.hsc" #-}

{-# LINE 1296 "Socket.hsc" #-}

{-# LINE 1299 "Socket.hsc" #-}

{-# LINE 1302 "Socket.hsc" #-}

{-# LINE 1303 "Socket.hsc" #-}
	AF_IPX -> 4
{-# LINE 1304 "Socket.hsc" #-}

{-# LINE 1305 "Socket.hsc" #-}

{-# LINE 1308 "Socket.hsc" #-}

{-# LINE 1311 "Socket.hsc" #-}

{-# LINE 1314 "Socket.hsc" #-}

{-# LINE 1317 "Socket.hsc" #-}

{-# LINE 1320 "Socket.hsc" #-}

{-# LINE 1323 "Socket.hsc" #-}

{-# LINE 1326 "Socket.hsc" #-}

{-# LINE 1329 "Socket.hsc" #-}

{-# LINE 1332 "Socket.hsc" #-}

{-# LINE 1335 "Socket.hsc" #-}

{-# LINE 1338 "Socket.hsc" #-}

{-# LINE 1341 "Socket.hsc" #-}

{-# LINE 1344 "Socket.hsc" #-}

{-# LINE 1347 "Socket.hsc" #-}

{-# LINE 1350 "Socket.hsc" #-}

{-# LINE 1353 "Socket.hsc" #-}

{-# LINE 1356 "Socket.hsc" #-}

{-# LINE 1359 "Socket.hsc" #-}

{-# LINE 1362 "Socket.hsc" #-}

{-# LINE 1365 "Socket.hsc" #-}

{-# LINE 1368 "Socket.hsc" #-}

--------- ----------

unpackFamily f = case f of
	(0) -> AF_UNSPEC
{-# LINE 1373 "Socket.hsc" #-}

{-# LINE 1374 "Socket.hsc" #-}
	(1) -> AF_UNIX
{-# LINE 1375 "Socket.hsc" #-}

{-# LINE 1376 "Socket.hsc" #-}

{-# LINE 1377 "Socket.hsc" #-}
	(2) -> AF_INET
{-# LINE 1378 "Socket.hsc" #-}

{-# LINE 1379 "Socket.hsc" #-}

{-# LINE 1380 "Socket.hsc" #-}
        (10) -> AF_INET6
{-# LINE 1381 "Socket.hsc" #-}

{-# LINE 1382 "Socket.hsc" #-}

{-# LINE 1385 "Socket.hsc" #-}

{-# LINE 1388 "Socket.hsc" #-}

{-# LINE 1391 "Socket.hsc" #-}

{-# LINE 1394 "Socket.hsc" #-}

{-# LINE 1397 "Socket.hsc" #-}

{-# LINE 1400 "Socket.hsc" #-}

{-# LINE 1403 "Socket.hsc" #-}

{-# LINE 1406 "Socket.hsc" #-}

{-# LINE 1407 "Socket.hsc" #-}
	(22) -> AF_SNA
{-# LINE 1408 "Socket.hsc" #-}

{-# LINE 1409 "Socket.hsc" #-}

{-# LINE 1410 "Socket.hsc" #-}
	(12) -> AF_DECnet
{-# LINE 1411 "Socket.hsc" #-}

{-# LINE 1412 "Socket.hsc" #-}

{-# LINE 1415 "Socket.hsc" #-}

{-# LINE 1418 "Socket.hsc" #-}

{-# LINE 1421 "Socket.hsc" #-}

{-# LINE 1422 "Socket.hsc" #-}
	(5) -> AF_APPLETALK
{-# LINE 1423 "Socket.hsc" #-}

{-# LINE 1424 "Socket.hsc" #-}

{-# LINE 1425 "Socket.hsc" #-}
	(16) -> AF_ROUTE
{-# LINE 1426 "Socket.hsc" #-}

{-# LINE 1427 "Socket.hsc" #-}

{-# LINE 1430 "Socket.hsc" #-}

{-# LINE 1433 "Socket.hsc" #-}

{-# LINE 1436 "Socket.hsc" #-}

{-# LINE 1439 "Socket.hsc" #-}

{-# LINE 1444 "Socket.hsc" #-}

{-# LINE 1447 "Socket.hsc" #-}

{-# LINE 1448 "Socket.hsc" #-}
	(9) -> AF_X25
{-# LINE 1449 "Socket.hsc" #-}

{-# LINE 1450 "Socket.hsc" #-}

{-# LINE 1451 "Socket.hsc" #-}
	(3) -> AF_AX25
{-# LINE 1452 "Socket.hsc" #-}

{-# LINE 1453 "Socket.hsc" #-}

{-# LINE 1456 "Socket.hsc" #-}

{-# LINE 1459 "Socket.hsc" #-}

{-# LINE 1460 "Socket.hsc" #-}
	(4) -> AF_IPX
{-# LINE 1461 "Socket.hsc" #-}

{-# LINE 1462 "Socket.hsc" #-}

{-# LINE 1465 "Socket.hsc" #-}

{-# LINE 1468 "Socket.hsc" #-}

{-# LINE 1471 "Socket.hsc" #-}

{-# LINE 1474 "Socket.hsc" #-}

{-# LINE 1477 "Socket.hsc" #-}

{-# LINE 1480 "Socket.hsc" #-}

{-# LINE 1483 "Socket.hsc" #-}

{-# LINE 1486 "Socket.hsc" #-}

{-# LINE 1489 "Socket.hsc" #-}

{-# LINE 1492 "Socket.hsc" #-}

{-# LINE 1495 "Socket.hsc" #-}

{-# LINE 1498 "Socket.hsc" #-}

{-# LINE 1501 "Socket.hsc" #-}

{-# LINE 1504 "Socket.hsc" #-}

{-# LINE 1507 "Socket.hsc" #-}

{-# LINE 1510 "Socket.hsc" #-}

{-# LINE 1513 "Socket.hsc" #-}

{-# LINE 1516 "Socket.hsc" #-}

{-# LINE 1519 "Socket.hsc" #-}

{-# LINE 1522 "Socket.hsc" #-}

{-# LINE 1525 "Socket.hsc" #-}

-- Socket Types.

-- | Socket Types.
--
-- This data type might have different constructors depending on what is
-- supported by the operating system.
data SocketType
	= NoSocketType

{-# LINE 1535 "Socket.hsc" #-}
	| Stream 

{-# LINE 1537 "Socket.hsc" #-}

{-# LINE 1538 "Socket.hsc" #-}
	| Datagram

{-# LINE 1540 "Socket.hsc" #-}

{-# LINE 1541 "Socket.hsc" #-}
	| Raw 

{-# LINE 1543 "Socket.hsc" #-}

{-# LINE 1544 "Socket.hsc" #-}
	| RDM 

{-# LINE 1546 "Socket.hsc" #-}

{-# LINE 1547 "Socket.hsc" #-}
	| SeqPacket

{-# LINE 1549 "Socket.hsc" #-}
	deriving (Eq, Ord, Read, Show)
	
packSocketType stype = case stype of
	NoSocketType -> 0

{-# LINE 1554 "Socket.hsc" #-}
	Stream -> 1
{-# LINE 1555 "Socket.hsc" #-}

{-# LINE 1556 "Socket.hsc" #-}

{-# LINE 1557 "Socket.hsc" #-}
	Datagram -> 2
{-# LINE 1558 "Socket.hsc" #-}

{-# LINE 1559 "Socket.hsc" #-}

{-# LINE 1560 "Socket.hsc" #-}
	Raw -> 3
{-# LINE 1561 "Socket.hsc" #-}

{-# LINE 1562 "Socket.hsc" #-}

{-# LINE 1563 "Socket.hsc" #-}
	RDM -> 4
{-# LINE 1564 "Socket.hsc" #-}

{-# LINE 1565 "Socket.hsc" #-}

{-# LINE 1566 "Socket.hsc" #-}
	SeqPacket -> 5
{-# LINE 1567 "Socket.hsc" #-}

{-# LINE 1568 "Socket.hsc" #-}

-- ---------------------------------------------------------------------------
-- Utility Functions

aNY_PORT :: PortNumber 
aNY_PORT = 0

iNADDR_ANY :: HostAddress
iNADDR_ANY = htonl (0)
{-# LINE 1577 "Socket.hsc" #-}

sOMAXCONN :: Int
sOMAXCONN = 128
{-# LINE 1580 "Socket.hsc" #-}

sOL_SOCKET :: Int
sOL_SOCKET = 1
{-# LINE 1583 "Socket.hsc" #-}


{-# LINE 1585 "Socket.hsc" #-}
sCM_RIGHTS :: Int
sCM_RIGHTS = 1
{-# LINE 1587 "Socket.hsc" #-}

{-# LINE 1588 "Socket.hsc" #-}

maxListenQueue :: Int
maxListenQueue = sOMAXCONN

-- -----------------------------------------------------------------------------

data ShutdownCmd 
 = ShutdownReceive
 | ShutdownSend
 | ShutdownBoth

sdownCmdToInt :: ShutdownCmd -> CInt
sdownCmdToInt ShutdownReceive = 0
sdownCmdToInt ShutdownSend    = 1
sdownCmdToInt ShutdownBoth    = 2

shutdown :: Socket -> ShutdownCmd -> IO ()
shutdown (MkSocket s _ _ _ _) stype = do
  throwSocketErrorIfMinus1Retry "shutdown" (c_shutdown s (sdownCmdToInt stype))
  return ()

-- -----------------------------------------------------------------------------

sClose	 :: Socket -> IO ()
sClose (MkSocket s _ _ _ _) = do c_close s; return ()

-- -----------------------------------------------------------------------------

sIsConnected :: Socket -> IO Bool
sIsConnected (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected)	

-- -----------------------------------------------------------------------------
-- Socket Predicates

sIsBound :: Socket -> IO Bool
sIsBound (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Bound)	

sIsListening :: Socket -> IO Bool
sIsListening (MkSocket _ _ _  _ status) = do
    value <- readMVar status
    return (value == Listening)	

sIsReadable  :: Socket -> IO Bool
sIsReadable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Listening || value == Connected)

sIsWritable  :: Socket -> IO Bool
sIsWritable = sIsReadable -- sort of.

sIsAcceptable :: Socket -> IO Bool

{-# LINE 1644 "Socket.hsc" #-}
sIsAcceptable (MkSocket _ AF_UNIX Stream _ status) = do
    value <- readMVar status
    return (value == Connected || value == Bound || value == Listening)
sIsAcceptable (MkSocket _ AF_UNIX _ _ _) = return False

{-# LINE 1649 "Socket.hsc" #-}
sIsAcceptable (MkSocket _ _ _ _ status) = do
    value <- readMVar status
    return (value == Connected || value == Listening)
    
-- -----------------------------------------------------------------------------
-- Internet address manipulation routines:

inet_addr :: String -> IO HostAddress
inet_addr ipstr = do
   withCString ipstr $ \str -> do
     had <- c_inet_addr str
     if had == -1
      then ioError (userError ("inet_addr: Malformed address: " ++ ipstr))
      else return had  -- network byte order

inet_ntoa :: HostAddress -> IO String
inet_ntoa haddr = do
  pstr <- c_inet_ntoa haddr
  peekCString pstr

-- socketHandle turns a Socket into a Haskell IO Handle. By default, the new
-- handle is unbuffered. Use hSetBuffering to alter this.


{-# LINE 1673 "Socket.hsc" #-}
socketToHandle :: Socket -> IOMode -> IO Handle
socketToHandle s@(MkSocket fd _ _ _ _) mode = do

{-# LINE 1678 "Socket.hsc" #-}

{-# LINE 1679 "Socket.hsc" #-}
    openFd (fromIntegral fd) True{-is a socket-} mode True{-bin-}

{-# LINE 1681 "Socket.hsc" #-}

{-# LINE 1685 "Socket.hsc" #-}

mkInvalidRecvArgError :: String -> IOError
mkInvalidRecvArgError loc = IOError Nothing 

{-# LINE 1691 "Socket.hsc" #-}
				    IllegalOperation

{-# LINE 1693 "Socket.hsc" #-}
				    loc "non-positive length" Nothing

mkEOFError :: String -> IOError
mkEOFError loc = IOError Nothing EOF loc "end of file" Nothing

-- ---------------------------------------------------------------------------
-- WinSock support

{-| On Windows operating systems, the networking subsystem has to be
initialised using 'withSocketsDo' before any networking operations can
be used.  eg.

> main = withSocketsDo $ do {...}

Although this is only strictly necessary on Windows platforms, it is
harmless on other platforms, so for portability it is good practice to
use it all the time.
-}
withSocketsDo :: IO a -> IO a

{-# LINE 1713 "Socket.hsc" #-}
withSocketsDo x = x

{-# LINE 1726 "Socket.hsc" #-}

-- ---------------------------------------------------------------------------
-- foreign imports from the C library

foreign import ccall unsafe "HsNet.h my_inet_ntoa"
  c_inet_ntoa :: HostAddress -> IO (Ptr CChar)

foreign import ccall unsafe "HsNet.h inet_addr"
  c_inet_addr :: Ptr CChar -> IO HostAddress

foreign import ccall unsafe "HsNet.h shutdown"
  c_shutdown :: CInt -> CInt -> IO CInt 


{-# LINE 1740 "Socket.hsc" #-}
foreign import ccall unsafe "HsNet.h close"
  c_close :: CInt -> IO CInt

{-# LINE 1746 "Socket.hsc" #-}

foreign import ccall unsafe "HsNet.h socket"
  c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "HsNet.h bind"
  c_bind :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import ccall unsafe "HsNet.h connect"
  c_connect :: CInt -> Ptr SockAddr -> CInt{-CSockLen???-} -> IO CInt
foreign import ccall unsafe "HsNet.h accept"
  c_accept :: CInt -> Ptr SockAddr -> Ptr CInt{-CSockLen???-} -> IO CInt
foreign import ccall unsafe "HsNet.h listen"
  c_listen :: CInt -> CInt -> IO CInt

foreign import ccall unsafe "HsNet.h send"
  c_send :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "HsNet.h sendto"
  c_sendto :: CInt -> Ptr CChar -> CSize -> CInt -> Ptr SockAddr -> CInt -> IO CInt
foreign import ccall unsafe "HsNet.h recv"
  c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
foreign import ccall unsafe "HsNet.h recvfrom"
  c_recvfrom :: CInt -> Ptr CChar -> CSize -> CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall unsafe "HsNet.h getpeername"
  c_getpeername :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt
foreign import ccall unsafe "HsNet.h getsockname"
  c_getsockname :: CInt -> Ptr SockAddr -> Ptr CInt -> IO CInt

foreign import ccall unsafe "HsNet.h getsockopt"
  c_getsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt
foreign import ccall unsafe "HsNet.h setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

-----------------------------------------------------------------------------
-- Support for thread-safe blocking operations in GHC.


{-# LINE 1808 "Socket.hsc" #-}

throwErrnoIfMinus1Retry_mayBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

throwErrnoIfMinus1Retry_repeatOnBlock name _ act
  = throwSocketErrorIfMinus1Retry name act

throwSocketErrorIfMinus1_ :: Num a => String -> IO a -> IO ()
throwSocketErrorIfMinus1_ name act = do
  throwSocketErrorIfMinus1Retry name act
  return ()


{-# LINE 1852 "Socket.hsc" #-}
throwSocketErrorIfMinus1Retry name act = throwErrnoIfMinus1Retry name act

{-# LINE 1854 "Socket.hsc" #-}

{-# LINE 1855 "Socket.hsc" #-}

