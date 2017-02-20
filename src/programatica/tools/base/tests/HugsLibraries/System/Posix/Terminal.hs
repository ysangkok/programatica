{-# LINE 1 "Terminal.hsc" #-}
{-# LINE 2 "Terminal.hsc" #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.Terminal
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX Terminal support
--
-----------------------------------------------------------------------------

module System.Posix.Terminal (
  -- * Terminal support

  -- ** Terminal attributes
  TerminalAttributes,
  getTerminalAttributes,
  TerminalState(..),
  setTerminalAttributes,

  TerminalMode(..),
  withoutMode,
  withMode,
  terminalMode,
  bitsPerByte,
  withBits,

  ControlCharacter(..),
  controlChar,
  withCC,
  withoutCC,

  inputTime,
  withTime,
  minInput,
  withMinInput,

  BaudRate(..),
  inputSpeed,
  withInputSpeed,
  outputSpeed,
  withOutputSpeed,

  -- ** Terminal operations
  sendBreak,
  drainOutput,
  QueueSelector(..),
  discardData,
  FlowAction(..),
  controlFlow,

  -- ** Process groups
  getTerminalProcessGroupID,
  setTerminalProcessGroupID,

  -- ** Testing a file descriptor
  queryTerminal,
  getTerminalName,
  getControllingTerminalName

  ) where


{-# LINE 68 "Terminal.hsc" #-}

import Data.Bits
import Data.Char
import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_, throwErrnoIfNull )
import Foreign.C.String ( CString, peekCString )
import Foreign.C.Types ( CInt )
import Foreign.ForeignPtr ( ForeignPtr, withForeignPtr, mallocForeignPtrBytes )
import Foreign.Marshal.Utils ( copyBytes )
import Foreign.Ptr ( Ptr, nullPtr, plusPtr )
import Foreign.Storable ( Storable(..) )
import System.IO.Unsafe ( unsafePerformIO )
import System.Posix.Types

-- -----------------------------------------------------------------------------
-- Terminal attributes

type CTermios = ()
newtype TerminalAttributes = TerminalAttributes (ForeignPtr CTermios)

makeTerminalAttributes :: ForeignPtr CTermios -> TerminalAttributes
makeTerminalAttributes = TerminalAttributes

withTerminalAttributes :: TerminalAttributes -> (Ptr CTermios -> IO a) -> IO a
withTerminalAttributes (TerminalAttributes termios) = withForeignPtr termios


data TerminalMode
	-- input flags
   = InterruptOnBreak		-- BRKINT
   | MapCRtoLF			-- ICRNL
   | IgnoreBreak		-- IGNBRK
   | IgnoreCR			-- IGNCR
   | IgnoreParityErrors		-- IGNPAR
   | MapLFtoCR			-- INLCR
   | CheckParity		-- INPCK
   | StripHighBit		-- ISTRIP
   | StartStopInput		-- IXOFF
   | StartStopOutput		-- IXON
   | MarkParityErrors		-- PARMRK

	-- output flags
   | ProcessOutput		-- OPOST
	-- ToDo: ONLCR, OCRNL, ONOCR, ONLRET, OFILL,
	--       NLDLY(NL0,NL1), CRDLY(CR0,CR1,CR2,CR2)
	--	 TABDLY(TAB0,TAB1,TAB2,TAB3)
	--	 BSDLY(BS0,BS1), VTDLY(VT0,VT1), FFDLY(FF0,FF1)

	-- control flags
   | LocalMode			-- CLOCAL
   | ReadEnable			-- CREAD
   | TwoStopBits		-- CSTOPB
   | HangupOnClose		-- HUPCL
   | EnableParity		-- PARENB
   | OddParity			-- PARODD

	-- local modes
   | EnableEcho			-- ECHO
   | EchoErase			-- ECHOE
   | EchoKill			-- ECHOK
   | EchoLF			-- ECHONL
   | ProcessInput		-- ICANON
   | ExtendedFunctions		-- IEXTEN
   | KeyboardInterrupts		-- ISIG
   | NoFlushOnInterrupt		-- NOFLSH
   | BackgroundWriteInterrupt	-- TOSTOP

withoutMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withoutMode termios InterruptOnBreak = clearInputFlag (2) termios
{-# LINE 136 "Terminal.hsc" #-}
withoutMode termios MapCRtoLF = clearInputFlag (256) termios
{-# LINE 137 "Terminal.hsc" #-}
withoutMode termios IgnoreBreak = clearInputFlag (1) termios
{-# LINE 138 "Terminal.hsc" #-}
withoutMode termios IgnoreCR = clearInputFlag (128) termios
{-# LINE 139 "Terminal.hsc" #-}
withoutMode termios IgnoreParityErrors = clearInputFlag (4) termios
{-# LINE 140 "Terminal.hsc" #-}
withoutMode termios MapLFtoCR = clearInputFlag (64) termios
{-# LINE 141 "Terminal.hsc" #-}
withoutMode termios CheckParity = clearInputFlag (16) termios
{-# LINE 142 "Terminal.hsc" #-}
withoutMode termios StripHighBit = clearInputFlag (32) termios
{-# LINE 143 "Terminal.hsc" #-}
withoutMode termios StartStopInput = clearInputFlag (4096) termios
{-# LINE 144 "Terminal.hsc" #-}
withoutMode termios StartStopOutput = clearInputFlag (1024) termios
{-# LINE 145 "Terminal.hsc" #-}
withoutMode termios MarkParityErrors = clearInputFlag (8) termios
{-# LINE 146 "Terminal.hsc" #-}
withoutMode termios ProcessOutput = clearOutputFlag (1) termios
{-# LINE 147 "Terminal.hsc" #-}
withoutMode termios LocalMode = clearControlFlag (2048) termios
{-# LINE 148 "Terminal.hsc" #-}
withoutMode termios ReadEnable = clearControlFlag (128) termios
{-# LINE 149 "Terminal.hsc" #-}
withoutMode termios TwoStopBits = clearControlFlag (64) termios
{-# LINE 150 "Terminal.hsc" #-}
withoutMode termios HangupOnClose = clearControlFlag (1024) termios
{-# LINE 151 "Terminal.hsc" #-}
withoutMode termios EnableParity = clearControlFlag (256) termios
{-# LINE 152 "Terminal.hsc" #-}
withoutMode termios OddParity = clearControlFlag (512) termios
{-# LINE 153 "Terminal.hsc" #-}
withoutMode termios EnableEcho = clearLocalFlag (8) termios
{-# LINE 154 "Terminal.hsc" #-}
withoutMode termios EchoErase = clearLocalFlag (16) termios
{-# LINE 155 "Terminal.hsc" #-}
withoutMode termios EchoKill = clearLocalFlag (32) termios
{-# LINE 156 "Terminal.hsc" #-}
withoutMode termios EchoLF = clearLocalFlag (64) termios
{-# LINE 157 "Terminal.hsc" #-}
withoutMode termios ProcessInput = clearLocalFlag (2) termios
{-# LINE 158 "Terminal.hsc" #-}
withoutMode termios ExtendedFunctions = clearLocalFlag (32768) termios
{-# LINE 159 "Terminal.hsc" #-}
withoutMode termios KeyboardInterrupts = clearLocalFlag (1) termios
{-# LINE 160 "Terminal.hsc" #-}
withoutMode termios NoFlushOnInterrupt = setLocalFlag (128) termios
{-# LINE 161 "Terminal.hsc" #-}
withoutMode termios BackgroundWriteInterrupt = clearLocalFlag (256) termios
{-# LINE 162 "Terminal.hsc" #-}

withMode :: TerminalAttributes -> TerminalMode -> TerminalAttributes
withMode termios InterruptOnBreak = setInputFlag (2) termios
{-# LINE 165 "Terminal.hsc" #-}
withMode termios MapCRtoLF = setInputFlag (256) termios
{-# LINE 166 "Terminal.hsc" #-}
withMode termios IgnoreBreak = setInputFlag (1) termios
{-# LINE 167 "Terminal.hsc" #-}
withMode termios IgnoreCR = setInputFlag (128) termios
{-# LINE 168 "Terminal.hsc" #-}
withMode termios IgnoreParityErrors = setInputFlag (4) termios
{-# LINE 169 "Terminal.hsc" #-}
withMode termios MapLFtoCR = setInputFlag (64) termios
{-# LINE 170 "Terminal.hsc" #-}
withMode termios CheckParity = setInputFlag (16) termios
{-# LINE 171 "Terminal.hsc" #-}
withMode termios StripHighBit = setInputFlag (32) termios
{-# LINE 172 "Terminal.hsc" #-}
withMode termios StartStopInput = setInputFlag (4096) termios
{-# LINE 173 "Terminal.hsc" #-}
withMode termios StartStopOutput = setInputFlag (1024) termios
{-# LINE 174 "Terminal.hsc" #-}
withMode termios MarkParityErrors = setInputFlag (8) termios
{-# LINE 175 "Terminal.hsc" #-}
withMode termios ProcessOutput = setOutputFlag (1) termios
{-# LINE 176 "Terminal.hsc" #-}
withMode termios LocalMode = setControlFlag (2048) termios
{-# LINE 177 "Terminal.hsc" #-}
withMode termios ReadEnable = setControlFlag (128) termios
{-# LINE 178 "Terminal.hsc" #-}
withMode termios TwoStopBits = setControlFlag (64) termios
{-# LINE 179 "Terminal.hsc" #-}
withMode termios HangupOnClose = setControlFlag (1024) termios
{-# LINE 180 "Terminal.hsc" #-}
withMode termios EnableParity = setControlFlag (256) termios
{-# LINE 181 "Terminal.hsc" #-}
withMode termios OddParity = setControlFlag (512) termios
{-# LINE 182 "Terminal.hsc" #-}
withMode termios EnableEcho = setLocalFlag (8) termios
{-# LINE 183 "Terminal.hsc" #-}
withMode termios EchoErase = setLocalFlag (16) termios
{-# LINE 184 "Terminal.hsc" #-}
withMode termios EchoKill = setLocalFlag (32) termios
{-# LINE 185 "Terminal.hsc" #-}
withMode termios EchoLF = setLocalFlag (64) termios
{-# LINE 186 "Terminal.hsc" #-}
withMode termios ProcessInput = setLocalFlag (2) termios
{-# LINE 187 "Terminal.hsc" #-}
withMode termios ExtendedFunctions = setLocalFlag (32768) termios
{-# LINE 188 "Terminal.hsc" #-}
withMode termios KeyboardInterrupts = setLocalFlag (1) termios
{-# LINE 189 "Terminal.hsc" #-}
withMode termios NoFlushOnInterrupt = clearLocalFlag (128) termios
{-# LINE 190 "Terminal.hsc" #-}
withMode termios BackgroundWriteInterrupt = setLocalFlag (256) termios
{-# LINE 191 "Terminal.hsc" #-}

terminalMode :: TerminalMode -> TerminalAttributes -> Bool
terminalMode InterruptOnBreak = testInputFlag (2)
{-# LINE 194 "Terminal.hsc" #-}
terminalMode MapCRtoLF = testInputFlag (256)
{-# LINE 195 "Terminal.hsc" #-}
terminalMode IgnoreBreak = testInputFlag (1)
{-# LINE 196 "Terminal.hsc" #-}
terminalMode IgnoreCR = testInputFlag (128)
{-# LINE 197 "Terminal.hsc" #-}
terminalMode IgnoreParityErrors = testInputFlag (4)
{-# LINE 198 "Terminal.hsc" #-}
terminalMode MapLFtoCR = testInputFlag (64)
{-# LINE 199 "Terminal.hsc" #-}
terminalMode CheckParity = testInputFlag (16)
{-# LINE 200 "Terminal.hsc" #-}
terminalMode StripHighBit = testInputFlag (32)
{-# LINE 201 "Terminal.hsc" #-}
terminalMode StartStopInput = testInputFlag (4096)
{-# LINE 202 "Terminal.hsc" #-}
terminalMode StartStopOutput = testInputFlag (1024)
{-# LINE 203 "Terminal.hsc" #-}
terminalMode MarkParityErrors = testInputFlag (8)
{-# LINE 204 "Terminal.hsc" #-}
terminalMode ProcessOutput = testOutputFlag (1)
{-# LINE 205 "Terminal.hsc" #-}
terminalMode LocalMode = testControlFlag (2048)
{-# LINE 206 "Terminal.hsc" #-}
terminalMode ReadEnable = testControlFlag (128)
{-# LINE 207 "Terminal.hsc" #-}
terminalMode TwoStopBits = testControlFlag (64)
{-# LINE 208 "Terminal.hsc" #-}
terminalMode HangupOnClose = testControlFlag (1024)
{-# LINE 209 "Terminal.hsc" #-}
terminalMode EnableParity = testControlFlag (256)
{-# LINE 210 "Terminal.hsc" #-}
terminalMode OddParity = testControlFlag (512)
{-# LINE 211 "Terminal.hsc" #-}
terminalMode EnableEcho = testLocalFlag (8)
{-# LINE 212 "Terminal.hsc" #-}
terminalMode EchoErase = testLocalFlag (16)
{-# LINE 213 "Terminal.hsc" #-}
terminalMode EchoKill = testLocalFlag (32)
{-# LINE 214 "Terminal.hsc" #-}
terminalMode EchoLF = testLocalFlag (64)
{-# LINE 215 "Terminal.hsc" #-}
terminalMode ProcessInput = testLocalFlag (2)
{-# LINE 216 "Terminal.hsc" #-}
terminalMode ExtendedFunctions = testLocalFlag (32768)
{-# LINE 217 "Terminal.hsc" #-}
terminalMode KeyboardInterrupts = testLocalFlag (1)
{-# LINE 218 "Terminal.hsc" #-}
terminalMode NoFlushOnInterrupt = not . testLocalFlag (128)
{-# LINE 219 "Terminal.hsc" #-}
terminalMode BackgroundWriteInterrupt = testLocalFlag (256)
{-# LINE 220 "Terminal.hsc" #-}

bitsPerByte :: TerminalAttributes -> Int
bitsPerByte termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 225 "Terminal.hsc" #-}
    return $! (word2Bits (cflag .&. (48)))
{-# LINE 226 "Terminal.hsc" #-}
  where
    word2Bits :: CTcflag -> Int
    word2Bits x =
	if x == (0) then 5
{-# LINE 230 "Terminal.hsc" #-}
	else if x == (16) then 6
{-# LINE 231 "Terminal.hsc" #-}
	else if x == (32) then 7
{-# LINE 232 "Terminal.hsc" #-}
	else if x == (48) then 8
{-# LINE 233 "Terminal.hsc" #-}
	else 0

withBits :: TerminalAttributes -> Int -> TerminalAttributes
withBits termios bits = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 239 "Terminal.hsc" #-}
    ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p
{-# LINE 240 "Terminal.hsc" #-}
       ((cflag .&. complement (48)) .|. mask bits)
{-# LINE 241 "Terminal.hsc" #-}
  where
    mask :: Int -> CTcflag
    mask 5 = (0)
{-# LINE 244 "Terminal.hsc" #-}
    mask 6 = (16)
{-# LINE 245 "Terminal.hsc" #-}
    mask 7 = (32)
{-# LINE 246 "Terminal.hsc" #-}
    mask 8 = (48)
{-# LINE 247 "Terminal.hsc" #-}
    mask _ = error "withBits bit value out of range [5..8]"

data ControlCharacter
  = EndOfFile		-- VEOF
  | EndOfLine		-- VEOL
  | Erase		-- VERASE
  | Interrupt		-- VINTR
  | Kill		-- VKILL
  | Quit		-- VQUIT
  | Start		-- VSTART
  | Stop		-- VSTOP
  | Suspend		-- VSUSP

controlChar :: TerminalAttributes -> ControlCharacter -> Maybe Char
controlChar termios cc = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 264 "Terminal.hsc" #-}
    val <- peekElemOff c_cc (cc2Word cc)
    if val == ((0)::CCc)
{-# LINE 266 "Terminal.hsc" #-}
       then return Nothing
       else return (Just (chr (fromEnum val)))
  
withCC :: TerminalAttributes
       -> (ControlCharacter, Char)
       -> TerminalAttributes
withCC termios (cc, c) = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 275 "Terminal.hsc" #-}
    pokeElemOff c_cc (cc2Word cc) (fromIntegral (ord c) :: CCc)

withoutCC :: TerminalAttributes
          -> ControlCharacter
          -> TerminalAttributes
withoutCC termios cc = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 283 "Terminal.hsc" #-}
    pokeElemOff c_cc (cc2Word cc) ((0) :: CCc)
{-# LINE 284 "Terminal.hsc" #-}

inputTime :: TerminalAttributes -> Int
inputTime termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    c <- peekElemOff (((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p) (5)
{-# LINE 289 "Terminal.hsc" #-}
    return (fromEnum (c :: CCc))

withTime :: TerminalAttributes -> Int -> TerminalAttributes
withTime termios time = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 295 "Terminal.hsc" #-}
    pokeElemOff c_cc (5) (fromIntegral time :: CCc)
{-# LINE 296 "Terminal.hsc" #-}

minInput :: TerminalAttributes -> Int
minInput termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    c <- peekElemOff (((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p) (6)
{-# LINE 301 "Terminal.hsc" #-}
    return (fromEnum (c :: CCc))

withMinInput :: TerminalAttributes -> Int -> TerminalAttributes
withMinInput termios count = unsafePerformIO $ do
  withNewTermios termios $ \p -> do
    let c_cc = ((\hsc_ptr -> hsc_ptr `plusPtr` 17)) p
{-# LINE 307 "Terminal.hsc" #-}
    pokeElemOff c_cc (6) (fromIntegral count :: CCc)
{-# LINE 308 "Terminal.hsc" #-}

data BaudRate
  = B0
  | B50
  | B75
  | B110
  | B134
  | B150
  | B200
  | B300
  | B600
  | B1200
  | B1800
  | B2400
  | B4800
  | B9600
  | B19200
  | B38400

inputSpeed :: TerminalAttributes -> BaudRate
inputSpeed termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p -> do
    w <- c_cfgetispeed p
    return (word2Baud w)

foreign import ccall unsafe "HsUnix.h cfgetispeed"
  c_cfgetispeed :: Ptr CTermios -> IO CSpeed

withInputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withInputSpeed termios br = unsafePerformIO $ do
  withNewTermios termios $ \p -> c_cfsetispeed p (baud2Word br)

foreign import ccall unsafe "HsUnix.h cfsetispeed"
  c_cfsetispeed :: Ptr CTermios -> CSpeed -> IO CInt


outputSpeed :: TerminalAttributes -> BaudRate
outputSpeed termios = unsafePerformIO $ do
  withTerminalAttributes termios $ \p ->  do
    w <- c_cfgetospeed p
    return (word2Baud w)

foreign import ccall unsafe "HsUnix.h cfgetospeed"
  c_cfgetospeed :: Ptr CTermios -> IO CSpeed

withOutputSpeed :: TerminalAttributes -> BaudRate -> TerminalAttributes
withOutputSpeed termios br = unsafePerformIO $ do
  withNewTermios termios $ \p -> c_cfsetospeed p (baud2Word br)

foreign import ccall unsafe "HsUnix.h cfsetospeed"
  c_cfsetospeed :: Ptr CTermios -> CSpeed -> IO CInt


getTerminalAttributes :: Fd -> IO TerminalAttributes
getTerminalAttributes fd = do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 364 "Terminal.hsc" #-}
  withForeignPtr fp $ \p ->
      throwErrnoIfMinus1_ "getTerminalAttributes" (c_tcgetattr fd p)
  return $ makeTerminalAttributes fp

foreign import ccall unsafe "HsUnix.h tcgetattr"
  c_tcgetattr :: Fd -> Ptr CTermios -> IO CInt

data TerminalState
  = Immediately
  | WhenDrained
  | WhenFlushed

setTerminalAttributes :: Fd
                      -> TerminalAttributes
                      -> TerminalState
                      -> IO ()
setTerminalAttributes fd termios state = do
  withTerminalAttributes termios $ \p ->
    throwErrnoIfMinus1_ "setTerminalAttributes"
      (c_tcsetattr fd (state2Int state) p)
  where
    state2Int :: TerminalState -> CInt
    state2Int Immediately = (0)
{-# LINE 387 "Terminal.hsc" #-}
    state2Int WhenDrained = (1)
{-# LINE 388 "Terminal.hsc" #-}
    state2Int WhenFlushed = (2)
{-# LINE 389 "Terminal.hsc" #-}

foreign import ccall unsafe "HsUnix.h tcsetattr"
   c_tcsetattr :: Fd -> CInt -> Ptr CTermios -> IO CInt


sendBreak :: Fd -> Int -> IO ()
sendBreak fd duration
  = throwErrnoIfMinus1_ "sendBreak" (c_tcsendbreak fd (fromIntegral duration))

foreign import ccall unsafe "HsUnix.h tcsendbreak"
  c_tcsendbreak :: Fd -> CInt -> IO CInt

drainOutput :: Fd -> IO ()
drainOutput fd = throwErrnoIfMinus1_ "drainOutput" (c_tcdrain fd)

foreign import ccall unsafe "HsUnix.h tcdrain"
  c_tcdrain :: Fd -> IO CInt


data QueueSelector
  = InputQueue		-- TCIFLUSH
  | OutputQueue		-- TCOFLUSH
  | BothQueues		-- TCIOFLUSH

discardData :: Fd -> QueueSelector -> IO ()
discardData fd queue =
  throwErrnoIfMinus1_ "discardData" (c_tcflush fd (queue2Int queue))
  where
    queue2Int :: QueueSelector -> CInt
    queue2Int InputQueue  = (0)
{-# LINE 419 "Terminal.hsc" #-}
    queue2Int OutputQueue = (1)
{-# LINE 420 "Terminal.hsc" #-}
    queue2Int BothQueues  = (2)
{-# LINE 421 "Terminal.hsc" #-}

foreign import ccall unsafe "HsUnix.h tcflush"
  c_tcflush :: Fd -> CInt -> IO CInt

data FlowAction
  = SuspendOutput	-- TCOOFF
  | RestartOutput	-- TCOON
  | TransmitStop	-- TCIOFF
  | TransmitStart	-- TCION

controlFlow :: Fd -> FlowAction -> IO ()
controlFlow fd action =
  throwErrnoIfMinus1_ "controlFlow" (c_tcflow fd (action2Int action))
  where
    action2Int :: FlowAction -> CInt
    action2Int SuspendOutput = (0)
{-# LINE 437 "Terminal.hsc" #-}
    action2Int RestartOutput = (1)
{-# LINE 438 "Terminal.hsc" #-}
    action2Int TransmitStop  = (2)
{-# LINE 439 "Terminal.hsc" #-}
    action2Int TransmitStart = (3)
{-# LINE 440 "Terminal.hsc" #-}

foreign import ccall unsafe "HsUnix.h tcflow"
  c_tcflow :: Fd -> CInt -> IO CInt

getTerminalProcessGroupID :: Fd -> IO ProcessGroupID
getTerminalProcessGroupID fd = do
  throwErrnoIfMinus1 "getTerminalProcessGroupID" (c_tcgetpgrp fd)

foreign import ccall unsafe "HsUnix.h tcgetpgrp"
  c_tcgetpgrp :: Fd -> IO CPid

setTerminalProcessGroupID :: Fd -> ProcessGroupID -> IO ()
setTerminalProcessGroupID fd pgid =
  throwErrnoIfMinus1_ "setTerminalProcessGroupID" (c_tcsetpgrp fd pgid)

foreign import ccall unsafe "HsUnix.h tcsetpgrp"
  c_tcsetpgrp :: Fd -> CPid -> IO CInt

-- -----------------------------------------------------------------------------
-- file descriptor queries

queryTerminal :: Fd -> IO Bool
queryTerminal fd = do
  r <- c_isatty fd
  return (r == 1)
  -- ToDo: the spec says that it can set errno to EBADF if the result is zero

foreign import ccall unsafe "HsUnix.h isatty"
  c_isatty :: Fd -> IO CInt


getTerminalName :: Fd -> IO FilePath
getTerminalName fd = do
  s <- throwErrnoIfNull "getTerminalName" (c_ttyname fd)
  peekCString s  

foreign import ccall unsafe "HsUnix.h ttyname"
  c_ttyname :: Fd -> IO CString

getControllingTerminalName :: IO FilePath
getControllingTerminalName = do
  s <- throwErrnoIfNull "getControllingTerminalName" (c_ctermid nullPtr)
  peekCString s

foreign import ccall unsafe "HsUnix.h ctermid"
  c_ctermid :: CString -> IO CString

-- -----------------------------------------------------------------------------
-- Local utility functions

-- Convert Haskell ControlCharacter to Int

cc2Word :: ControlCharacter -> Int
cc2Word EndOfFile = (4)
{-# LINE 494 "Terminal.hsc" #-}
cc2Word EndOfLine = (11)
{-# LINE 495 "Terminal.hsc" #-}
cc2Word Erase     = (2)
{-# LINE 496 "Terminal.hsc" #-}
cc2Word Interrupt = (0)
{-# LINE 497 "Terminal.hsc" #-}
cc2Word Kill      = (3)
{-# LINE 498 "Terminal.hsc" #-}
cc2Word Quit      = (1)
{-# LINE 499 "Terminal.hsc" #-}
cc2Word Suspend   = (10)
{-# LINE 500 "Terminal.hsc" #-}
cc2Word Start     = (8)
{-# LINE 501 "Terminal.hsc" #-}
cc2Word Stop      = (9)
{-# LINE 502 "Terminal.hsc" #-}

-- Convert Haskell BaudRate to unsigned integral type (Word)

baud2Word :: BaudRate -> CSpeed
baud2Word B0 = (0)
{-# LINE 507 "Terminal.hsc" #-}
baud2Word B50 = (1)
{-# LINE 508 "Terminal.hsc" #-}
baud2Word B75 = (2)
{-# LINE 509 "Terminal.hsc" #-}
baud2Word B110 = (3)
{-# LINE 510 "Terminal.hsc" #-}
baud2Word B134 = (4)
{-# LINE 511 "Terminal.hsc" #-}
baud2Word B150 = (5)
{-# LINE 512 "Terminal.hsc" #-}
baud2Word B200 = (6)
{-# LINE 513 "Terminal.hsc" #-}
baud2Word B300 = (7)
{-# LINE 514 "Terminal.hsc" #-}
baud2Word B600 = (8)
{-# LINE 515 "Terminal.hsc" #-}
baud2Word B1200 = (9)
{-# LINE 516 "Terminal.hsc" #-}
baud2Word B1800 = (10)
{-# LINE 517 "Terminal.hsc" #-}
baud2Word B2400 = (11)
{-# LINE 518 "Terminal.hsc" #-}
baud2Word B4800 = (12)
{-# LINE 519 "Terminal.hsc" #-}
baud2Word B9600 = (13)
{-# LINE 520 "Terminal.hsc" #-}
baud2Word B19200 = (14)
{-# LINE 521 "Terminal.hsc" #-}
baud2Word B38400 = (15)
{-# LINE 522 "Terminal.hsc" #-}

-- And convert a word back to a baud rate
-- We really need some cpp macros here.

word2Baud :: CSpeed -> BaudRate
word2Baud x =
    if x == (0) then B0
{-# LINE 529 "Terminal.hsc" #-}
    else if x == (1) then B50
{-# LINE 530 "Terminal.hsc" #-}
    else if x == (2) then B75
{-# LINE 531 "Terminal.hsc" #-}
    else if x == (3) then B110
{-# LINE 532 "Terminal.hsc" #-}
    else if x == (4) then B134
{-# LINE 533 "Terminal.hsc" #-}
    else if x == (5) then B150
{-# LINE 534 "Terminal.hsc" #-}
    else if x == (6) then B200
{-# LINE 535 "Terminal.hsc" #-}
    else if x == (7) then B300
{-# LINE 536 "Terminal.hsc" #-}
    else if x == (8) then B600
{-# LINE 537 "Terminal.hsc" #-}
    else if x == (9) then B1200
{-# LINE 538 "Terminal.hsc" #-}
    else if x == (10) then B1800
{-# LINE 539 "Terminal.hsc" #-}
    else if x == (11) then B2400
{-# LINE 540 "Terminal.hsc" #-}
    else if x == (12) then B4800
{-# LINE 541 "Terminal.hsc" #-}
    else if x == (13) then B9600
{-# LINE 542 "Terminal.hsc" #-}
    else if x == (14) then B19200
{-# LINE 543 "Terminal.hsc" #-}
    else if x == (15) then B38400
{-# LINE 544 "Terminal.hsc" #-}
    else error "unknown baud rate"

-- Clear termios i_flag

clearInputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearInputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 551 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 554 "Terminal.hsc" #-}
      iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p2
{-# LINE 555 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p1 (iflag .&. complement flag)
{-# LINE 556 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios i_flag

setInputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setInputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 563 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 566 "Terminal.hsc" #-}
      iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p2
{-# LINE 567 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) p1 (iflag .|. flag)
{-# LINE 568 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios i_flag

testInputFlag :: CTcflag -> TerminalAttributes -> Bool
testInputFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p ->  do
    iflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 576 "Terminal.hsc" #-}
    return $! ((iflag .&. flag) /= 0)

-- Clear termios c_flag

clearControlFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearControlFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 583 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 586 "Terminal.hsc" #-}
      cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p2
{-# LINE 587 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p1 (cflag .&. complement flag)
{-# LINE 588 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios c_flag

setControlFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setControlFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 595 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 598 "Terminal.hsc" #-}
      cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p2
{-# LINE 599 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) p1 (cflag .|. flag)
{-# LINE 600 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios c_flag

testControlFlag :: CTcflag -> TerminalAttributes -> Bool
testControlFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p -> do
    cflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 608 "Terminal.hsc" #-}
    return $! ((cflag .&. flag) /= 0)

-- Clear termios l_flag

clearLocalFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearLocalFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 615 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 618 "Terminal.hsc" #-}
      lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p2
{-# LINE 619 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p1 (lflag .&. complement flag)
{-# LINE 620 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios l_flag

setLocalFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setLocalFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 627 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 630 "Terminal.hsc" #-}
      lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p2
{-# LINE 631 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) p1 (lflag .|. flag)
{-# LINE 632 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios l_flag

testLocalFlag :: CTcflag -> TerminalAttributes -> Bool
testLocalFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p ->  do
    lflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 640 "Terminal.hsc" #-}
    return $! ((lflag .&. flag) /= 0)

-- Clear termios o_flag

clearOutputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
clearOutputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 647 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 650 "Terminal.hsc" #-}
      oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p2
{-# LINE 651 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p1 (oflag .&. complement flag)
{-# LINE 652 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Set termios o_flag

setOutputFlag :: CTcflag -> TerminalAttributes -> TerminalAttributes
setOutputFlag flag termios = unsafePerformIO $ do
  fp <- mallocForeignPtrBytes (60)
{-# LINE 659 "Terminal.hsc" #-}
  withForeignPtr fp $ \p1 -> do
    withTerminalAttributes termios $ \p2 -> do
      copyBytes p1 p2 (60) 
{-# LINE 662 "Terminal.hsc" #-}
      oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p2
{-# LINE 663 "Terminal.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4)) p1 (oflag .|. flag)
{-# LINE 664 "Terminal.hsc" #-}
  return $ makeTerminalAttributes fp

-- Examine termios o_flag

testOutputFlag :: CTcflag -> TerminalAttributes -> Bool
testOutputFlag flag termios = unsafePerformIO $
  withTerminalAttributes termios $ \p -> do
    oflag <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 672 "Terminal.hsc" #-}
    return $! ((oflag .&. flag) /= 0)

withNewTermios :: TerminalAttributes -> (Ptr CTermios -> IO a) 
  -> IO TerminalAttributes
withNewTermios termios action = do
  fp1 <- mallocForeignPtrBytes (60)
{-# LINE 678 "Terminal.hsc" #-}
  withForeignPtr fp1 $ \p1 -> do
   withTerminalAttributes termios $ \p2 -> do
    copyBytes p1 p2 (60)
{-# LINE 681 "Terminal.hsc" #-}
    action p1
  return $ makeTerminalAttributes fp1
