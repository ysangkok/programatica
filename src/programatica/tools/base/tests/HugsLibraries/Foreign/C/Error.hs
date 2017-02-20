-----------------------------------------------------------------------------
-- |
-- Module      :  Foreign.C.Error
-- Copyright   :  (c) The FFI task force 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  ffi@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- C-specific Marshalling support: Handling of C \"errno\" error codes
--
-----------------------------------------------------------------------------

module Foreign.C.Error (

  -- Haskell representation for "errno" values
  --
  Errno(..),		-- instance: Eq
  eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,
                        -- :: Errno
  isValidErrno,		-- :: Errno -> Bool

  -- access to the current thread's "errno" value
  --
  getErrno,             -- :: IO Errno
  resetErrno,           -- :: IO ()

  -- conversion of an "errno" value into IO error
  --
  errnoToIOError,       -- :: String       -- location
                        -- -> Errno        -- errno
                        -- -> Maybe Handle -- handle
                        -- -> Maybe String -- filename
                        -- -> IOError

  -- throw current "errno" value
  --
  throwErrno,           -- ::                String               -> IO a

  -- guards for IO operations that may fail
  --
  throwErrnoIf,         -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIf_,        -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfRetry,    -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIfRetry_,   -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfMinus1,   -- :: Num a 
			-- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1_,  -- :: Num a 
			-- =>                String -> IO a       -> IO ()
  throwErrnoIfMinus1Retry,  
			-- :: Num a 
			-- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1Retry_,  
			-- :: Num a 
			-- =>                String -> IO a       -> IO ()
  throwErrnoIfNull,	-- ::                String -> IO (Ptr a) -> IO (Ptr a)
  throwErrnoIfNullRetry,-- ::                String -> IO (Ptr a) -> IO (Ptr a)

  throwErrnoIfRetryMayBlock, 
  throwErrnoIfRetryMayBlock_,
  throwErrnoIfMinus1RetryMayBlock,
  throwErrnoIfMinus1RetryMayBlock_,  
  throwErrnoIfNullRetryMayBlock
) where

-- this is were we get the CONST_XXX definitions from that configure
-- calculated for us
--

-- system dependent imports
-- ------------------------

-- GHC allows us to get at the guts inside IO errors/exceptions
--

-- regular imports
-- ---------------

import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Error 	( void )
import Data.Maybe

import System.IO		( IOError, Handle, ioError )
import System.IO.Unsafe		( unsafePerformIO )

{-# CBITS errno.c #-}

-- "errno" type
-- ------------

-- import of C function that gives address of errno
-- This function exists because errno is a variable on some systems, but on
-- Windows it is a macro for a function...
-- [yes, global variables and thread safety don't really go hand-in-hand. -- sof]

foreign import ccall unsafe "HsBase.h ghcErrno" _errno :: Ptr CInt

-- Haskell representation for "errno" values
--
newtype Errno = Errno CInt

instance Eq Errno where
  errno1@(Errno no1) == errno2@(Errno no2) 
    | isValidErrno errno1 && isValidErrno errno2 = no1 == no2
    | otherwise					 = False

-- common "errno" symbols
--
eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV		       :: Errno
--
-- the cCONST_XXX identifiers are cpp symbols whose value is computed by
-- configure 
--
eOK             = Errno 0

e2BIG           = Errno (7)
eACCES		= Errno (13)
eADDRINUSE	= Errno (98)
eADDRNOTAVAIL	= Errno (99)
eADV		= Errno (68)
eAFNOSUPPORT	= Errno (97)
eAGAIN		= Errno (11)
eALREADY	= Errno (114)
eBADF		= Errno (9)
eBADMSG		= Errno (74)
eBADRPC		= Errno (-1)
eBUSY		= Errno (16)
eCHILD		= Errno (10)
eCOMM		= Errno (70)
eCONNABORTED	= Errno (103)
eCONNREFUSED	= Errno (111)
eCONNRESET	= Errno (104)
eDEADLK		= Errno (35)
eDESTADDRREQ	= Errno (89)
eDIRTY		= Errno (-1)
eDOM		= Errno (33)
eDQUOT		= Errno (122)
eEXIST		= Errno (17)
eFAULT		= Errno (14)
eFBIG		= Errno (27)
eFTYPE		= Errno (-1)
eHOSTDOWN	= Errno (112)
eHOSTUNREACH	= Errno (113)
eIDRM		= Errno (43)
eILSEQ		= Errno (84)
eINPROGRESS	= Errno (115)
eINTR		= Errno (4)
eINVAL		= Errno (22)
eIO		= Errno (5)
eISCONN		= Errno (106)
eISDIR		= Errno (21)
eLOOP		= Errno (40)
eMFILE		= Errno (24)
eMLINK		= Errno (31)
eMSGSIZE	= Errno (90)
eMULTIHOP	= Errno (72)
eNAMETOOLONG	= Errno (36)
eNETDOWN	= Errno (100)
eNETRESET	= Errno (102)
eNETUNREACH	= Errno (101)
eNFILE		= Errno (23)
eNOBUFS		= Errno (105)
eNODATA		= Errno (61)
eNODEV		= Errno (19)
eNOENT		= Errno (2)
eNOEXEC		= Errno (8)
eNOLCK		= Errno (37)
eNOLINK		= Errno (67)
eNOMEM		= Errno (12)
eNOMSG		= Errno (42)
eNONET		= Errno (64)
eNOPROTOOPT	= Errno (92)
eNOSPC		= Errno (28)
eNOSR		= Errno (63)
eNOSTR		= Errno (60)
eNOSYS		= Errno (38)
eNOTBLK		= Errno (15)
eNOTCONN	= Errno (107)
eNOTDIR		= Errno (20)
eNOTEMPTY	= Errno (39)
eNOTSOCK	= Errno (88)
eNOTTY		= Errno (25)
eNXIO		= Errno (6)
eOPNOTSUPP	= Errno (95)
ePERM		= Errno (1)
ePFNOSUPPORT	= Errno (96)
ePIPE		= Errno (32)
ePROCLIM	= Errno (-1)
ePROCUNAVAIL	= Errno (-1)
ePROGMISMATCH	= Errno (-1)
ePROGUNAVAIL	= Errno (-1)
ePROTO		= Errno (71)
ePROTONOSUPPORT = Errno (93)
ePROTOTYPE	= Errno (91)
eRANGE		= Errno (34)
eREMCHG		= Errno (78)
eREMOTE		= Errno (66)
eROFS		= Errno (30)
eRPCMISMATCH	= Errno (-1)
eRREMOTE	= Errno (-1)
eSHUTDOWN	= Errno (108)
eSOCKTNOSUPPORT = Errno (94)
eSPIPE		= Errno (29)
eSRCH		= Errno (3)
eSRMNT		= Errno (69)
eSTALE		= Errno (116)
eTIME		= Errno (62)
eTIMEDOUT	= Errno (110)
eTOOMANYREFS	= Errno (109)
eTXTBSY		= Errno (26)
eUSERS		= Errno (87)
eWOULDBLOCK	= Errno (11)
eXDEV		= Errno (18)

-- checks whether the given errno value is supported on the current
-- architecture
--
isValidErrno               :: Errno -> Bool
--
-- the configure script sets all invalid "errno"s to -1
--
isValidErrno (Errno errno)  = errno /= -1

-- access to the current thread's "errno" value
-- --------------------------------------------

-- yield the current thread's "errno" value
--
getErrno :: IO Errno
getErrno  = do e <- peek _errno; return (Errno e)

-- set the current thread's "errno" value to 0
--
resetErrno :: IO ()
resetErrno  = poke _errno 0

-- throw current "errno" value
-- ---------------------------

-- the common case: throw an IO error based on a textual description
-- of the error location and the current thread's "errno" value
--
throwErrno     :: String -> IO a
throwErrno loc  =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing Nothing)

-- guards for IO operations that may fail
-- --------------------------------------

-- guard an IO operation and throw an "errno" based exception of the result
-- value of the IO operation meets the given predicate
--
throwErrnoIf            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf pred loc f  = 
  do
    res <- f
    if pred res then throwErrno loc else return res

-- as `throwErrnoIf', but discards the result
--
throwErrnoIf_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ pred loc f  = void $ throwErrnoIf pred loc f

-- as `throwErrnoIf', but retries interrupted IO operations (ie, those whose
-- flag `EINTR')
--
throwErrnoIfRetry            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry pred loc f  = 
  do
    res <- f
    if pred res
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfRetry pred loc f
	  else throwErrno loc
      else return res

-- as `throwErrnoIfRetry', but checks for operations that would block and
-- executes an alternative action in that case.

throwErrnoIfRetryMayBlock  :: (a -> Bool) -> String -> IO a -> IO b -> IO a
throwErrnoIfRetryMayBlock pred loc f on_block  = 
  do
    res <- f
    if pred res
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfRetryMayBlock pred loc f on_block
          else if err == eWOULDBLOCK || err == eAGAIN
	         then do on_block; throwErrnoIfRetryMayBlock pred loc f on_block
                 else throwErrno loc
      else return res

-- as `throwErrnoIfRetry', but discards the result
--
throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

-- as `throwErrnoIfRetryMayBlock', but discards the result
--
throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ pred loc f on_block 
  = void $ throwErrnoIfRetryMayBlock pred loc f on_block

-- throws "errno" if a result of "-1" is returned
--
throwErrnoIfMinus1 :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1  = throwErrnoIf (== -1)

-- as `throwErrnoIfMinus1', but discards the result
--
throwErrnoIfMinus1_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1_  = throwErrnoIf_ (== -1)

-- throws "errno" if a result of "-1" is returned, but retries in case of an
-- interrupted operation
--
throwErrnoIfMinus1Retry :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)

-- as `throwErrnoIfMinus1', but discards the result
--
throwErrnoIfMinus1Retry_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_  = throwErrnoIfRetry_ (== -1)

-- as throwErrnoIfMinus1Retry, but checks for operations that would block
--
throwErrnoIfMinus1RetryMayBlock :: Num a => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock  = throwErrnoIfRetryMayBlock (== -1)

-- as `throwErrnoIfMinus1RetryMayBlock', but discards the result
--
throwErrnoIfMinus1RetryMayBlock_ :: Num a => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_  = throwErrnoIfRetryMayBlock_ (== -1)

-- throws "errno" if a result of a NULL pointer is returned
--
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

-- throws "errno" if a result of a NULL pointer is returned, but retries in
-- case of an interrupted operation
--
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)

-- as throwErrnoIfNullRetry, but checks for operations that would block
--
throwErrnoIfNullRetryMayBlock :: String -> IO (Ptr a) -> IO b -> IO (Ptr a)
throwErrnoIfNullRetryMayBlock  = throwErrnoIfRetryMayBlock (== nullPtr)

-- conversion of an "errno" value into IO error
-- --------------------------------------------

-- convert a location string, an "errno" value, an optional handle,
-- and an optional filename into a matching IO error
--
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError loc errno maybeHdl maybeName = unsafePerformIO $ do
    str <- strerror errno >>= peekCString
    return (userError (loc ++ ": " ++ str ++ maybe "" (": "++) maybeName))

foreign import ccall unsafe "string.h" strerror :: Errno -> IO (Ptr CChar)
