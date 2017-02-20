{-# LINE 1 "Time.hsc" #-}
-----------------------------------------------------------------------------
{-# LINE 2 "Time.hsc" #-}
-- |
-- Module      :  System.Time
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/core/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The standard Time library.
--
-----------------------------------------------------------------------------

{-
Haskell 98 Time of Day Library
------------------------------

The Time library provides standard functionality for clock times,
including timezone information (i.e, the functionality of "time.h",
adapted to the Haskell environment), It follows RFC 1129 in its use of
Coordinated Universal Time (UTC).

2000/06/17 <michael.weber@post.rwth-aachen.de>:
RESTRICTIONS:
  * min./max. time diff currently is restricted to
    [minBound::Int, maxBound::Int]

  * surely other restrictions wrt. min/max bounds


NOTES:
  * printing times

    `showTime' (used in `instance Show ClockTime') always prints time
    converted to the local timezone (even if it is taken from
    `(toClockTime . toUTCTime)'), whereas `calendarTimeToString'
    honors the tzone & tz fields and prints UTC or whatever timezone
    is stored inside CalendarTime.

    Maybe `showTime' should be changed to use UTC, since it would
    better correspond to the actual representation of `ClockTime'
    (can be done by replacing localtime(3) by gmtime(3)).


BUGS:
  * add proper handling of microsecs, currently, they're mostly
    ignored

  * `formatFOO' case of `%s' is currently broken...


TODO:
  * check for unusual date cases, like 1970/1/1 00:00h, and conversions
    between different timezone's etc.

  * check, what needs to be in the IO monad, the current situation
    seems to be a bit inconsistent to me

  * check whether `isDst = -1' works as expected on other arch's
    (Solaris anyone?)

  * add functions to parse strings to `CalendarTime' (some day...)

  * implement padding capabilities ("%_", "%-") in `formatFOO'

  * add rfc822 timezone (+0200 is CEST) representation ("%z") in `formatFOO'
-}

module System.Time
     (
        Month(..)
     ,  Day(..)

     ,  ClockTime(..) -- non-standard, lib. report gives this as abstract
	-- instance Eq, Ord
	-- instance Show (non-standard)

     ,	getClockTime

     ,  TimeDiff(..)
     ,  noTimeDiff      -- non-standard (but useful when constructing TimeDiff vals.)
     ,  diffClockTimes
     ,  addToClockTime

     ,  normalizeTimeDiff -- non-standard
     ,  timeDiffToString  -- non-standard
     ,  formatTimeDiff    -- non-standard

     ,  CalendarTime(..)
     ,	toCalendarTime
     ,  toUTCTime
     ,  toClockTime
     ,  calendarTimeToString
     ,  formatCalendarTime

     ) where


{-# LINE 102 "Time.hsc" #-}

import Prelude

import Data.Ix
import System.Locale
import System.IO.Unsafe


{-# LINE 110 "Time.hsc" #-}
import Hugs.Time ( getClockTimePrim, toCalTimePrim, toClockTimePrim )

{-# LINE 115 "Time.hsc" #-}

-- One way to partition and give name to chunks of a year and a week:

data Month
 = January   | February | March    | April
 | May       | June     | July     | August
 | September | October  | November | December
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

data Day 
 = Sunday   | Monday | Tuesday | Wednesday
 | Thursday | Friday | Saturday
 deriving (Eq, Ord, Enum, Bounded, Ix, Read, Show)

-- @ClockTime@ is an abstract type, used for the internal clock time.
-- Clock times may be compared, converted to strings, or converted to an
-- external calendar time @CalendarTime@.

data ClockTime = TOD Integer 		-- Seconds since 00:00:00 on 1 Jan 1970
		     Integer		-- Picoseconds with the specified second
	       deriving (Eq, Ord)

-- When a ClockTime is shown, it is converted to a CalendarTime in the current
-- timezone and then printed.  FIXME: This is arguably wrong, since we can't
-- get the current timezone without being in the IO monad.

instance Show ClockTime where
    showsPrec _ t = showString (calendarTimeToString 
	  			 (unsafePerformIO (toCalendarTime t)))

{-
@CalendarTime@ is a user-readable and manipulable
representation of the internal $ClockTime$ type.  The
numeric fields have the following ranges.

\begin{verbatim}
Value         Range             Comments
-----         -----             --------

year    -maxInt .. maxInt       [Pre-Gregorian dates are inaccurate]
mon           0 .. 11           [Jan = 0, Dec = 11]
day           1 .. 31
hour          0 .. 23
min           0 .. 59
sec           0 .. 61           [Allows for two leap seconds]
picosec       0 .. (10^12)-1    [This could be over-precise?]
wday          0 .. 6            [Sunday = 0, Saturday = 6]
yday          0 .. 365          [364 in non-Leap years]
tz       -43200 .. 43200        [Variation from UTC in seconds]
\end{verbatim}

The {\em tzname} field is the name of the time zone.  The {\em isdst}
field indicates whether Daylight Savings Time would be in effect.
-}

data CalendarTime 
 = CalendarTime  {
     ctYear    :: Int,
     ctMonth   :: Month,
     ctDay     :: Int,
     ctHour    :: Int,
     ctMin     :: Int,
     ctSec     :: Int,
     ctPicosec :: Integer,
     ctWDay    :: Day,
     ctYDay    :: Int,
     ctTZName  :: String,
     ctTZ      :: Int,
     ctIsDST   :: Bool
 }
 deriving (Eq,Ord,Read,Show)

-- The @TimeDiff@ type records the difference between two clock times in
-- a user-readable way.

data TimeDiff
 = TimeDiff {
     tdYear    :: Int,
     tdMonth   :: Int,
     tdDay     :: Int,
     tdHour    :: Int,
     tdMin     :: Int,
     tdSec     :: Int,
     tdPicosec :: Integer -- not standard
   }
   deriving (Eq,Ord,Read,Show)

noTimeDiff :: TimeDiff
noTimeDiff = TimeDiff 0 0 0 0 0 0 0

-- -----------------------------------------------------------------------------
-- getClockTime returns the current time in its internal representation.

getClockTime :: IO ClockTime

{-# LINE 210 "Time.hsc" #-}
getClockTime = do
  (sec,usec) <- getClockTimePrim
  return (TOD (fromIntegral sec) ((fromIntegral usec) * 1000000))


{-# LINE 236 "Time.hsc" #-}

-- -----------------------------------------------------------------------------
-- addToClockTime d t adds a time difference d and a
-- clock time t to yield a new clock time.  The difference d
-- may be either positive or negative.  diffClockTimes t1 t2 returns 
-- the difference between two clock times t1 and t2 as a TimeDiff.

addToClockTime  :: TimeDiff  -> ClockTime -> ClockTime
addToClockTime (TimeDiff year mon day hour min sec psec) 
	       (TOD c_sec c_psec) = 
	let
	  sec_diff = toInteger sec +
                     60 * toInteger min +
                     3600 * toInteger hour +
                     24 * 3600 * toInteger day
	  cal      = toUTCTime (TOD (c_sec + sec_diff) (c_psec + psec))
                                                       -- FIXME! ^^^^
          new_mon  = fromEnum (ctMonth cal) + r_mon 
	  (month', yr_diff)
	    | new_mon < 0  = (toEnum (12 + new_mon), (-1))
	    | new_mon > 11 = (toEnum (new_mon `mod` 12), 1)
	    | otherwise    = (toEnum new_mon, 0)
	    
	  (r_yr, r_mon) = mon `quotRem` 12

          year' = ctYear cal + year + r_yr + yr_diff
	in
	toClockTime cal{ctMonth=month', ctYear=year'}

diffClockTimes  :: ClockTime -> ClockTime -> TimeDiff
-- diffClockTimes is meant to be the dual to `addToClockTime'.
-- If you want to have the TimeDiff properly splitted, use
-- `normalizeTimeDiff' on this function's result
--
-- CAVEAT: see comment of normalizeTimeDiff
diffClockTimes (TOD sa pa) (TOD sb pb) =
    noTimeDiff{ tdSec     = fromIntegral (sa - sb) 
                -- FIXME: can handle just 68 years...
              , tdPicosec = pa - pb
              }


normalizeTimeDiff :: TimeDiff -> TimeDiff
-- FIXME: handle psecs properly
-- FIXME: ?should be called by formatTimeDiff automagically?
--
-- when applied to something coming out of `diffClockTimes', you loose
-- the duality to `addToClockTime', since a year does not always have
-- 365 days, etc.
--
-- apply this function as late as possible to prevent those "rounding"
-- errors
normalizeTimeDiff td =
  let
      rest0 = tdSec td 
               + 60 * (tdMin td 
                    + 60 * (tdHour td 
                         + 24 * (tdDay td 
                              + 30 * (tdMonth td 
                                   + 365 * tdYear td))))

      (diffYears,  rest1)    = rest0 `quotRem` (365 * 24 * 3600)
      (diffMonths, rest2)    = rest1 `quotRem` (30 * 24 * 3600)
      (diffDays,   rest3)    = rest2 `quotRem` (24 * 3600)
      (diffHours,  rest4)    = rest3 `quotRem` 3600
      (diffMins,   diffSecs) = rest4 `quotRem` 60
  in
      td{ tdYear = diffYears
        , tdMonth = diffMonths
        , tdDay   = diffDays
        , tdHour  = diffHours
        , tdMin   = diffMins
        , tdSec   = diffSecs
        }


{-# LINE 372 "Time.hsc" #-}

-- -----------------------------------------------------------------------------
-- toCalendarTime t converts t to a local time, modified by
-- the current timezone and daylight savings time settings.  toUTCTime
-- t converts t into UTC time.  toClockTime l converts l into the 
-- corresponding internal ClockTime.  The wday, yday, tzname, and isdst fields
-- are ignored.


toCalendarTime :: ClockTime -> IO CalendarTime

{-# LINE 383 "Time.hsc" #-}
toCalendarTime =  toCalTime False

{-# LINE 389 "Time.hsc" #-}

toUTCTime :: ClockTime -> CalendarTime

{-# LINE 392 "Time.hsc" #-}
toUTCTime      =  unsafePerformIO . toCalTime True

{-# LINE 398 "Time.hsc" #-}


{-# LINE 400 "Time.hsc" #-}
toCalTime :: Bool -> ClockTime -> IO CalendarTime
toCalTime toUTC (TOD s psecs)
  | (s > fromIntegral (maxBound :: Int)) || 
    (s < fromIntegral (minBound :: Int))
  = error ((if toUTC then "toUTCTime: " else "toCalendarTime: ") ++
           "clock secs out of range")
  | otherwise = do
    (sec,min,hour,mday,mon,year,wday,yday,isdst,zone,off) <- 
  		toCalTimePrim (if toUTC then 1 else 0) (fromIntegral s)
    return (CalendarTime{ ctYear=1900+year
  		        , ctMonth=toEnum mon
		        , ctDay=mday
		        , ctHour=hour
		        , ctMin=min
		        , ctSec=sec
		        , ctPicosec=psecs
		        , ctWDay=toEnum wday
		        , ctYDay=yday
		        , ctTZName=(if toUTC then "UTC" else zone)
		        , ctTZ=(if toUTC then 0 else off)
		        , ctIsDST=not toUTC && (isdst/=0)
		        })

{-# LINE 475 "Time.hsc" #-}

toClockTime :: CalendarTime -> ClockTime

{-# LINE 478 "Time.hsc" #-}
toClockTime (CalendarTime yr mon mday hour min sec psec
			  _wday _yday _tzname tz _isdst) =
  unsafePerformIO $ do
    s <- toClockTimePrim (yr-1900) (fromEnum mon) mday hour min sec tz
    return (TOD (fromIntegral s) psec)

{-# LINE 526 "Time.hsc" #-}

-- -----------------------------------------------------------------------------
-- Converting time values to strings.

calendarTimeToString  :: CalendarTime -> String
calendarTimeToString  =  formatCalendarTime defaultTimeLocale "%c"

formatCalendarTime :: TimeLocale -> String -> CalendarTime -> String
formatCalendarTime l fmt (CalendarTime year mon day hour min sec _
                                       wday yday tzname _ _) =
        doFmt fmt
  where doFmt ('%':'-':cs) = doFmt ('%':cs) -- padding not implemented
        doFmt ('%':'_':cs) = doFmt ('%':cs) -- padding not implemented
        doFmt ('%':c:cs)   = decode c ++ doFmt cs
        doFmt (c:cs) = c : doFmt cs
        doFmt "" = ""

        decode 'A' = fst (wDays l  !! fromEnum wday) -- day of the week, full name
        decode 'a' = snd (wDays l  !! fromEnum wday) -- day of the week, abbrev.
        decode 'B' = fst (months l !! fromEnum mon)  -- month, full name
        decode 'b' = snd (months l !! fromEnum mon)  -- month, abbrev
        decode 'h' = snd (months l !! fromEnum mon)  -- ditto
        decode 'C' = show2 (year `quot` 100)         -- century
        decode 'c' = doFmt (dateTimeFmt l)           -- locale's data and time format.
        decode 'D' = doFmt "%m/%d/%y"
        decode 'd' = show2 day                       -- day of the month
        decode 'e' = show2' day                      -- ditto, padded
        decode 'H' = show2 hour                      -- hours, 24-hour clock, padded
        decode 'I' = show2 (to12 hour)               -- hours, 12-hour clock
        decode 'j' = show3 yday                      -- day of the year
        decode 'k' = show2' hour                     -- hours, 24-hour clock, no padding
        decode 'l' = show2' (to12 hour)              -- hours, 12-hour clock, no padding
        decode 'M' = show2 min                       -- minutes
        decode 'm' = show2 (fromEnum mon+1)          -- numeric month
        decode 'n' = "\n"
        decode 'p' = (if hour < 12 then fst else snd) (amPm l) -- am or pm
        decode 'R' = doFmt "%H:%M"
        decode 'r' = doFmt (time12Fmt l)
        decode 'T' = doFmt "%H:%M:%S"
        decode 't' = "\t"
        decode 'S' = show2 sec			     -- seconds
        decode 's' = show2 sec			     -- number of secs since Epoch. (ToDo.)
        decode 'U' = show2 ((yday + 7 - fromEnum wday) `div` 7) -- week number, starting on Sunday.
        decode 'u' = show (let n = fromEnum wday in  -- numeric day of the week (1=Monday, 7=Sunday)
                           if n == 0 then 7 else n)
        decode 'V' =                                 -- week number (as per ISO-8601.)
            let (week, days) =                       -- [yep, I've always wanted to be able to display that too.]
                   (yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `divMod` 7
            in  show2 (if days >= 4 then
                          week+1 
                       else if week == 0 then 53 else week)

        decode 'W' =				     -- week number, weeks starting on monday
            show2 ((yday + 7 - if fromEnum wday > 0 then 
                               fromEnum wday - 1 else 6) `div` 7)
        decode 'w' = show (fromEnum wday)            -- numeric day of the week, weeks starting on Sunday.
        decode 'X' = doFmt (timeFmt l)               -- locale's preferred way of printing time.
        decode 'x' = doFmt (dateFmt l)               -- locale's preferred way of printing dates.
        decode 'Y' = show year                       -- year, including century.
        decode 'y' = show2 (year `rem` 100)          -- year, within century.
        decode 'Z' = tzname                          -- timezone name
        decode '%' = "%"
        decode c   = [c]


show2, show2', show3 :: Int -> String
show2 x
 | x' < 10   = '0': show x'
 | otherwise = show x'
 where x' = x `rem` 100

show2' x
 | x' < 10   = ' ': show x'
 | otherwise = show x'
 where x' = x `rem` 100

show3 x = show (x `quot` 100) ++ show2 (x `rem` 100)
 where x' = x `rem` 1000

to12 :: Int -> Int
to12 h = let h' = h `mod` 12 in if h' == 0 then 12 else h'

-- Useful extensions for formatting TimeDiffs.

timeDiffToString :: TimeDiff -> String
timeDiffToString = formatTimeDiff defaultTimeLocale "%c"

formatTimeDiff :: TimeLocale -> String -> TimeDiff -> String
formatTimeDiff l fmt td@(TimeDiff year month day hour min sec _)
 = doFmt fmt
  where 
   doFmt ""         = ""
   doFmt ('%':'-':cs) = doFmt ('%':cs) -- padding not implemented
   doFmt ('%':'_':cs) = doFmt ('%':cs) -- padding not implemented
   doFmt ('%':c:cs) = decode c ++ doFmt cs
   doFmt (c:cs)     = c : doFmt cs

   decode spec =
    case spec of
      'B' -> fst (months l !! fromEnum month)
      'b' -> snd (months l !! fromEnum month)
      'h' -> snd (months l !! fromEnum month)
      'c' -> defaultTimeDiffFmt td
      'C' -> show2 (year `quot` 100)
      'D' -> doFmt "%m/%d/%y"
      'd' -> show2 day
      'e' -> show2' day
      'H' -> show2 hour
      'I' -> show2 (to12 hour)
      'k' -> show2' hour
      'l' -> show2' (to12 hour)
      'M' -> show2 min
      'm' -> show2 (fromEnum month + 1)
      'n' -> "\n"
      'p' -> (if hour < 12 then fst else snd) (amPm l)
      'R' -> doFmt "%H:%M"
      'r' -> doFmt (time12Fmt l)
      'T' -> doFmt "%H:%M:%S"
      't' -> "\t"
      'S' -> show2 sec
      's' -> show2 sec -- Implementation-dependent, sez the lib doc..
      'X' -> doFmt (timeFmt l)
      'x' -> doFmt (dateFmt l)
      'Y' -> show year
      'y' -> show2 (year `rem` 100)
      '%' -> "%"
      c   -> [c]

   defaultTimeDiffFmt (TimeDiff year month day hour min sec _) =
       foldr (\ (v,s) rest -> 
                  (if v /= 0 
                     then show v ++ ' ':(addS v s)
                       ++ if null rest then "" else ", "
                     else "") ++ rest
             )
             ""
             (zip [year, month, day, hour, min, sec] (intervals l))

   addS v s = if abs v == 1 then fst s else snd s


{-# LINE 700 "Time.hsc" #-}
