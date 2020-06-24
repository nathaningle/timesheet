{- |
Module      : WorkDay
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Calculate time worked from timesheets.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module WorkDay where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           Text.Printf        (printf)


-- | Minutes scheduled in a regular working day.  Equal to 7.5 hours.
regulationMinutes :: Int
regulationMinutes = 450


-- | Show a two-digit 'Int', with a leading zero if necessary.
ppInt :: Int -> Text
ppInt x
  | x < 10 = "0" <> T.pack (show x)
  | otherwise = T.pack (show x)


-- | Time of some day represented as minutes since midnight.
newtype TimeOfDay = TimeOfDay Int deriving (Eq, Ord, Show)

-- | Minutes between two 'TimeOfDay's.  This is used like the @-@ operator; iff
-- the first argument is less than the second then the result will be negative.
minutesDiff :: TimeOfDay -> TimeOfDay -> Int
minutesDiff (TimeOfDay t2) (TimeOfDay t1) = t2 - t1

-- Format a 'TimeOfDay' for pretty-printing.
ppTimeOfDay :: TimeOfDay -> Text
ppTimeOfDay (TimeOfDay t) = ppInt hours <> ":" <> ppInt mins
  where (hours, mins) = t `divMod` 60


-- | A period between two 'TimeOfDay's.  Valid iff the first value is less than
-- (or, for compatibility, equal to) the second.
data Interval = Interval TimeOfDay TimeOfDay deriving (Eq, Ord, Show)

-- | Construct a valid 'Interval'.
makeInterval :: TimeOfDay -> TimeOfDay -> Maybe Interval
makeInterval (TimeOfDay t1) (TimeOfDay t2)
  | 0 <= t1 && t1 <= t2 = Just $ Interval (TimeOfDay t1) (TimeOfDay t2)
  | otherwise = Nothing

-- | Minutes elapsed during an 'Interval'.  This is never negative.
duration :: Interval -> Int
duration (Interval t1 t2) = t2 `minutesDiff` t1

-- | Format an 'Interval' for pretty-printing.
ppInterval :: Interval -> Text
ppInterval (Interval tod1 tod2) = ppTimeOfDay tod1 <> "  " <> ppTimeOfDay tod2


-- A description of activities undertaken in a working day.
data Work = Work [Interval]
          | TOIL
          | Training
          | Leave
          deriving (Eq, Ord, Show)

-- | Calculate the total minutes worked.
workMinutes :: Work -> Int
workMinutes (Work intvls) = sum (map duration intvls)
workMinutes TOIL          = 0
workMinutes Training      = 0
workMinutes Leave         = 0

-- | Minutes scheduled for this type of work day.
scheduledMinutes :: Work -> Int
scheduledMinutes (Work _) = regulationMinutes
scheduledMinutes TOIL     = regulationMinutes
scheduledMinutes Training = 0
scheduledMinutes Leave    = 0

-- | Calculate the balance of minutes worked versus scheduled.
workBalance :: Work -> Int
workBalance w = workMinutes w - scheduledMinutes w

-- | Format a 'Work' for pretty-printing.
ppWork :: Work -> Text
ppWork w@(Work intvls) = T.pack $ printf "%-54s  |  %5.2f (%2d:%02d) [%6.2f]" inOuts hDec hours mins balHDec
  where
    inOuts = T.intercalate "  " $ map ppInterval intvls
    hDec = (fromIntegral (workMinutes w)) / 60.0 :: Double
    (hours, mins) = (workMinutes w) `divMod` 60
    balHDec = (fromIntegral (workBalance w)) / 60.0 :: Double
ppWork TOIL          = "TOIL"
ppWork Training      = "Training"
ppWork Leave         = "Leave"


-- | A complete record of a working day.
data DayRecord = DayRecord { drDay     :: Day
                           , drWork    :: Work
                           , drComment :: Maybe Text
                           } deriving (Eq, Ord, Show)

-- | Calculate the balance of minutes worked versus scheduled.
dayBalance :: DayRecord -> Int
dayBalance = workBalance . drWork

-- | Format a 'DayRecord' for pretty-printing.
ppDayRecord :: DayRecord -> Text
ppDayRecord DayRecord{..} = T.pack (show drDay) <> "  |  " <> ppWork drWork <> maybe "" ("  " <>) drComment

-- | Given an initial balance, calculate the final balance.
tallyMinutes :: Int -> [DayRecord] -> Int
tallyMinutes = foldr f
  where f DayRecord{..} bal = bal + workBalance drWork

-- | Format a balance in minutes for pretty-printing.
ppBalance :: Int -> Text
ppBalance bal
  | bal == 0  = "zero"
  | bal > 0   = hhmm <> " in credit"
  | otherwise = hhmm <> " in deficit"
  where
    (h, m) = abs bal `quotRem` 60
    hhmm = T.pack $ printf "%d:%02d" h m
