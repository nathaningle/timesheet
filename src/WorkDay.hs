{- |
Module      : WorkDay
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Calculate time worked from timesheets.
-}
module WorkDay where

import           Data.Text          (Text)
import           Data.Time.Calendar (Day)


-- | Minutes scheduled in a regular working day.  Equal to 7.5 hours.
scheduledMinutes :: Int
scheduledMinutes = 450


-- | Time of some day represented as minutes since midnight.
newtype TimeOfDay = TimeOfDay Int deriving (Eq, Ord, Show)

-- | Minutes between two 'TimeOfDay's.  This is used like the @-@ operator; iff
-- the first argument is less than the second then the result will be negative.
minutesDiff :: TimeOfDay -> TimeOfDay -> Int
minutesDiff (TimeOfDay t2) (TimeOfDay t1) = t2 - t1


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


-- A description of activities undertaken in a working day.
data Work = Work [Interval]
          | TOIL
          | Training
          | Leave
          deriving (Eq, Ord, Show)

-- | A complete record of a working day.
data DayRecord = DayRecord { drDay     :: Day
                           , drWork    :: Work
                           , drComment :: Maybe Text
                           } deriving (Eq, Ord, Show)

-- | Calculate the balance of minutes worked versus scheduled.
workBalance :: Work -> Int
workBalance (Work intvls) = sum $ map duration intvls
workBalance TOIL          = negate scheduledMinutes
workBalance Training      = 0
workBalance Leave         = 0

-- | Calculate the balance of minutes worked versus scheduled.
dayBalance :: DayRecord -> Int
dayBalance = workBalance . drWork
