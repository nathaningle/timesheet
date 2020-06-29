{- |
Module      : Validate
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Validate timesheets according to business rules.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Validate where

import           WorkDay

import           Control.Monad        (when)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Time.Calendar   (Day)


toilLimit :: Int
toilLimit = 2250  -- 35.7 hours

noBreakLimit :: Int
noBreakLimit = 300  -- 5 hours

minBreak :: Int
minBreak = 30


data ValidationError = ValidationError Day Violation deriving (Eq, Ord, Show)

data Violation = NegBalance Int
               | ExcessTOIL Int
               | NeedABreak Int
               deriving (Eq, Ord, Show)


validate :: WorkDay -> StateT Int (Writer [ValidationError]) ()
validate WorkDay{..} = do
  bal <- get
  let bal' = bal + workBalance wdWork
  put bal'

  -- Business logic.
  when (bal' < 0) $ do
    tell [ValidationError wdDay (NegBalance bal')]
    put 0
  when (bal' > toilLimit) $ do
    tell [ValidationError wdDay (ExcessTOIL bal')]
    put toilLimit
  validateBreaks wdDay wdWork

-- | Decision: don't clamp balance here; shouldn't lose TOIL for working too
-- long without a break.
validateBreaks :: MonadWriter [ValidationError] m => Day -> Work -> m ()
validateBreaks day (Work intvls) = mapM_ validateInterval $ aggregateBreaks intvls
  where
    validateInterval intvl = let d = duration intvl
                             in when (d > noBreakLimit) $ tell [ValidationError day (NeedABreak d)]
validateBreaks _ TOIL = pure ()
validateBreaks _ Training = pure ()
validateBreaks _ Leave = pure ()

aggregateBreaks :: [Interval] -> [Interval]
aggregateBreaks [] = []
aggregateBreaks [intvl] = [intvl]
aggregateBreaks (i1:i2:intvls)
  | minutesBetween i1 i2 < minBreak = aggregateBreaks (joinInterval i1 i2:intvls)
  | otherwise = i1 : aggregateBreaks (i2:intvls)
