{- |
Module      : WorkDay.Test
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Test suite for module 'WorkDay'.
-}
{-# LANGUAGE OverloadedStrings #-}
module WorkDay.Test where

import           WorkDay

import           Data.Time.Calendar (fromGregorian)
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "WorkDay"
  [ testGroup "minutesDiff"
    [ testCase "Zero minus zero" $ (TimeOfDay 0 `minutesDiff` TimeOfDay 0) @?= 0
    , testCase "One minus zero"  $ (TimeOfDay 1 `minutesDiff` TimeOfDay 0) @?= 1
    , testCase "Zero minus one"  $ (TimeOfDay 0 `minutesDiff` TimeOfDay 1) @?= negate 1
    ]
  , testGroup "makeInterval"
    [ testCase "00:00 until 00:00" $ makeInterval (TimeOfDay 0) (TimeOfDay 0) @?= Just (Interval (TimeOfDay 0) (TimeOfDay 0))
    , testCase "00:00 until 00:01" $ makeInterval (TimeOfDay 0) (TimeOfDay 1) @?= Just (Interval (TimeOfDay 0) (TimeOfDay 1))
    , testCase "00:00 until 24:00" $ makeInterval (TimeOfDay 0) (TimeOfDay 1440) @?= Just (Interval (TimeOfDay 0) (TimeOfDay 1440))
    , testCase "24:00 until 24:00" $ makeInterval (TimeOfDay 1440) (TimeOfDay 1440) @?= Just (Interval (TimeOfDay 1440) (TimeOfDay 1440))
    , testCase "00:01 until 00:00" $ makeInterval (TimeOfDay 1) (TimeOfDay 0) @?= Nothing
    ]
  , testGroup "duration"
    [ testCase "00:00 until 00:00" $ duration (Interval (TimeOfDay 0) (TimeOfDay 0)) @?= 0
    , testCase "00:00 until 00:01" $ duration (Interval (TimeOfDay 0) (TimeOfDay 1)) @?= 1
    , testCase "00:00 until 24:00" $ duration (Interval (TimeOfDay 0) (TimeOfDay 1440)) @?= 1440
    , testCase "24:00 until 24:00" $ duration (Interval (TimeOfDay 1440) (TimeOfDay 1440)) @?= 0
    ]
  , testGroup "minutesBetween"
    [ testCase "00:00 until 00:00" $ let i1 = Interval (TimeOfDay 0) (TimeOfDay 0)
                                         i2 = Interval (TimeOfDay 0) (TimeOfDay 0)
                                     in minutesBetween i1 i2 @?= 0
    , testCase "00:00 until 00:01" $ let i1 = Interval (TimeOfDay 0) (TimeOfDay 0)
                                         i2 = Interval (TimeOfDay 1) (TimeOfDay 10)
                                     in minutesBetween i1 i2 @?= 1
    , testCase "00:00 until 24:00" $ let i1 = Interval (TimeOfDay 0) (TimeOfDay 0)
                                         i2 = Interval (TimeOfDay 1440) (TimeOfDay 1440)
                                     in minutesBetween i1 i2 @?= 1440
    , testCase "24:00 until 24:00" $ let i1 = Interval (TimeOfDay 0) (TimeOfDay 1440)
                                         i2 = Interval (TimeOfDay 1440) (TimeOfDay 1440)
                                     in minutesBetween i1 i2 @?= 0
    ]
  , testCase "joinInterval" $ let i1 = Interval (TimeOfDay 0) (TimeOfDay 10)
                                  i2 = Interval (TimeOfDay 20) (TimeOfDay 30)
                              in joinInterval i1 i2 @?= Interval (TimeOfDay 0) (TimeOfDay 30)
  , testGroup "workBalance"
    [ testCase "Ordinary work day, nominal hours" $
        let i1 = Interval (TimeOfDay (7 * 60)) (TimeOfDay (12 * 60))
            i2 = Interval (TimeOfDay (12 * 60 + 30)) (TimeOfDay (15 * 60))
            w = Work [i1, i2]
        in workBalance w @?= 0
    , testCase "Ordinary work day, 30min over" $
        let i1 = Interval (TimeOfDay (7 * 60)) (TimeOfDay (12 * 60))
            i2 = Interval (TimeOfDay (12 * 60 + 30)) (TimeOfDay (15 * 60 + 30))
            w = Work [i1, i2]
        in workBalance w @?= 30
    , testCase "Ordinary work day, 30min under" $
        let i1 = Interval (TimeOfDay (7 * 60)) (TimeOfDay (12 * 60))
            i2 = Interval (TimeOfDay (13 * 60)) (TimeOfDay (15 * 60))
            w = Work [i1, i2]
        in workBalance w @?= negate 30
    , testCase "TOIL" $ workBalance TOIL @?= negate (7 * 60 + 30)
    , testCase "Training" $ workBalance Training @?= 0
    , testCase "Leave" $ workBalance Leave @?= 0
    ]
    , testCase "tallyMinutes" $
        let i1 = Interval (TimeOfDay (7 * 60)) (TimeOfDay (12 * 60))
            i2 = Interval (TimeOfDay (12 * 60 + 30)) (TimeOfDay (15 * 60))
            d1 = WorkDay { wdDay     = fromGregorian 2020 01 01
                         , wdWork    = Work [i1, i2]
                         , wdComment = Nothing
                         }
            i3 = Interval (TimeOfDay (7 * 60)) (TimeOfDay (12 * 60))
            i4 = Interval (TimeOfDay (13 * 60)) (TimeOfDay (15 * 60))
            d2 = WorkDay { wdDay     = fromGregorian 2020 01 02
                         , wdWork    = Work [i3, i4]
                         , wdComment = Nothing
                         }
        in tallyMinutes 60 [d1, d2] @?= 30
  ]
