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
  [ testGroup "TimeOfDay IsString"
    [ testCase "00:00" $ "00:00" @?= TimeOfDay 0
    , testCase "00:01" $ "00:01" @?= TimeOfDay 1
    , testCase "00:10" $ "00:10" @?= TimeOfDay 10
    , testCase "10:00" $ "10:00" @?= TimeOfDay 600
    , testCase "24:00" $ "24:00" @?= TimeOfDay 1440
    ]
  , testGroup "minutesDiff"
    [ testCase "Zero minus zero" $ ("00:00" `minutesDiff` "00:00") @?= 0
    , testCase "One minus zero"  $ ("00:01" `minutesDiff` "00:00") @?= 1
    , testCase "Zero minus one"  $ ("00:00" `minutesDiff` "00:01") @?= negate 1
    ]
  , testGroup "makeInterval"
    [ testCase "00:00 until 00:00" $ makeInterval "00:00" "00:00" @?= Just (Interval "00:00" "00:00")
    , testCase "00:00 until 00:01" $ makeInterval "00:00" "00:01" @?= Just (Interval "00:00" "00:01")
    , testCase "00:00 until 24:00" $ makeInterval "00:00" "24:00" @?= Just (Interval "00:00" "24:00")
    , testCase "24:00 until 24:00" $ makeInterval "24:00" "24:00" @?= Just (Interval "24:00" "24:00")
    , testCase "00:01 until 00:00" $ makeInterval "00:01" "00:00" @?= Nothing
    ]
  , testGroup "duration"
    [ testCase "00:00 until 00:00" $ duration (Interval "00:00" "00:00") @?= 0
    , testCase "00:00 until 00:01" $ duration (Interval "00:00" "00:01") @?= 1
    , testCase "00:00 until 24:00" $ duration (Interval "00:00" "24:00") @?= 1440
    , testCase "24:00 until 24:00" $ duration (Interval "24:00" "24:00") @?= 0
    ]
  , testGroup "minutesBetween"
    [ testCase "00:00 until 00:00" $ let i1 = Interval "00:00" "00:00"
                                         i2 = Interval "00:00" "00:00"
                                     in minutesBetween i1 i2 @?= 0
    , testCase "00:00 until 00:01" $ let i1 = Interval "00:00" "00:00"
                                         i2 = Interval "00:01" "00:10"
                                     in minutesBetween i1 i2 @?= 1
    , testCase "00:00 until 24:00" $ let i1 = Interval "00:00" "00:00"
                                         i2 = Interval "24:00" "24:00"
                                     in minutesBetween i1 i2 @?= 1440
    , testCase "24:00 until 24:00" $ let i1 = Interval "00:00" "24:00"
                                         i2 = Interval "24:00" "24:00"
                                     in minutesBetween i1 i2 @?= 0
    ]
  , testCase "joinInterval" $ let i1 = Interval "00:00" "00:10"
                                  i2 = Interval "00:20" "00:30"
                              in joinInterval i1 i2 @?= Interval "00:00" "00:30"
  , testGroup "workBalance"
    [ testCase "Ordinary work day, nominal hours" $
        let i1 = Interval "07:00" "12:00"
            i2 = Interval "12:30" "15:00"
            w = Work [i1, i2]
        in workBalance w @?= 0
    , testCase "Ordinary work day, 30min over" $
        let i1 = Interval "07:00" "12:00"
            i2 = Interval "12:30" "15:30"
            w = Work [i1, i2]
        in workBalance w @?= 30
    , testCase "Ordinary work day, 30min under" $
        let i1 = Interval "07:00" "12:00"
            i2 = Interval "13:00" "15:00"
            w = Work [i1, i2]
        in workBalance w @?= negate 30
    , testCase "TOIL" $ workBalance TOIL @?= negate (7 * 60 + 30)
    , testCase "Training" $ workBalance Training @?= 0
    , testCase "Leave" $ workBalance Leave @?= 0
    ]
  , testCase "tallyMinutes" $
      let i1 = Interval "07:00" "12:00"
          i2 = Interval "12:30" "15:00"
          d1 = WorkDay { wdDay     = fromGregorian 2020 01 01
                       , wdWork    = Work [i1, i2]
                       , wdComment = Nothing
                       }
          i3 = Interval "07:00" "12:00"
          i4 = Interval "13:00" "15:00"
          d2 = WorkDay { wdDay     = fromGregorian 2020 01 02
                       , wdWork    = Work [i3, i4]
                       , wdComment = Nothing
                       }
      in tallyMinutes 60 [d1, d2] @?= 30
  ]
