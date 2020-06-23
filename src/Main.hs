{- |
Module      : Main
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Calculate time worked from timesheets.
-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Parse                (parseLines)
import           WorkDay              (ppDayRecord)

import           Data.Attoparsec.Text (endOfInput, parseOnly)
import           Data.Text            (Text)
import qualified Data.Text.IO         as TIO


sampleInput :: Text
sampleInput = "2020-01-01\t0700\t1200\t1230\t1500  # Happy New Year  \n\n2020-01-02 8:10  08:50  09:10  12:10  13:10  14:55  15:10  16:30\n\n  # Holidays\n"


main :: IO ()
main = do
  case parseOnly (parseLines <* endOfInput) sampleInput of
    Left err   -> error err
    Right recs -> mapM_ (TIO.putStrLn . ppDayRecord) recs
