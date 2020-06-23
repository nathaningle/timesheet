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

import           Parse                (dayRecord)
import           WorkDay              (dayBalance)

import           Data.Attoparsec.Text (endOfInput, parseOnly)
import           Data.Text            (Text)


sampleInput :: Text
sampleInput = "2020-01-01\t0700\t1200\t1230\t1500  # Happy New Year  "


main :: IO ()
main = do
  case parseOnly (dayRecord <* endOfInput) sampleInput of
    Left err -> error err
    Right dr -> do
      print dr
      print $ dayBalance dr
