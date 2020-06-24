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
import           WorkDay              (ppBalance, ppDayRecord, tallyMinutes)

import           Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Data.Text.IO         as TIO


main :: IO ()
main = do
  input <- TIO.getContents
  case parseOnly (parseLines <* endOfInput) input of
    Left err   -> error err
    Right recs -> do
      mapM_ (TIO.putStrLn . ppDayRecord) recs
      putStrLn $ replicate 79 '='
      TIO.putStrLn $ "Balance is " <> ppBalance (tallyMinutes 0 recs) <> "."
