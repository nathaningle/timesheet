{- |
File        : timesheet.hs
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
import           WorkDay              (WorkDay (wdWork), isCovidDay2020,
                                       isHomeDay, ppWorkDay, workMinutes)

import           Data.Attoparsec.Text (endOfInput, parseOnly)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO


main :: IO ()
main = do
  input <- TIO.getContents
  case parseOnly (parseLines <* endOfInput) input of
    Left err   -> error err
    Right recs -> do
      let recs' = filter (\r -> isCovidDay2020 r && isHomeDay r) recs
          homeMins = sum $ map (workMinutes . wdWork) recs'
          homeHours = fromIntegral homeMins / 60.0 :: Double
      mapM_ (TIO.putStrLn . ppWorkDay) recs'
      putStrLn $ replicate 79 '='
      TIO.putStrLn $ "Hours worked from home between 1 March and 30 June 2020: " <> T.pack (show homeHours)
