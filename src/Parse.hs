{- |
Module      : Parse
Copyright   : 2020 Nathan Ingle
License     : ISC

Maintainer  : elgni.nahtan@gmail.com
Stability   : experimental
Portability : non-portable

Calculate time worked from timesheets.
-}
{-# LANGUAGE OverloadedStrings #-}
module Parse where

import           WorkDay              (Interval, TimeOfDay (..), Work (..),
                                       WorkDay (..), makeInterval)

import           Control.Applicative  (empty, optional, (<|>))
import           Control.Monad        (guard)
import           Data.Attoparsec.Text
import           Data.Char            (isDigit)
import           Data.Functor         (($>))
import           Data.Maybe           (catMaybes)
import           Data.Text            (Text, strip)
import           Data.Time.Calendar   (Day, fromGregorianValid)


-- | Parse @n@ decimal digits to an unsigned number.
digitCount :: Read a => Int -> Parser a
digitCount n = read <$> count n (satisfy isDigit)

-- | Skip zero or more spaces and/or horizontal tabs.
skipHSpace :: Parser ()
skipHSpace = many' (char ' ' <|> char '\t') $> ()

-- | Skip one or more spaces and/or horizontal tabs.
skipHSpace1 :: Parser ()
skipHSpace1 = many1 (char ' ' <|> char '\t') $> ()

-- | Skip a single colon, if found.
optionalColon :: Parser ()
optionalColon = option () (char ':' $> ())


date :: Parser Day
date = do
  d <- fromGregorianValid <$> digitCount 4 <*> (char '-' *> digitCount 2) <*> (char '-' *> digitCount 2)
  maybe empty pure d

time :: Parser TimeOfDay
time = do
  h <- hoursReg <|> hoursAlt
  m <- digitCount 2
  let mins = (h * 60) + m
      allDay = h == 24 && m == 0
  guard $ allDay || (h <= 24 && m < 60 && mins < 1440)
  pure $ TimeOfDay mins
  where
    hoursReg = digitCount 2 <* optionalColon
    hoursAlt = digitCount 1 <* char ':'

interval :: Parser Interval
interval = do
  i <- makeInterval <$> time <*> (skipHSpace1 *> time)
  maybe empty pure i

comment :: Parser Text
comment = strip <$> (many1 commentStartChar *> takeTill isEndOfLine)
  where commentStartChar = char '#' <|> char '-'

spaceThenWork :: Parser Work
spaceThenWork = toil <|> leave <|> training <|> work
  where
    toil     = TOIL     <$ (skipHSpace1 *> asciiCI "toil")
    leave    = Leave    <$ (skipHSpace1 *> asciiCI "leave")
    training = Training <$ (skipHSpace1 *> asciiCI "training")
    work = Work <$> many1' (skipHSpace1 *> interval)

dayRecord :: Parser WorkDay
dayRecord = WorkDay <$> date <*> spaceThenWork <*> (skipHSpace *> optional comment)

parseLine :: Parser (Maybe WorkDay)
parseLine = emptyLine <|> commentLine <|> dayRecordLine
  where
    emptyLine = Nothing <$ (skipHSpace *> endOfLine)
    commentLine = Nothing <$ (skipHSpace *> char '#' *> takeTill isEndOfLine *> endOfLine)
    dayRecordLine = Just <$> (dayRecord <* endOfLine)

parseLines :: Parser [WorkDay]
parseLines = catMaybes <$> many' parseLine
