module ParseProf where

import Prelude

import Control.Alternative ((<|>))
import Data.Date.Component (Day, Month(..), Year)
import Data.DateTime (DateTime(..), Time(..), Weekday(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Int (fromString, pow, toNumber)
import Math
import Data.List (List(..), (:), length)
import Data.Array (fromFoldable)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, char, string, skipSpaces)
import Text.Parsing.StringParser.Combinators (option, sepBy1, many1, manyTill, many)

type TotalTime =
    { time :: Number
    , ticks :: Int
    , interval :: Int
    , processors :: Int
    }

type Profile =
    { timestamp :: DateTime
    , title     :: String
    , totalTime :: TotalTime
    , totalAlloc :: Int
    }

-- https://github.com/ghc/ghc/blob/1c2c2d3dfd4c36884b22163872feb87122b4528d/rts/ProfilerReport.c#L284

parseProfFile :: Parser Profile
parseProfFile = do
    timestamp <- skipSpaces *> parseDateTime
    title <- parseHeader *> skipSpaces *> parseTitle <* skipSpaces
    totalTime <- parseTotalTime <* skipSpaces
    totalAlloc <- parseTotalAlloc <* skipSpaces
    pure { timestamp: timestamp
         , title: title
         , totalTime: totalTime
         , totalAlloc: totalAlloc
         }

parseTotalAlloc :: Parser Int
parseTotalAlloc = do
    alloc <- skipSpaces *> string "total alloc" *> 
             skipSpaces *> string "=" *> 
             skipSpaces *> parseIntegerWithCommas <* 
             skipSpaces <* string "bytes" <* 
             skipSpaces <* string "(excludes profiling overheads)" <* skipSpaces
    pure alloc


parseTotalTime :: Parser TotalTime
parseTotalTime = do
    time <- skipSpaces *> string "total time" *>
            skipSpaces *> string "=" *>
            skipSpaces *> parseFloat <*
            skipSpaces <* string "secs"
    { ticks: ticks, interval: interval, processors: processors } <- do
        ticks <- string "(" *> parseInteger <* skipSpaces <* string "ticks @" <* skipSpaces
        interval <- parseInteger <* string "us," <* skipSpaces
        processors <- parseInteger <* skipSpaces <* string "processors)" <* skipSpaces
        pure { ticks: ticks, interval: interval, processors: processors }
    pure { time: time, ticks: ticks, interval: interval, processors: processors }

parseTitle :: Parser String
parseTitle = do
    title <- skipSpaces *> manyTill anyChar (string "\n" <|> string "\r\n")
    pure $ fromCharArray $ fromFoldable title

parseHeader :: Parser Unit
parseHeader = do
    string "Time and Allocation Profiling Report" *>
    skipSpaces *> 
    string "(Final)" *>
    skipSpaces *>
    pure unit

-- https://github.com/ghc/ghc/blob/21f0f56164f50844c2150c62f950983b2376f8b6/rts/RtsUtils.c#L160
-- https://en.cppreference.com/w/c/chrono/ctime
-- ctime = Www Mmm dd hh:mm:ss yyyy\n
parseDateTime :: Parser DateTime
parseDateTime = do
    month <- parseWeekDay *> skipSpaces *> parseMonth
    day <- skipSpaces *> parseDay
    time <- skipSpaces *> parseTime
    year <- skipSpaces *> parseYear <* skipSpaces
    pure $ DateTime (canonicalDate year month day) time

parseTime :: Parser Time
parseTime = do
    hours <- parseInteger <* string ":"
    minutes <- parseInteger
    seconds <- option 0 $ string ":" *> parseInteger
    case (convertTime hours minutes seconds) of
        Just time -> pure time
        _ -> fail "Invalid time"
    where
    convertTime hours minutes seconds = do
        hours' <- toEnum hours
        minutes' <- toEnum minutes
        seconds' <- toEnum seconds
        milliseconds <- toEnum 0
        pure $ Time hours' minutes' seconds' milliseconds

--     Time Hour Minute Second Millisecond

parseYear :: Parser Year
parseYear = do
    year <- parseInteger
    case (toEnum year) of
        Just year' -> pure year'
        _ -> fail "Invalid year"

parseDay :: Parser Day
parseDay = do
    day <- parseInteger
    case (toEnum day) of
        Just day' -> pure day'
        _ -> fail "Invalid day"

parseFloat :: Parser Number
parseFloat = do
    integer <- parseInteger
    fractional <- option 0 $ string "." *> parseInteger
    pure $ (toNumber integer) + (fractionalize $ toNumber fractional)
    where
    -- TODO: Use logs?
    fractionalize :: Number -> Number
    fractionalize n = if n > 1.0 then fractionalize (n / 10.0) else n

parseIntegerWithCommas :: Parser Int
parseIntegerWithCommas = do
    ints <- sepBy1 (char ',') anyDigit -- TODO: This is not very well-formed
    convertDigits (toList ints) 0 (length $ toList ints)

parseInteger :: Parser Int
parseInteger = do
    ds <- getDigits
    convertDigits ds 0 (length ds)
    where
    getDigits :: Parser (List Char)
    getDigits = do
        ds <- many1 anyDigit
        pure (toList ds)

convertDigits :: List Char -> Int -> Int -> Parser Int
convertDigits _ acc 0 = pure acc
convertDigits Nil acc _ = pure acc
convertDigits (d:ds) acc n' =
    let p = (pow 10 (n' - 1)) in
    case (fromString $ singleton d) of
        Just d' -> convertDigits ds (acc + (d' * p)) (n' - 1)
        _ -> fail "Invalid number"
        

parseWeekDay :: Parser Weekday
parseWeekDay =
    (string "Mon" *> pure Monday) <|>
    (string "Tue" *> pure Tuesday) <|>
    (string "Wed" *> pure Wednesday) <|>
    (string "Thu" *> pure Thursday) <|>
    (string "Fri" *> pure Friday) <|>
    (string "Sat" *> pure Saturday) <|>
    (string "Sun" *> pure Sunday)

parseMonth :: Parser Month
parseMonth = 
    (string "Jan" *> pure January) <|>
    (string "Feb" *> pure February) <|>
    (string "Mar" *> pure March) <|>
    (string "Apr" *> pure April) <|>
    (string "May" *> pure May) <|>
    (string "Jun" *> pure June) <|>
    (string "Jul" *> pure July) <|>
    (string "Aug" *> pure August) <|>
    (string "Sep" *> pure September) <|>
    (string "Oct" *> pure October) <|>
    (string "Nov" *> pure November) <|>
    (string "Dec" *> pure December)
