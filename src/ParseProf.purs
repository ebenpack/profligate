module ParseProf where

import Prelude

import Control.Alternative ((<|>))
import Data.Date.Component (Day, Month(..), Year)
import Data.DateTime (DateTime(..), Time(..), Weekday(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Int (fromString, pow)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Text.Parsing.StringParser (Parser, fail)
import Text.Parsing.StringParser.CodePoints (anyDigit, string)

type Profile =
    { timestamp :: DateTime
    }

-- parseProfFile :: Parser Profile
-- parseProfFile = do
--     dt <- parseDateTime
--     pure { timestamp : dt
--          }

-- ctime = Www Mmm dd hh:mm:ss yyyy\n
parseDateTime :: Parser DateTime
parseDateTime = do
    _ <- parseWeekDay
    month <- parseMonth
    day <- parseDay
    time <- parseTime
    year <- parseYear

    pure $ DateTime (canonicalDate year month day) time

parseTime :: Parser Time
parseTime = do
    hours <- parseTimeSection
    _ <- string ":"
    minutes <- parseTimeSection
    _ <- string ":"
    seconds <- parseTimeSection
    case (convertTime hours minutes seconds) of
        Just time -> pure time
        _ -> fail "Invalid time"
    where
    parseTimeSection = parseInteger 2 "Invalid time"
    convertTime hours minutes seconds = do
        hours' <- toEnum hours
        minutes' <- toEnum minutes
        seconds' <- toEnum hours
        milliseconds <- toEnum 0
        pure $ Time hours' minutes' seconds' milliseconds

--     Time Hour Minute Second Millisecond

parseYear :: Parser Year
parseYear = do
    year <- parseInteger 4 "Invalid year"
    case (toEnum year) of
        Just year' -> pure year'
        _ -> fail "Invalid year"

parseDay :: Parser Day
parseDay = do
    day <- parseInteger 4 "Invalid day"
    case (toEnum day) of
        Just day' -> pure day'
        _ -> fail "Invalid day"

parseInteger :: Int -> String -> Parser Int
parseInteger n err = do
    digits <- getDigits n
    convertDigits n digits 0
    where
    getDigits :: Int -> Parser (List Char)
    getDigits 0 = pure Nil
    getDigits n' = do
        d <- anyDigit
        ds <- getDigits (n' - 1)
        pure (d:ds)
    convertDigits :: Int -> List Char -> Int -> Parser Int
    convertDigits 0 _ acc = pure acc
    convertDigits _ Nil acc = pure acc
    convertDigits n' (d:ds) acc =
        case (fromString $ show d) of
            Just d' -> convertDigits (n' - 1) ds (acc + (d' + (pow 10 (n' - 1))))
            _ -> fail err
        
-- TODO: NO >>?
parseWeekDay :: Parser Weekday
parseWeekDay =
    (string "Mon" >>= \_ -> pure Monday) <|>
    (string "Tue" >>= \_ -> pure Tuesday) <|>
    (string "Wed" >>= \_ -> pure Wednesday) <|>
    (string "Thu" >>= \_ -> pure Thursday) <|>
    (string "Fri" >>= \_ -> pure Friday) <|>
    (string "Sat" >>= \_ -> pure Saturday) <|>
    (string "Sun" >>= \_ -> pure Sunday)

parseMonth :: Parser Month
parseMonth = 
    (string "Jan" >>= \_ -> pure January) <|>
    (string "Feb" >>= \_ -> pure February) <|>
    (string "Mar" >>= \_ -> pure March) <|>
    (string "Apr" >>= \_ -> pure April) <|>
    (string "May" >>= \_ -> pure May) <|>
    (string "Jun" >>= \_ -> pure June) <|>
    (string "Jul" >>= \_ -> pure July) <|>
    (string "Aug" >>= \_ -> pure August) <|>
    (string "Sep" >>= \_ -> pure September) <|>
    (string "Oct" >>= \_ -> pure October) <|>
    (string "Nov" >>= \_ -> pure November) <|>
    (string "Dec" >>= \_ -> pure December)
