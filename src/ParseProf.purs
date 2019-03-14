module ParseProf where

import Prelude

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Date.Component (Day, Month(..), Year)
import Data.DateTime (DateTime(..), Time(..), Weekday(..), canonicalDate)
import Data.Enum (toEnum)
import Data.Number as Num
import Data.Int (fromString, pow, toNumber)
import Data.List (List(..), (:), length, concat)
import Data.List.NonEmpty (toList)
import Data.Tuple.Nested (Tuple5(..), T5, tuple5)
import Data.Map (Map(..), empty, lookup, insert, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Foldable (foldr)
import Data.String as S
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.Tuple (Tuple(..))
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, char, string, skipSpaces, satisfy)
import Text.Parsing.StringParser.Combinators (option, sepBy1, many1, manyTill, many)

type TotalTime =
    { time :: Number
    , ticks :: Int
    , interval :: Int
    , processors :: Int
    }

type PerCostCenterCosts =
    { name :: String
    , mod :: String
    , src :: String
    , time :: Number
    , alloc :: Number
    }

type PerCostCenterCostsColumnWidths =
    { costCenter :: Int
    , mod :: Int
    , src :: Int
    , time :: Int
    , alloc :: Int
    }

type Profile =
    { timestamp :: DateTime
    , title     :: String
    , totalTime :: TotalTime
    , totalAlloc :: Int
    , perCostCenterCosts :: List PerCostCenterCosts
    }

-- https://github.com/ghc/ghc/blob/6aaa0655a721605740f23e49c5b4bf6165bfe865/docs/users_guide/profiling.rst#id13

-- https://github.com/ghc/ghc/blob/1c2c2d3dfd4c36884b22163872feb87122b4528d/rts/ProfilerReport.c#L284

parseProfFile :: Parser Profile
parseProfFile = do
    timestamp <- skipSpaces *> parseDateTime
    title <- parseHeader *> skipSpaces *> parseTitle <* skipSpaces
    totalTime <- parseTotalTime <* skipSpaces
    totalAlloc <- parseTotalAlloc <* skipSpaces
    perCostCenterCosts <- parsePerCostCenterCosts
    pure $ 
        { timestamp: timestamp
        , title: title
        , totalTime: totalTime
        , totalAlloc: totalAlloc
        , perCostCenterCosts: perCostCenterCosts
        }

parsePerCostCenterCosts :: Parser (List PerCostCenterCosts)
parsePerCostCenterCosts = do
    widths <- skipSpaces *> parseCostCenterCostHeader
    _ <- skipSpaces
    ls <- many (parsePerCostCenterCostsLine widths <* eol)
    pure ls


parseCostCenterCostHeader :: Parser PerCostCenterCostsColumnWidths
parseCostCenterCostHeader = do
    cs <- spaceAndThing $ string "COST CENTRE" 
    m <- spaceAndThing $ string "MODULE"
    src <- spaceAndThing $ string "SRC"
    time <- spaceAndThing $ string "%time"
    alloc <- spaceAndThing $ string "%alloc"
    pure $ 
        { costCenter: (S.length cs)
        , mod: (S.length m)
        , src: (S.length src) 
        , time: (S.length time)
        , alloc: (S.length alloc)
        }
    where
    spaceAndThing p = do
        s <- blankSpace
        t <- p
        e <- blankSpace
        pure $ s <> t <> e

parsePerCostCenterCostsLine :: PerCostCenterCostsColumnWidths -> Parser PerCostCenterCosts
parsePerCostCenterCostsLine { costCenter: cs, mod: m, src: src, time: time, alloc: alloc } = do
    name <- takeN cs Just
    mod <- takeN m Just
    s <- takeN src Just
    t <- takeN time Num.fromString
    a <- takeN alloc Num.fromString
    pure $
        { name: name
        , mod: mod
        , src: s
        , time: t
        , alloc: a
        }

-- 	total alloc =     164,784 bytes  (excludes profiling overheads)
parseTotalAlloc :: Parser Int
parseTotalAlloc = 
    skipSpaces *> string "total alloc" *> 
    skipSpaces *> string "=" *> 
    skipSpaces *> parseIntegerWithCommas <* 
    skipSpaces <* string "bytes" <* 
    skipSpaces <* string "(excludes profiling overheads)" <* skipSpaces
    

-- COST CENTRE MODULE           SRC                         %time %alloc

-- MAIN        MAIN             <built-in>                    0.0   68.8

parseTotalTime :: Parser TotalTime
parseTotalTime = do
    time <- skipSpaces *> string "total time" *>
            skipSpaces *> string "=" *>
            skipSpaces *> parseFloat <*
            skipSpaces <* string "secs"
    { ticks: ticks, interval: interval, processors: processors } <- do
        ticks <- skipSpaces *> string "(" *> parseInteger <* skipSpaces <* string "ticks @" <* skipSpaces
        interval <- parseInteger <* skipSpaces <* string "us," <* skipSpaces
        processors <- parseInteger <* skipSpaces <* string "processors)" <* skipSpaces
        pure { ticks: ticks, interval: interval, processors: processors }
    pure { time: time, ticks: ticks, interval: interval, processors: processors }

parseTitle :: Parser String
parseTitle = do
    title <- skipSpaces *> manyTill anyChar eol
    pure $ fromCharArray $ Array.fromFoldable title

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
    ints <- getDigits'
    convertDigits ints 0 (length ints)
    where
    nDigs 0 = pure Nil
    nDigs n = do
        d <- anyDigit
        ds <- nDigs (n - 1)
        pure (d:ds)
    getDigits' = do
        first <- (try (nDigs 3) <|> try (nDigs 2) <|> try (nDigs 1)) -- Not sure if/why these trys are necessary
        rest <- many (char ',' *> nDigs 3)
        pure $ first <> (concat rest)

parseInteger :: Parser Int
parseInteger = do
    ds <- getDigits
    convertDigits ds 0 (length ds)

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

blankLine :: Parser Unit
blankLine = many separator *> eol *> pure unit

eol :: Parser String
eol = (string "\n" <|> string "\r\n")

separator :: Parser String
separator = (string " " <|> string "\t")

blankSpace :: Parser String
blankSpace = do
    s <- many separator
    pure (foldr (<>) "" s)

nonSpace :: Parser String
nonSpace = do
    c <- (satisfy \ c -> c /= '\n' && c /= '\r' && c /= ' ' && c /= '\t')
    pure $ singleton c

-- TODO: Better name? make less specialized?
takeN :: forall a. Int -> (String -> Maybe a) -> Parser a
takeN n p = go n ""
    where
    go 0 acc = case (p (S.trim  acc)) of
        Just v -> pure v
        Nothing -> fail "Error"
    go n' acc = do
      s <- (try nonSpace <|> try separator)
      go (n' - 1) (acc <> s)

isSpace :: String -> Boolean
isSpace s = s == "\n" || s == "\r" || s == " " || s == "\t"
