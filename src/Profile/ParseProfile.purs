module Profligate.Profile.ParseProfile where

import Prelude hiding (between)

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Date.Component (Day, Month(..), Year)
import Data.DateTime (DateTime(..), Time(..), Weekday(..), canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Foldable (foldr)
import Data.Int (fromString, pow, toNumber)
import Data.List (List(..), (:), length, concat, reverse)
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe(..))
import Data.Number as Num
import Data.String as S
import Data.String.CodePoints (countPrefix, codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton)
import Profligate.Profile.Profile (Tree(..), Forest, TotalTime, PerCostCenterCosts, CostCenterStackCosts, Profile)
import Text.Parsing.StringParser (Parser(..), fail, try)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, char, string, skipSpaces, satisfy)
import Text.Parsing.StringParser.Combinators (between, option, many1, manyTill, many)

type CostCenterHeader  =
    { costCenterVerbose :: Boolean
    }

type CostCenterStackCostsWithDepth = { depth :: Int, stack :: CostCenterStackCosts }

-- This is a hack to clear the parse input, to prevent some egregious quadratic behavior
-- This is ultimately due to `Data.String.CodePoints.indexOf'`. The `CodeUnits` version doesn't have the
-- same performance characteristics, so another alternative would be to use the `CodeUnits` parsers.
clear :: Parser Unit
clear = Parser \{ str, pos } -> Right { result: unit, suffix: { str: S.drop pos str, pos: 0 } }

-- Prof file

-- https://github.com/ghc/ghc/blob/6aaa0655a721605740f23e49c5b4bf6165bfe865/docs/users_guide/profiling.rst#id13
-- https://github.com/ghc/ghc/blob/1c2c2d3dfd4c36884b22163872feb87122b4528d/rts/ProfilerReport.c#L284

parseProfFile :: Parser Profile
parseProfFile = do
    timestamp <- skipSpaces *> parseDateTime
    title <- parseHeader *> skipSpaces *> parseTitle <* skipSpaces
    totalTime <- parseTotalTime <* skipSpaces
    totalAlloc <- parseTotalAlloc <* skipSpaces
    csh <- skipSpaces *> parseCostCenterCostHeader
    perCostCenterCosts <- parsePerCostCenterCosts csh
    costCenterStackTree <- parseCostCenterStack csh
    pure $ 
        { timestamp: timestamp
        , title: title
        , totalTime: totalTime
        , totalAlloc: totalAlloc
        , perCostCenterCosts: perCostCenterCosts
        , costCenterStack: costCenterStackTree
        }

-- Timestamp

-- https://github.com/ghc/ghc/blob/21f0f56164f50844c2150c62f950983b2376f8b6/rts/RtsUtils.c#L160
-- https://en.cppreference.com/w/c/chrono/ctime

parseDateTime :: Parser DateTime
parseDateTime = do
    month <- parseWeekDay *> skipSpaces *> parseMonth
    day <- skipSpaces *> parseDay
    time <- skipSpaces *> parseTime
    year <- skipSpaces *> parseYear <* skipSpaces
    pure $ DateTime (canonicalDate year month day) time

-- Title

parseHeader :: Parser Unit
parseHeader =
    string "Time and Allocation Profiling Report" *>
    skipSpaces *> 
    string "(Final)" *>
    skipSpaces *>
    pure unit

parseTitle :: Parser String
parseTitle = do
    title <- skipSpaces *> manyTill anyChar eol
    pure $ fromCharArray $ Array.fromFoldable title

-- Total time

parseTotalTime :: Parser TotalTime
parseTotalTime = do
    time <- skipSpaces *> string "total time" *>
            skipSpaces *> string "=" *>
            skipSpaces *> parseFloat <*
            skipSpaces <* string "secs"
    { ticks: ticks, interval: interval, processors: processors } <- do
        ticks <- skipSpaces *> string "(" *> parseInteger <* skipSpaces <* string "ticks @" <* skipSpaces
        interval <- parseInteger <* skipSpaces <* string "us," <* skipSpaces
        processors <- parseInteger <* skipSpaces <* string "processor" <* option "" (string "s") <* string ")" <* skipSpaces
        pure { ticks: ticks, interval: interval, processors: processors }
    pure { time: time, ticks: ticks, interval: interval, processors: processors }

-- Total alloc

parseTotalAlloc :: Parser Int
parseTotalAlloc = 
    skipSpaces *> string "total alloc" *> 
    skipSpaces *> string "=" *> 
    skipSpaces *> parseIntegerWithCommas <* 
    skipSpaces <* string "bytes" <* 
    skipSpaces <* string "(excludes profiling overheads)" <* skipSpaces

-- Per cost center costs

-- TODO: Rename
parsePerCostCenterCosts :: CostCenterHeader -> Parser (List PerCostCenterCosts)
parsePerCostCenterCosts csh = do
    _ <- skipSpaces
    ls <- many ((parsePerCostCenterCostsLine csh) <* eol)
    pure ls

parseCostCenterCostHeader :: Parser CostCenterHeader
parseCostCenterCostHeader = do
    cs <- parseHeaderColumn "COST CENTRE" 
    m <- parseHeaderColumn "MODULE"
    src <- parseHeaderColumn "SRC"
    time <- parseHeaderColumn "%time"
    alloc <- parseHeaderColumn "%alloc"
    verbose <- parseHeaderOptionals
    pure { costCenterVerbose: verbose }

parsePerCostCenterCostsLine :: CostCenterHeader -> Parser PerCostCenterCosts
parsePerCostCenterCostsLine { costCenterVerbose: verbose } = do
    name <- parseRowColumn "name" justString
    mod <- parseRowColumn "module" justString
    src <- parseSrcColumn
    time <- parseRowColumn "time" justNum
    alloc <- parseRowColumn "alloc" justNum
    { ticks: ticks, bytes: bytes } <- parseEnd verbose
    pure $
        { name: name
        , mod: mod
        , src: src
        , time: time
        , alloc: alloc
        , ticks: ticks
        , bytes: bytes
        }

-- Cost center stack costs

parseCostCenterStack :: CostCenterHeader -> Parser (Forest CostCenterStackCosts)
parseCostCenterStack csh@{ costCenterVerbose: verbose } = do
    _ <- skipSpaces *> string "individual" *> skipSpaces *> string "inherited" *> eol
    _ <- (parseCostCenterStackHeader csh) <* eol
    _ <- blankLine
    cs <- many (parseCostCenterStackLine csh <* eol <* clear)
    pure $ reverseTree $ treeify $ reverse cs -- TODO: This is not optimal
    where
    treeify :: List CostCenterStackCostsWithDepth -> Forest CostCenterStackCosts
    treeify cs = foldr (\c t -> insert c t 0) Nil cs
    reverseTree :: Forest CostCenterStackCosts -> Forest CostCenterStackCosts
    reverseTree ts = map (\(Node { value: a, children: t}) -> Node { value: a, children: reverseTree t }) (reverse ts)
    insert :: CostCenterStackCostsWithDepth -> Forest CostCenterStackCosts -> Int -> Forest CostCenterStackCosts
    insert c Nil d = (Node { value: c.stack, children: Nil} : Nil)
    insert c (n@(Node { value: a, children: cs}):ts) d = 
        if d == c.depth
        then (Node { value: c.stack, children: Nil} : n : ts)
        else (Node { value: a, children: (insert c cs (d + 1)) } : ts)

-- TODO: Rename
parseCostCenterStackHeader :: CostCenterHeader -> Parser Unit
parseCostCenterStackHeader { costCenterVerbose: verbose } = do
    cs <- parseHeaderColumn "COST CENTRE" 
    m <- parseHeaderColumn "MODULE"
    src <- parseHeaderColumn "SRC"
    no <- parseHeaderColumn "no."
    entries <- parseHeaderColumn "entries"
    individualTime <- parseHeaderColumn "%time"
    individualAlloc <- parseHeaderColumn "%alloc"
    inheritedTime <- parseHeaderColumn "%time"
    inheritedAlloc <- parseHeaderColumn "%alloc"
    _ <- if verbose then parseVerbose else pure unit
    pure unit
    where
    parseVerbose = do
        _ <- parseHeaderColumn "ticks"
        _ <- parseHeaderColumn "bytes"
        pure unit

-- TODO: Rename
parseCostCenterStackLine :: CostCenterHeader -> Parser CostCenterStackCostsWithDepth
parseCostCenterStackLine { costCenterVerbose: verbose } = do
    { name: name, depth: depth } <- parseRowColumn "name" justDepthAndName
    mod <- parseRowColumn "module" justString
    src <- parseSrcColumn
    num <- parseRowColumn "number" justInt
    entries <- parseRowColumn "entries" justInt
    indT <- parseRowColumn "individual time" justNum
    indA <- parseRowColumn "individual alloc" justNum
    inhT <- parseRowColumn "inherited time" justNum
    inhA <- parseRowColumn "inherited alloc" justNum
    { ticks: ticks, bytes: bytes } <- parseEnd verbose
    pure $
        { stack: 
            { name: name
            , mod: mod
            , src: src
            , number: num
            , entries: entries
            , individual: { time : indT, alloc: indA }
            , inherited: { time : inhT, alloc: inhA }
            , ticks: ticks
            , bytes: bytes
            }
        , depth: depth
        }
    where
    justDepthAndName :: String -> Maybe ({ name :: String,  depth :: Int })
    justDepthAndName s =
        let depth = countPrefix (\c -> c == (codePointFromChar ' ')) s
        in pure { name: S.trim s, depth: depth }

-- Date/time helpers

parseTime :: Parser Time
parseTime = do
    hours <- parseInteger <* string ":"
    minutes <- parseInteger
    seconds <- option 0 $ string ":" *> parseInteger
    case (convertTime hours minutes seconds) of
        Just time -> pure time
        _ -> fail "Invalid time"
    where
    convertTime :: Int -> Int -> Int -> Maybe Time 
    convertTime hours minutes seconds = do
        hours' <- toEnum hours
        minutes' <- toEnum minutes
        seconds' <- toEnum seconds
        milliseconds <- toEnum 0
        pure $ Time hours' minutes' seconds' milliseconds

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

-- Integer/number helpers

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
    nDigs :: Int -> Parser (List Char)
    nDigs 0 = pure Nil
    nDigs n = do
        d <- anyDigit
        ds <- nDigs (n - 1)
        pure (d:ds)
    getDigits' :: Parser (List Char)
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

-- Parser/conversion helpers

blankLine :: Parser Unit
blankLine = many separator *> eol *> pure unit

eol :: Parser String
eol = (string "\n" <|> string "\r" <|> string "\r\n")

separator :: Parser String
separator = do
    c <- (satisfy \ c -> c == ' ' || c == '\t')
    pure $ singleton c

blankSpace :: Parser String
blankSpace = do
    s <- many separator
    pure (foldr (<>) "" s)

blankSpace1 :: Parser String
blankSpace1 = do
    s <- many1 separator
    pure (foldr (<>) "" s)

nonSpace :: Parser String
nonSpace = do
    c <- (satisfy \ c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r')
    pure $ singleton c

nonEolChar :: Parser Char
nonEolChar = satisfy \c -> c /= '\n' && c /= '\r'

word :: Parser String
word = do
    s <- many1 nonSpace
    pure (foldr (<>) "" s)

parseHeaderOptionals :: Parser Boolean
parseHeaderOptionals = do
    t <- option false (parseHeaderColumn "ticks")
    b <- option false (parseHeaderColumn "bytes")
    pure $ t && b

parseEnd :: Boolean -> Parser ({ ticks :: Maybe Int, bytes :: Maybe Int })
parseEnd verbose = 
    if verbose
    then parseDetailed
    else pure { ticks: Nothing, bytes: Nothing }

parseDetailed ::  Parser ({ ticks :: Maybe Int, bytes :: Maybe Int })
parseDetailed = do
    t <- parseRowColumn "ticks" justInt
    b <- parseRowColumn "bytes" justInt
    pure { ticks: Just t, bytes: Just b }

parseHeaderColumn :: String -> Parser Boolean
parseHeaderColumn p = do
    s <- blankSpace
    t <- string p
    e <- blankSpace
    pure true

parseRowColumn :: forall a. String -> (String -> Maybe a) -> Parser a
parseRowColumn err f = do
    prespace <- blankSpace
    val <- word 
    space <- blankSpace
    case (f (prespace <> val <> space)) of
        Just v -> pure v
        Nothing -> fail ("Parsing failed on `" <> err <> "` " <> val)

parseSrcColumn :: Parser String
parseSrcColumn = (bracketed <|> parseRowColumn "source" justString)
    where
    bracketed :: Parser String
    bracketed = do
        prespace <- blankSpace
        src <- between (char '<') (char '>') (many bracketedChar)
        space <- blankSpace
        pure $ "<" <> (foldr (<>) "" src) <> ">"
    bracketedChar = do
        c <- satisfy \c -> c /= '\n' && c /= '\r' && c /= '>'
        pure $ singleton c

justString :: String -> Maybe String
justString s = Just $ S.trim s

justNum :: String -> Maybe Number
justNum s = Num.fromString $ S.trim s

justInt :: String -> Maybe Int
justInt s = fromString $ S.trim s
