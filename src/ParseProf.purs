module ParseProf where
import Prelude

import Control.Alternative ((<|>))
import Data.Array as Array
import Data.Date.Component (Day, Month(..), Year)
import Data.DateTime (DateTime(..), Time(..), Weekday(..), canonicalDate)
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
import Prof (Tree(..), Forest, TotalTime, PerCostCenterCosts, CostCenterStackCosts, Profile)
import Text.Parsing.StringParser (Parser, fail, try)
import Text.Parsing.StringParser.CodePoints (anyChar, anyDigit, char, string, skipSpaces, satisfy)
import Text.Parsing.StringParser.Combinators (option, many1, manyTill, many)

type CostCenterCostsColumnWidths l =
    { name :: Int
    , mod :: Int
    , src :: Int
    , ticks :: Int
    , bytes :: Int
    | l
    }

type PerCostCenterCostsColumnWidths = CostCenterCostsColumnWidths 
    ( time :: Int
    , alloc :: Int
    )

type CostCenterStackColumnWidths = CostCenterCostsColumnWidths
    ( number :: Int
    , entries :: Int
    , individual :: { time :: Int , alloc :: Int }
    , inherited :: { time :: Int , alloc :: Int }
    )

type CostCenterStackCostsWithDepth = { depth :: Int, stack :: CostCenterStackCosts }

-- Prof file

-- https://github.com/ghc/ghc/blob/6aaa0655a721605740f23e49c5b4bf6165bfe865/docs/users_guide/profiling.rst#id13
-- https://github.com/ghc/ghc/blob/1c2c2d3dfd4c36884b22163872feb87122b4528d/rts/ProfilerReport.c#L284

parseProfFile :: Parser Profile
parseProfFile = do
    timestamp <- skipSpaces *> parseDateTime
    title <- parseHeader *> skipSpaces *> parseTitle <* skipSpaces
    totalTime <- parseTotalTime <* skipSpaces
    totalAlloc <- parseTotalAlloc <* skipSpaces
    perCostCenterCosts <- parsePerCostCenterCosts
    costCenterStackTree <- parseCostCenterStack
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
parseHeader = do
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
        processors <- parseInteger <* skipSpaces <* string "processors)" <* skipSpaces
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
parsePerCostCenterCosts :: Parser (List PerCostCenterCosts)
parsePerCostCenterCosts = do
    widths <- skipSpaces *> parseCostCenterCostHeader
    _ <- skipSpaces
    ls <- many (parsePerCostCenterCostsLine widths <* eol)
    pure ls

parseCostCenterCostHeader :: Parser PerCostCenterCostsColumnWidths
parseCostCenterCostHeader = do
    cs <- countWidth true $ string "COST CENTRE" 
    m <- countWidth true $ string "MODULE"
    src <- countWidth true $ string "SRC"
    time <- countWidth false $ string "%time"
    alloc <- countWidth false $ string "%alloc"
    { bytes: b, ticks: t } <- parseHeaderOptionals
    pure $ 
        { name: cs
        , mod: m
        , src: src
        , time: time
        , alloc: alloc
        , bytes: b
        , ticks: t
        }

parsePerCostCenterCostsLine :: PerCostCenterCostsColumnWidths -> Parser PerCostCenterCosts
parsePerCostCenterCostsLine { name: cs, mod: m, src: src, time: time, alloc: alloc, bytes: bytes, ticks: ticks } = do
    name <- takeN cs justString
    mod <- takeN m justString
    s <- takeN src justString
    t <- takeN time justNum
    { alloc: a, ticks: ti, bytes: by } <- parseEnd alloc ticks bytes
    pure $
        { name: name
        , mod: mod
        , src: s
        , time: t
        , alloc: a
        , ticks: ti
        , bytes: by
        }

-- Cost center stack costs

parseCostCenterStack :: Parser (Forest CostCenterStackCosts)
parseCostCenterStack = do
    _ <- skipSpaces *> string "individual" *> skipSpaces *> string "inherited" *> eol
    widths <- parseCostCenterStackHeader <* eol
    _ <- blankLine
    cs <- many (parseCostCenterStackLine widths <* eol)
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
parseCostCenterStackHeader :: Parser CostCenterStackColumnWidths
parseCostCenterStackHeader = do
    cs <- countWidth true $ string "COST CENTRE" 
    m <- countWidth true $ string "MODULE"
    src <- countWidth true $ string "SRC"
    no <- countWidth true $ string "no."
    entries <- countWidth false $ string "entries"
    individualTime <- countWidth false $ string "%time"
    individualAlloc <- countWidth false $ string "%alloc"
    inheritedTime <- countWidth false $ string "%time"
    inheritedAlloc <- countWidth false $ string "%alloc"
    { bytes: b, ticks: t } <- parseHeaderOptionals
    pure $ 
        { name: cs
        , mod: m
        , src: src
        , number: no
        , entries: entries
        , individual:
            { time: individualTime
            , alloc: individualAlloc
            }
        , inherited:
            { time: inheritedTime
            , alloc: inheritedAlloc
            }
        , bytes: b
        , ticks: t
        }

-- TODO: Rename
parseCostCenterStackLine :: CostCenterStackColumnWidths -> Parser CostCenterStackCostsWithDepth
parseCostCenterStackLine
    { name: cs
    , mod: m
    , src: src
    , number: num
    , entries: ent
    , individual: { time : individualTime , alloc: individualAlloc }
    , inherited : { time : inheritedTime , alloc : inheritedAlloc }
    , ticks: ticks
    , bytes: bytes }
    = do
        { name: name, depth: depth } <- takeN cs justDepthAndName
        mod <- takeN m justString
        s <- takeN src justString
        n <- takeN num justInt
        e <- takeN ent justInt
        indT <- takeN individualTime justNum
        indA <- takeN individualAlloc justNum
        inhT <- takeN inheritedTime justNum
        { alloc: inhA, ticks: t, bytes: b } <- parseEnd inheritedAlloc ticks bytes
        pure $
            { stack: 
                { name: name
                , mod: mod
                , src: s
                , number: n
                , entries: e
                , individual: { time : indT, alloc: indA }
                , inherited: { time : inhT, alloc: inhA }
                , ticks: t
                , bytes: b
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
    go :: Int -> String -> Parser a
    go 0 acc = case (p  acc) of
        Just v -> pure v
        Nothing -> fail "Error"
    go n' acc = do
      s <- option "" (nonSpace <|> separator)
      go (n' - 1) (acc <> s)

parseHeaderOptionals :: Parser { ticks :: Int, bytes :: Int }
parseHeaderOptionals = do
    t <- option 0 (countWidth false $ string "ticks")
    b <- option 0 (countWidth false $ string "bytes")
    pure { ticks: t, bytes: b }

parseEnd :: Int -> Int -> Int -> Parser ({ alloc :: Number, ticks :: Maybe Int, bytes :: Maybe Int })
parseEnd alloc ticks bytes = (try (parseDetailed alloc ticks bytes)) <|> (parseRegular alloc)

parseDetailed :: Int -> Int -> Int -> Parser ({ alloc :: Number, ticks :: Maybe Int, bytes :: Maybe Int })
parseDetailed alloc ticks bytes = do
    a <- takeN alloc justNum
    t <- takeN ticks justInt
    b <- takeN bytes justInt
    pure { alloc: a, ticks: Just t, bytes: Just b }

parseRegular :: Int -> Parser ({ alloc :: Number, ticks :: Maybe Int, bytes :: Maybe Int })
parseRegular alloc = do
    a <- takeN alloc justNum
    pure { alloc: a, ticks: Nothing, bytes: Nothing }

countWidth :: Boolean -> Parser String -> Parser Int
countWidth takeRight p = do
    s <- blankSpace
    t <- p
    e <- if takeRight then blankSpace else pure ""
    pure $ S.length (s <> t <> e)

justString :: String -> Maybe String
justString s = Just $ S.trim s

justNum :: String -> Maybe Number
justNum s = Num.fromString $ S.trim s

justInt :: String -> Maybe Int
justInt s = fromString $ S.trim s
