module Test.Main where

import Prelude

import Data.Either (Either(..))
import Data.List (List, fromFoldable)
import Data.Maybe
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import ParseProf as ParseProf
import Partial.Unsafe (unsafePartial)
import Prof as Prof
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import TestData ( dateTime,
    simplePerCostCenterCostsColumnWidth, detailedPerCostCenterCostsColumnWidth,
    simpleCostCenterStackCostsColumnWidth, detailedCostCenterStackCostsColumnWidth,
    simpleCostCenterStackCosts, detailedCostCenterStackCosts,
    simplePerCostCenterCosts, detailedPerCostCenterCosts,
    simplePerCostCenterCostsLine, detailedPerCostCenterCostsLine,
    simpleCostCenterStackCostsLine, detailedCostCenterStackCostsLine
    )
import Text.Parsing.StringParser (runParser)

test_parseDateTime :: Spec Unit
test_parseDateTime = describe "parseDateTime" $ do
    it "parses datetime" $ do
        let actual = runParser ParseProf.parseDateTime "Mon Mar 11 07:33 2019"
        actual `shouldEqual` (Right dateTime)

test_parseInteger :: Spec Unit
test_parseInteger = describe "parseInteger" $ do
    it "parses integer" $ do
        let actual = runParser ParseProf.parseInteger "1234"
        actual `shouldEqual` (Right 1234)

test_parseIntegerWithCommas :: Spec Unit
test_parseIntegerWithCommas = describe "parseIntegerWithCommas" $ do
    it "parses integers with comma separator" $ do
        let actual = runParser ParseProf.parseIntegerWithCommas "12,345"
        actual `shouldEqual` (Right 12345)

test_parseCostCenterCostHeader :: Spec Unit
test_parseCostCenterCostHeader = do
    it "parses simple cost center headers" $ do
        let header = "COST CENTRE MODULE           SRC                         %time %alloc"
            actual = runParser ParseProf.parseCostCenterCostHeader header
        actual `shouldEqual` (Right simplePerCostCenterCostsColumnWidth)
    it "parses detailed cost center headers" $ do
        let header = "COST CENTRE MODULE           SRC                         %time %alloc  ticks     bytes"
            actual = runParser ParseProf.parseCostCenterCostHeader header
        actual `shouldEqual` (Right detailedPerCostCenterCostsColumnWidth)

test_parseCostCenterStackHeader :: Spec Unit
test_parseCostCenterStackHeader = do
    it "parses simple cost center stack headers" $ do
        let header = "COST CENTRE  MODULE                SRC                        no.      entries  %time %alloc   %time %alloc"
            actual = runParser ParseProf.parseCostCenterStackHeader header
        actual `shouldEqual` (Right simpleCostCenterStackCostsColumnWidth)
    it "parses detailed cost center stack headers" $ do
        let header = "COST CENTRE  MODULE                SRC                        no.      entries  %time %alloc   %time %alloc  ticks     bytes"
            actual = runParser ParseProf.parseCostCenterStackHeader header
        actual `shouldEqual` (Right detailedCostCenterStackCostsColumnWidth)

test_parsePerCostCenterCostsLine :: Spec Unit
test_parsePerCostCenterCostsLine = do
    it "parses simple cost center cost line" $ do
        let line = "main        Main             app/Main.hs:(77,1)-(80,27)    0.0    6.2"
            actual = runParser (ParseProf.parsePerCostCenterCostsLine simplePerCostCenterCostsColumnWidth) line
        actual `shouldEqual` (Right simplePerCostCenterCostsLine)
    it "parses detailed cost center cost line" $ do
        let line = "main        Main             app/Main.hs:(77,1)-(80,27)    0.0    6.2      0     10272"
            actual = runParser (ParseProf.parsePerCostCenterCostsLine detailedPerCostCenterCostsColumnWidth) line
        actual `shouldEqual` (Right detailedPerCostCenterCostsLine)

test_parseCostCenterStackLine :: Spec Unit
test_parseCostCenterStackLine = do
    it "parses simple cost center stack cost line" $ do
        let line = "     fib'.go Main                  app/Main.hs:(72,5)-(74,51) 2492         31    0.0    0.6     0.0    0.6"
            actual = runParser (ParseProf.parseCostCenterStackLine simpleCostCenterStackCostsColumnWidth) line
        actual `shouldEqual` (Right { stack: simpleCostCenterStackCostsLine, depth: 5 })
    it "parses detailed cost center stack cost line" $ do
        let line = "     fib'.go Main                  app/Main.hs:(72,5)-(74,51) 2492         31    0.0    0.6     0.0    0.6      0       928"
            actual = runParser (ParseProf.parseCostCenterStackLine detailedCostCenterStackCostsColumnWidth) line
        actual `shouldEqual` (Right { stack: detailedCostCenterStackCostsLine, depth: 5 })

test_parseProfFile :: String -> Spec Unit
test_parseProfFile f = describe "parseProfFile" $ do
    it "parses a prof file" $ do
        let actual = runParser ParseProf.parseProfFile f
        actual `shouldEqual` 
            (Right { timestamp : dateTime
                    , title : "hasktest exe +RTS -N -p -RTS"
                    , totalTime : { time : 0.0, ticks : 0, interval : 1000, processors : 6 }
                    , totalAlloc : 164784
                    , perCostCenterCosts: simplePerCostCenterCosts
                    , costCenterStack: simpleCostCenterStackCosts
                    })

test_parseDetailedProfFile :: String -> Spec Unit
test_parseDetailedProfFile f = describe "parseDetailedProfFile" $ do
    it "parses a detailed prof file" $ do
        let actual = runParser ParseProf.parseProfFile f
        actual `shouldEqual`
            (Right { timestamp : dateTime
                    , title : "hasktest exe +RTS foo +RTS -N -p -P -RTS"
                    , totalTime : { time : 0.0, ticks : 2, interval : 1000, processors : 6 }
                    , totalAlloc : 164528
                    , perCostCenterCosts: detailedPerCostCenterCosts
                    , costCenterStack: detailedCostCenterStackCosts
                    })

test_takeN :: Spec Unit
test_takeN = describe "takeN" $ do
    it "works" $ do
        let actual = runParser (ParseProf.takeN 6 ParseProf.justInt) "1234567890"
        actual `shouldEqual` (Right 123456)
    it "works" $ do
        let takeTwo = (ParseProf.takeN 6 ParseProf.justInt >>= (\m ->
            ParseProf.takeN 6 ParseProf.justInt >>= (\n -> pure $ m + n)))
        let actual = runParser takeTwo "12345678901"
        actual `shouldEqual` (Right 912468)

main :: Effect Unit
main = do
    testFile <- readTextFile UTF8 "test/hasktest-exe.prof"
    testDetailedFile <- readTextFile UTF8 "test/hasktest-exe-detailed.prof"
    run [consoleReporter] do
        -- test_takeN
        -- test_parseInteger
        test_parseIntegerWithCommas
        test_parseDateTime
        test_parseCostCenterCostHeader
        test_parseCostCenterStackHeader
        test_parsePerCostCenterCostsLine
        test_parseCostCenterStackLine
        test_parseProfFile testFile
        test_parseDetailedProfFile testDetailedFile