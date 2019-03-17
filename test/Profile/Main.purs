module Test.Main where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Foldable (sequence_)
import Data.Traversable (traverse)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile, readdir)
import Node.Path (FilePath)
import Profligate.Profile.ParseProfile as ParseProf
import Test.Profile.TestData (dateTime, simpleCostCenterStackCosts, verboseCostCenterStackCosts, simplePerCostCenterCosts, verbosePerCostCenterCosts, simplePerCostCenterCostsLine, verbosePerCostCenterCostsLine, simpleCostCenterStackCostsLine, verboseCostCenterStackCostsLine)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
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
        actual `shouldEqual` (Right { costCenterVerbose: false })
    it "parses verbose cost center headers" $ do
        let header = "COST CENTRE MODULE           SRC                         %time %alloc  ticks     bytes"
            actual = runParser ParseProf.parseCostCenterCostHeader header
        actual `shouldEqual` (Right { costCenterVerbose: true })

test_parseCostCenterStackHeader :: Spec Unit
test_parseCostCenterStackHeader = do
    it "parses simple cost center stack headers" $ do
        let header = "COST CENTRE  MODULE                SRC                        no.      entries  %time %alloc   %time %alloc"
            actual = runParser (ParseProf.parseCostCenterStackHeader { costCenterVerbose: false } ) header
        actual `shouldEqual` (Right unit)
    it "parses verbose cost center stack headers" $ do
        let header = "COST CENTRE  MODULE                SRC                        no.      entries  %time %alloc   %time %alloc  ticks     bytes"
            actual = runParser (ParseProf.parseCostCenterStackHeader { costCenterVerbose: true } ) header
        actual `shouldEqual` (Right unit)

test_parsePerCostCenterCostsLine :: Spec Unit
test_parsePerCostCenterCostsLine = do
    it "parses simple cost center cost line" $ do
        let line = "main        Main             app/Main.hs:(77,1)-(80,27)    0.0    6.2"
            actual = runParser (ParseProf.parsePerCostCenterCostsLine { costCenterVerbose: false }) line
        actual `shouldEqual` (Right simplePerCostCenterCostsLine)
    it "parses verbose cost center cost line" $ do
        let line = "main        Main             app/Main.hs:(77,1)-(80,27)    0.0    6.2      0     10272"
            actual = runParser (ParseProf.parsePerCostCenterCostsLine { costCenterVerbose: true }) line
        actual `shouldEqual` (Right verbosePerCostCenterCostsLine)

test_parseCostCenterStackLine :: Spec Unit
test_parseCostCenterStackLine = do
    it "parses simple cost center stack cost line" $ do
        let line = "     fib'.go Main                  app/Main.hs:(72,5)-(74,51) 2492         31    0.0    0.6     0.0    0.6"
            actual = runParser (ParseProf.parseCostCenterStackLine { costCenterVerbose: false }) line
        actual `shouldEqual` (Right { stack: simpleCostCenterStackCostsLine, depth: 5 })
    it "parses verbose cost center stack cost line" $ do
        let line = "     fib'.go Main                  app/Main.hs:(72,5)-(74,51) 2492         31    0.0    0.6     0.0    0.6      0       928"
            actual = runParser (ParseProf.parseCostCenterStackLine { costCenterVerbose: true }) line
        actual `shouldEqual` (Right { stack: verboseCostCenterStackCostsLine, depth: 5 })

test_parseProfFiles :: Array { filename :: String, contents :: String } -> Spec Unit
test_parseProfFiles files = describe "parseProfFile" $
    sequence_  $ map runTest files
    where
    runTest :: { filename :: String, contents :: String } -> Spec Unit
    runTest f = it ("parses prof file: " <> f.filename) $ do
        let actual = runParser ParseProf.parseProfFile f.contents
        isRight actual `shouldEqual` true

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

test_parseVerboseProfFile :: String -> Spec Unit
test_parseVerboseProfFile f = describe "parseverboseProfFile" $ do
    it "parses a verbose prof file" $ do
        let actual = runParser ParseProf.parseProfFile f
        actual `shouldEqual`
            (Right { timestamp : dateTime
                    , title : "hasktest exe +RTS foo +RTS -N -p -P -RTS"
                    , totalTime : { time : 0.0, ticks : 2, interval : 1000, processors : 6 }
                    , totalAlloc : 164528
                    , perCostCenterCosts: verbosePerCostCenterCosts
                    , costCenterStack: verboseCostCenterStackCosts
                    })

main :: Effect Unit
main = do
    testFilePaths <- readdir "test/data/"
    testFiles <- traverse getFile testFilePaths
    testFile <- readTextFile UTF8 "test/data/hasktest-exe.prof"
    testVerboseFile <- readTextFile UTF8 "test/data/hasktest-exe-verbose.prof"
    run [consoleReporter] do
        test_parseInteger
        test_parseIntegerWithCommas
        test_parseDateTime
        test_parseCostCenterCostHeader
        test_parseCostCenterStackHeader
        test_parsePerCostCenterCostsLine
        test_parseCostCenterStackLine
        test_parseProfFile testFile
        test_parseVerboseProfFile testVerboseFile
        test_parseProfFiles testFiles
    where
    getFile :: FilePath -> Effect { filename :: String, contents :: String }
    getFile p = do
        f <- readTextFile UTF8 $ "test/data/" <> p
        pure { filename: p, contents: f}
