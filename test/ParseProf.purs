module Test.Main where

import Prelude

import Data.Date as Date
import Data.DateTime as DateTime
import Data.Either (Either(..), isRight)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Time as Time
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS ()
import Node.FS.Sync (readTextFile)
import ParseProf as P
import Partial.Unsafe (unsafePartial)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)
import Text.Parsing.StringParser (runParser)

dateTime :: DateTime.DateTime
dateTime =
    let d1 = unsafePartial fromJust $ Date.canonicalDate <$> toEnum 2019 <*> pure Date.March <*> toEnum 11
        t1 = unsafePartial $ fromJust $ Time.Time <$> toEnum 7 <*> toEnum 33 <*> toEnum 0 <*> toEnum 0
        dt1 = DateTime.DateTime d1 t1
    in dt1

test_parseDateTime = describe "parseDateTime" $ do
    it "does thing" $ do
        let actual = runParser P.parseDateTime "Mon Mar 11 07:33 2019"
        actual `shouldEqual` (Right dateTime)


test_parseInteger = describe "parseDateTime" $ do
    it "does thing" $ do
        let actual = runParser P.parseInteger "1234"
        actual `shouldEqual` (Right 1234)

test_parseIntegerWithCommas = describe "commas" $ do
    it "does thing" $ do
        let actual = runParser P.parseIntegerWithCommas "12,345"
        actual `shouldEqual` (Right 12345)

test_parseProfFile f = describe "parseDateTime" $ do
    it "does thing" $ do
        let actual = runParser P.parseProfFile f
        actual `shouldEqual` 
            (Right { timestamp : dateTime
                    , title : "hasktest exe +RTS -N -p -RTS"
                    , totalTime : { time : 0.0
                                  , ticks : 0
                                  , interval : 1000
                                  , processors : 6
                                  }
                    , totalAlloc : 164784
                    })

-- test_parseDateTime = 
main :: Effect Unit
main = do
    testFile <- readTextFile UTF8 "test/hasktest-exe.prof"
    run [consoleReporter] do
        test_parseInteger
        test_parseIntegerWithCommas
        test_parseDateTime
        test_parseProfFile testFile
