import Test.Tasty
import Test.Tasty.HUnit

import Data.Time.Calendar

import Argparse

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests:" [unitTests]

unitTests = testGroup "Unit tests:" [periodTests]

periodTestCase s t = testCase s $ parseMaybe periodParser (fst t) @?= snd t

periodTests =
    testGroup
        "Period tests:"
        [ periodTestCase "Simple years period" simpleYear
        , periodTestCase "Full year period" fullYear
        , periodTestCase "Full period" fullPeriod
        ]

testPeriod = Just (fromGregorian 2017 1 1, fromGregorian 2018 1 1)

simpleYear = ("2017 to 2018", testPeriod)

fullYear = ("2017-01-01 to 2018", testPeriod)

fullPeriod = ("from 2017-01-01 to 2018-01-01", testPeriod)
