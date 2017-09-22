{-# LANGUAGE FlexibleContexts #-}
import Test.Tasty
import Test.Tasty.HUnit

import Data.Time.Calendar
import Text.Megaparsec hiding (parseMaybe)

import Argparse
import Rules

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests:" [unitTests]

unitTests = testGroup "Unit tests:" [periodTests, rulesTest]

rulesTest =
    testGroup
        "Rules tests:"
        [ nameTest
        , singleObjectTest
        , twoObjectTest_1
        , twoObjectTest_2
        , objectTest_1
        , objectTest_2
        , objectTest_3
        , patternVerbTest_1
        , patternVerbTest_2
        , actionVerbTest
        , patternLineTest
        , actionLineTest
        , fullRuleTest_1
        , fullRuleTest_2
        , multipleRulesTest
        ]

rulesTestCase desc parser input output =
    testCase desc $ parse parser "" input @?= output

nameTest = rulesTestCase "Name parsing test" parseName "[Name]" (Right "Name")

singleObjectTest =
    rulesTestCase "Single word object test"
    singleWordObject "description" (Right Description)

twoObjectTest_1 =
    rulesTestCase "Two word object test 1"
    twoWordObject "payer account" (Right $ TwoWordObject Payer Rules.Account)

twoObjectTest_2 =
    rulesTestCase "Two word object test 2"
    twoWordObject "payee currency" (Right $ TwoWordObject Payee Currency)

objectTest_1 =
    rulesTestCase "Object test 1"
    objects "payee account" (Right $ Right $ TwoWordObject Payee Account)

objectTest_2 =
    rulesTestCase "Object test 2"
    objects "description" (Right $ Left Description)

objectTest_3 =
    rulesTestCase "Object test 3"
    objects "payer account" (Right $ Right $ TwoWordObject Payer Account)

patternVerbTest_1 =
    rulesTestCase "Pattern verb test (matches)"
    patternVerbs "matches" (Right Matches)

patternVerbTest_2 =
    rulesTestCase "Pattern verb test (is)"
    patternVerbs "is" (Right Is)

actionVerbTest =
    rulesTestCase "Action verb test"
    actionVerbs "set" (Right Set)

patternLineTest =
    rulesTestCase "Pattern line test"
    parsePatternLine "description matches \"test\""
    (Right (Pattern (Left Description) Matches ["test"]))

actionLineTest =
    rulesTestCase "Pattern line test"
    parseActionLine "set description \"test\""
    (Right (Action Set (Left Description) "test"))

fullRuleTest_1 =
    rulesTestCase "Complete rule test 1"
    parseRule "[test rule]\ndescription matches \"test\"\nset description \"ok computer\""
    (Right (Rule "test rule"
        [Pattern (Left Description) Matches ["test"]]
        [Action Set (Left Description) "ok computer"]))

fullRuleTest_2 =
    rulesTestCase "Complete rule test 2"
    parseRule "[test rule]\ndescription is \"test\"\nset description \"ok computer\"\nset description \"2017-01-01\""
    (Right (Rule "test rule"
        [Pattern (Left Description) Is ["test"]]
        [ Action Set (Left Description) "ok computer"
        , Action Set (Left Description) "2017-01-01"]))

multipleRulesTest =
    rulesTestCase "Multiple rules test"
    parseRules "[test rule]\ndescription is \"test\"\nset description \
              \ \"ok computer\"\nset description \"2017-01-01\" \
              \ [second rule]\ndescription is \"2017-01-01\" \
              \ \n set payee account \"Some:Account\""
    (Right [ Rule "test rule"
                [Pattern (Left Description) Is ["test"]]
                [ Action Set (Left Description) "ok computer"
                , Action Set (Left Description) "2017-01-01"]
           , Rule "second rule"
                [Pattern (Left Description) Is ["2017-01-01"]]
                [Action Set (Right $ TwoWordObject Payee Account) "Some:Account"]
            ])

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
