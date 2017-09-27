module Transformations
    ( transformTransactions
    ) where

import Rules
import Transaction

import Data.List
import Data.Maybe
import Text.Regex.PCRE

-- TODO this could use some refactoring
patternMatches :: Transaction -> Pattern -> Bool
patternMatches (Transaction _ _ obj _ _ _) (Pattern (Left Description) Is args) =
    obj `elem` args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern (Left Comment) Is args) =
    case obj of
        Nothing -> False
        Just v -> v `elem` args
patternMatches (Transaction _ _ obj _ _ _) (Pattern (Left Description) Matches args) =
    any (obj =~) args
patternMatches (Transaction _ _ _ _ _ obj) (Pattern (Left Comment) Matches args) =
    case obj of
        Nothing -> False
        Just v -> any (v =~) args
patternMatches (Transaction _ _ _ obj _ _) (Pattern (Right (TwoWordObject Payer noun)) Matches args) =
    twoWordObjectMatches obj noun args
patternMatches (Transaction _ _ _ _ obj _) (Pattern (Right (TwoWordObject Payee noun)) Matches args) =
    twoWordObjectMatches obj noun args
patternMatches (Transaction _ _ _ obj _ _) (Pattern (Right (TwoWordObject Payer noun)) Is args) =
    twoWordObjectIs obj noun args
patternMatches (Transaction _ _ _ _ obj _) (Pattern (Right (TwoWordObject Payee noun)) Is args) =
    twoWordObjectIs obj noun args

twoWordObjectIs :: Posting -> Noun -> Arguments -> Bool
twoWordObjectIs (Posting acc _) Account args = acc `elem` args
twoWordObjectIs (Posting _ (Amount _ cur)) Currency args = cur `elem` args

twoWordObjectMatches :: Posting -> Noun -> Arguments -> Bool
twoWordObjectMatches (Posting acc _) Account args = any (acc =~) args
twoWordObjectMatches (Posting _ (Amount _ cur)) Currency args =
    any (cur =~) args

ruleMatches :: Transaction -> Rule -> Bool
ruleMatches t (Rule _ p _) = all (patternMatches t) p

executeAction :: Transaction -> Action -> Transaction
executeAction t (Action Set (Left Description) arg) = t {description = arg}
executeAction t (Action Set (Left Comment) arg) =
    t {transactionComment = Just arg}
executeAction t (Action Set (Right (TwoWordObject Payer Account)) arg) =
    t {payerPosting = Posting arg amount}
  where
    (Posting _ amount) = payerPosting t
executeAction t (Action Set (Right (TwoWordObject Payee Account)) arg) =
    t {payeePosting = Posting arg amount}
  where
    (Posting _ amount) = payeePosting t
executeAction t (Action Set (Right (TwoWordObject Payer Currency)) arg) =
    t {payerPosting = Posting acc amount}
  where
    (Posting acc (Amount n _)) = payerPosting t
    amount = Amount n arg
executeAction t (Action Set (Right (TwoWordObject Payee Currency)) arg) =
    t {payeePosting = Posting acc amount}
  where
    (Posting acc (Amount n _)) = payeePosting t
    amount = Amount n arg

findMatchingRule :: Rules -> Transaction -> Maybe Rule
findMatchingRule rs t = listToMaybe $ filter (ruleMatches t) rs

transformTransaction :: Rules -> Transaction -> Transaction
transformTransaction rs t = foldl executeAction t a
  where
    (Rule _ _ a) = fromMaybe (Rule "" [] []) $ findMatchingRule rs t

transformTransactions :: Rules -> Transactions -> Transactions
transformTransactions r ts = map (transformTransaction r) (sort ts)
