module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Map as Map

import Transitions
import Grammar

parseFromFile filename =
    do
        filepath <- getDataFileName filename
        input <- readFile filepath
        return $ parseGrammarRules input

test_first filename expected = predicate @? ""
    where
        predicate = assertionPredicate $
            do
                (Right parsed) <- parseFromFile filename
                let shaped = shapeGrammarRules parsed 
                let cleaned = cleanGrammarRules (head shaped) (tail shaped)
                let firsted = first cleaned
                if firsted == expected then do
                    return True
                else do
                    putStrLn "First failed"
                    print firsted
                    return False


test_follow filename expected = predicate @? ""
    where
        predicate = assertionPredicate $
            do
                (Right parsed) <- parseFromFile filename
                let shaped = shapeGrammarRules parsed 
                let cleaned = cleanGrammarRules (head shaped) (tail shaped)
                let followed = follow cleaned
                if followed == expected then do
                    return True
                else do
                    putStrLn "Follow failed"
                    print followed
                    return False

tests = 
    [ testGroup "First test"
        [ testCase "first clear" $
            test_first "testsuite/data/clear-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", [Term "a"] )
                    , ( NTerm "A", [Term "a"] )
                    , ( NTerm "B", [Term "b"] )
                    , ( Term "a", [Term "a"] )
                    , ( Term "b", [Term "b"] )
                    , ( End, [End] ) ]
        , testCase "first dirty" $
            test_first "testsuite/data/dirty-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", [Term "a"] )
                    , ( NTerm "A", [Term "d"] )
                    , ( Term "a", [Term "a"] )
                    , ( Term "d", [Term "d"] )
                    , ( End, [End] ) ] ]
        , testCase "first tanya" $
            test_first "testsuite/data/tanya-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", [Term "c"] )
                    , ( Term "c", [Term "c"] )
                    , ( End, [End] ) ]
    , testGroup "Follow test"
        [ testCase "follow clear" $
            test_follow "testsuite/data/clear-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", [] )
                    , ( NTerm "A", [Term "b"] )
                    , ( NTerm "B", [End] )
                    , ( Term "a", [Term "b"] )
                    , ( Term "b", [End] )
                    , ( End, [] ) ]
        , testCase "follow dirty" $
            test_follow "testsuite/data/dirty-grammar.txt" $
                Map.fromList
                    [ (NTerm "S", [] )
                    , (NTerm "A", [End] )
                    , (Term "a", [Term "d"] )
                    , (Term "d", [End] )
                    , (End, []) ] 
        , testCase "follow tanya" $
            test_follow "testsuite/data/tanya-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", [Term "c", End] )
                    , ( Term "c", [End, Term "c"] )
                    , ( End, [] ) ] ] ]

main = defaultMain tests
