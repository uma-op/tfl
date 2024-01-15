module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit
    ( (@?), AssertionPredicable (assertionPredicate) )
import Grammar
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)

parseFromFile filename =
    do
        filepath <- getDataFileName filename
        input <- readFile filepath
        return $ parseGrammarRules input

test_parsing filename (Right grs) =
    predicate @? ""
    where
        predicate =
            assertionPredicate $
            do
                parsed <- parseFromFile filename
                case parsed of
                    (Left e) ->
                        do
                            putStrLn "\nGot an unexpected error\n"
                            print e
                            return False
                    (Right grs') -> return $ grs == grs'

tests =
    [
        testGroup "grammar parsing"
        [
            testCase
            "Correct grammar" $
            test_parsing "testsuite/data/clear-grammar.txt" $
            Right
                [
                    ("S", ["a", "b"]),
                    ("S", ["A", "B"]),
                    ("A", ["a"]),
                    ("B", ["b"])
                ]
        ]
    ]

main = defaultMain tests
