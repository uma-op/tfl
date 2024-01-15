module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)
import Grammar (parseGrammarRules, shapeGrammarRules, lhs, rhs, GrammarRule (..), Symbol (..), cleanGrammarRules)

parseFromFile filename =
    do
        filepath <- getDataFileName filename
        input <- readFile filepath
        return $ parseGrammarRules input

test_parsing filename (Right grs) =
    predicate @? "File should be successfully parsed"
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

test_shaping filename expected =
    predicate @? "Grammar should be shaped correctly"
    where
        predicate =
            assertionPredicate $
            do
                (Right parsed) <- parseFromFile filename
                let shaped = shapeGrammarRules parsed
                if shaped == expected then do
                    return True
                else do
                    print shaped
                    return False

test_cleaning filename expected = predicate @? ""
    where
        predicate = assertionPredicate $
            do
                (Right parsed) <- parseFromFile filename
                let shaped = shapeGrammarRules parsed
                let cleaned = cleanGrammarRules (head shaped) (tail shaped)
                if cleaned == expected then do
                    return True
                else do
                    putStrLn "\nCleaned grammar:\n"
                    print cleaned
                    return False

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
        ],
        testGroup "grammar shaping"
        [
            testCase 
            "Simple grammar" $
            test_shaping "testsuite/data/clear-grammar.txt"
                [
                    GrammarRule { lhs = NTerm "S" , rhs = [Term "a", Term "b"] },
                    GrammarRule { lhs = NTerm "S" , rhs = [NTerm "A", NTerm "B"] },
                    GrammarRule { lhs = NTerm "A" , rhs = [Term "a"] },
                    GrammarRule { lhs = NTerm "B" , rhs = [Term "b"] }
                ]
        ],
        testGroup "grammar cleaning"
        [
            testCase 
            "Clear grammar" $
            test_cleaning "testsuite/data/clear-grammar.txt"
                [
                    GrammarRule { lhs = NTerm "A" , rhs = [Term "a"] },
                    GrammarRule { lhs = NTerm "B" , rhs = [Term "b"] },
                    GrammarRule { lhs = NTerm "S" , rhs = [NTerm "A", NTerm "B"] },
                    GrammarRule { lhs = NTerm "S" , rhs = [Term "a", Term "b"] }
                ],
            testCase 
            "Dirty grammar" $
            test_cleaning "testsuite/data/dirty-grammar.txt"
                [
                    GrammarRule { lhs = NTerm "A" , rhs = [Term "d"] },
                    GrammarRule { lhs = NTerm "S" , rhs = [Term "a", NTerm "A"] }
                ]
        ]
    ]

main = defaultMain tests
