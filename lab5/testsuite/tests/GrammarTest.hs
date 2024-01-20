module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Either as Either
import qualified Data.Set as Set

import TestingUtils
import Grammar

test_parsing = grammarTestFactory id
test_shaping = grammarTestFactory (shapeGrammarRules . Either.fromRight undefined)
test_cleaning = grammarTestFactory (uncurry cleanGrammarRules . shapeGrammarRules . Either.fromRight undefined)

tests =
    [ testGroup "grammar parsing"
        [ testCase
            "Correct grammar" $
            test_parsing "testsuite/data/clear-grammar.txt" $
                Right
                    [ ("S", ["a", "b"])
                    , ("S", ["A", "B"])
                    , ("A", ["a"])
                    , ("B", ["b"]) ] ]
    , testGroup "grammar shaping"
        [ testCase 
            "Clear grammar" $
            test_shaping "testsuite/data/clear-grammar.txt"
                ( NTerm "S"
                , Set.fromList
                    [ GrammarRule { lhs = NTerm "S", rhs = [Term "a", Term "b", End]}
                    , GrammarRule { lhs = NTerm "S", rhs = [NTerm "A", NTerm "B", End] }
                    , GrammarRule { lhs = NTerm "A", rhs = [Term "a"] }
                    , GrammarRule { lhs = NTerm "B", rhs = [Term "b"] } ] ) ]
    , testGroup "grammar cleaning"
        [ testCase 
            "Clear grammar" $
            test_cleaning "testsuite/data/clear-grammar.txt"
                ( NTerm "S"
                , Set.fromList 
                    [ GrammarRule { lhs = NTerm "S", rhs = [Term "a", Term "b", End] }
                    , GrammarRule { lhs = NTerm "S", rhs = [NTerm "A", NTerm "B", End] }
                    , GrammarRule { lhs = NTerm "A", rhs = [Term "a"] }
                    , GrammarRule { lhs = NTerm "B", rhs = [Term "b"] } ] )
        , testCase 
            "Dirty grammar" $
            test_cleaning "testsuite/data/dirty-grammar.txt"
                ( NTerm "S"
                , Set.fromList
                    [ GrammarRule { lhs = NTerm "S" , rhs = [Term "a", NTerm "A", End] }
                    , GrammarRule { lhs = NTerm "A" , rhs = [Term "d"] } ] )
        ]
    ]

main = defaultMain tests
