module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Either as Either
import TestingUtils

import Grammar
import SLRAnalyzer
import Transitions

test_analyze word =
    grammarTestFactory
        ( analyze word (-1)
        . initSLRAnalyzer
        . buildTransitions
        . cleanGrammarRules
        . shapeGrammarRules
        . Either.fromRight undefined)

tests =
    [ testGroup "Analyzing test" 
        [ testCase
            "Clear grammar good resolve" $
            test_analyze "ab" "testsuite/data/clear-grammar.txt" True
        , testCase
            "Clear grammar bad resolve" $
            test_analyze "ba" "testsuite/data/clear-grammar.txt" False 
        , testCase
            "Tanya grammar good resolve" $
            test_analyze "cccccc" "testsuite/data/tanya-grammar.txt" True
        , testCase 
            "Another Tanya grammar good resolve" $
            test_analyze "cccc" "testsuite/data/another-tanya-grammar.txt" True
        , testCase
            "New grammar good resolve" $
            test_analyze "aabaaababa" "testsuite/data/new-grammar.txt" True ] ]

main = defaultMain tests
