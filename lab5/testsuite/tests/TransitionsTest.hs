module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.List as List

import TestingUtils

import Transitions
import Grammar

initGrammar = uncurry cleanGrammarRules . shapeGrammarRules . Either.fromRight undefined

test_first = grammarTestFactory (first . initGrammar)

test_follow = grammarTestFactory (follow . initGrammar)

test_goto ss ts = grammarTestFactory (goto ss ts . initGrammar)

tests = 
    [ testGroup "First test"
        [ testCase "first clear" $
            test_first "testsuite/data/clear-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", Set.singleton (Term "a") )
                    , ( NTerm "A", Set.singleton (Term "a") )
                    , ( NTerm "B", Set.singleton (Term "b") )
                    , ( Term "a", Set.singleton (Term "a") )
                    , ( Term "b", Set.singleton (Term "b") )
                    , ( End, Set.singleton End ) ]
        , testCase "first dirty" $
            test_first "testsuite/data/dirty-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", Set.singleton (Term "a") )
                    , ( NTerm "A", Set.singleton (Term "d") )
                    , ( Term "a", Set.singleton (Term "a") )
                    , ( Term "d", Set.singleton (Term "d") )
                    , ( End, Set.singleton End ) ] ]
        , testCase "first tanya" $
            test_first "testsuite/data/tanya-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", Set.singleton (Term "c") )
                    , ( Term "c", Set.singleton (Term "c") )
                    , ( End, Set.singleton End ) ]
    , testGroup "Follow test"
        [ testCase "follow clear" $
            test_follow "testsuite/data/clear-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", Set.empty )
                    , ( NTerm "A", Set.singleton (Term "b") )
                    , ( NTerm "B", Set.singleton End )
                    , ( Term "a", Set.singleton (Term "b") )
                    , ( Term "b", Set.singleton End )
                    , ( End, Set.empty ) ]
        , testCase "follow dirty" $
            test_follow "testsuite/data/dirty-grammar.txt" $
                Map.fromList
                    [ (NTerm "S", Set.empty )
                    , (NTerm "A", Set.singleton End )
                    , (Term "a", Set.singleton (Term "d") )
                    , (Term "d", Set.singleton End )
                    , (End, Set.empty ) ] 
        , testCase "follow tanya" $
            test_follow "testsuite/data/tanya-grammar.txt" $
                Map.fromList
                    [ ( NTerm "S", Set.fromList [Term "c", End] )
                    , ( Term "c", Set.fromList [End, Term "c"] )
                    , ( End, Set.empty ) ] ]
    , testGroup "Goto test"
        [ testCase "goto clear" $
            test_goto
                (Set.fromList
                    [ Situation
                        { symbol = NTerm "S"
                        , beforeDot = []
                        , afterDot = [Term "a", Term "b", End]}
                    , Situation
                        { symbol = NTerm "S"
                        , beforeDot = []
                        , afterDot = [NTerm "A", NTerm "B", End] }])
                (NTerm "A")
                "testsuite/data/clear-grammar.txt"
                (Set.fromList
                    [ Situation
                        { symbol = NTerm "B"
                        , beforeDot = []
                        , afterDot = [Term "b"] }
                    , Situation
                        { symbol = NTerm "S"
                        , beforeDot = [NTerm "A"]
                        , afterDot = [NTerm "B", End] } ]) ] ]

main = defaultMain tests
