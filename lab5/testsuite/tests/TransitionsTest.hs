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

initGrammar = cleanGrammarRules . shapeGrammarRules . Either.fromRight undefined

test_first = grammarTestFactory (first . initGrammar)
test_follow = grammarTestFactory (follow . initGrammar)
test_goto ss ts = grammarTestFactory (goto ss ts . initGrammar)
test_build = grammarTestFactory (buildTransitions . initGrammar)

tests = 
    [ testGroup "First test"
        [ testCase "first clear" $
            test_first "testsuite/data/clear-grammar.txt" $
                Map.fromList
                    [ ( NTerm "", Set.singleton (Term "a") )
                    , ( NTerm "S", Set.singleton (Term "a") )
                    , ( NTerm "A", Set.singleton (Term "a") )
                    , ( NTerm "B", Set.singleton (Term "b") )
                    , ( Term "a", Set.singleton (Term "a") )
                    , ( Term "b", Set.singleton (Term "b") )
                    , ( End, Set.singleton End ) ]
        , testCase "first dirty" $
            test_first "testsuite/data/dirty-grammar.txt" $
                Map.fromList
                    [ ( NTerm "", Set.singleton (Term "a") )
                    , ( NTerm "S", Set.singleton (Term "a") )
                    , ( NTerm "A", Set.singleton (Term "d") )
                    , ( Term "a", Set.singleton (Term "a") )
                    , ( Term "d", Set.singleton (Term "d") )
                    , ( End, Set.singleton End ) ] ]
        , testCase "first tanya" $
            test_first "testsuite/data/tanya-grammar.txt" $
                Map.fromList
                    [ ( NTerm "", Set.singleton (Term "c") )
                    , ( NTerm "S", Set.singleton (Term "c") )
                    , ( Term "c", Set.singleton (Term "c") )
                    , ( End, Set.singleton End ) ]
    , testGroup "Follow test"
        [ testCase "follow clear" $
            test_follow "testsuite/data/clear-grammar.txt" $
                Map.fromList
                    [ ( NTerm "", Set.singleton End )
                    , ( NTerm "S", Set.singleton End )
                    , ( NTerm "A", Set.singleton (Term "b") )
                    , ( NTerm "B", Set.singleton End )
                    , ( Term "a", Set.singleton (Term "b") )
                    , ( Term "b", Set.singleton End )
                    , ( End, Set.empty ) ]
        , testCase "follow dirty" $
            test_follow "testsuite/data/dirty-grammar.txt" $
                Map.fromList
                    [ ( NTerm "", Set.singleton End )
                    , ( NTerm "S", Set.singleton End )
                    , ( NTerm "A", Set.singleton End )
                    , ( Term "a", Set.singleton (Term "d") )
                    , ( Term "d", Set.singleton End )
                    , ( End, Set.empty ) ] 
        , testCase "follow tanya" $
            test_follow "testsuite/data/tanya-grammar.txt" $
                Map.fromList
                    [ ( NTerm "", Set.singleton End )
                    , ( NTerm "S", Set.fromList [Term "c", End] )
                    , ( Term "c", Set.fromList [End, Term "c"] )
                    , ( End, Set.empty ) ] ]
    , testGroup "Goto test"
        [ testCase "goto clear" $
            test_goto
                (Set.fromList
                    [ Situation
                        { symbol = NTerm "S"
                        , beforeDot = []
                        , afterDot = [Term "a", Term "b"]}
                    , Situation
                        { symbol = NTerm "S"
                        , beforeDot = []
                        , afterDot = [NTerm "A", NTerm "B"] }])
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
                        , afterDot = [NTerm "B"] } ]) ]
    , testGroup "Build test"
        [ testCase "build clear" $
            test_build "testsuite/data/clear-grammar.txt" $
                Transitions
                    { table =
                        Map.fromList
                            [ (0, ( Map.fromList
                                        [ (NTerm "A", Just 1)
                                        , (NTerm "B", Nothing)
                                        , (NTerm "S", Just 2) ]
                                  , Map.fromList
                                        [ (End, [])
                                        , (Term "a", [Shift 3])
                                        , (Term "b", []) ]) )
                            , (1, ( Map.fromList
                                        [ (NTerm "A", Nothing)
                                        , (NTerm "B", Just 4)
                                        , (NTerm "S", Nothing) ]
                                  , Map.fromList
                                        [ (End, [])
                                        , (Term "a", [])
                                        , (Term "b", [Shift 5]) ]))
                            , (2, ( Map.fromList
                                        [ (NTerm "A", Nothing)
                                        , (NTerm "B", Nothing)
                                        , (NTerm "S", Nothing) ]
                                  , Map.fromList
                                        [ (End, [Reduce $ GrammarRule { lhs = NTerm "", rhs = [NTerm "S", End] }])
                                        , (Term "a", [])
                                        , (Term "b", []) ] ))

                            , (3, ( Map.fromList
                                        [ (NTerm "S", Nothing)
                                        , (NTerm "A", Nothing)
                                        , (NTerm "B", Nothing) ]
                                  , Map.fromList
                                        [ (Term "a", [])
                                        , (Term "b", [Reduce $ GrammarRule { lhs = NTerm "A", rhs = [Term "a"] }, Shift 6])
                                        , (End, []) ]) )
                            , (4, ( Map.fromList
                                        [ (NTerm "S", Nothing)
                                        , (NTerm "A", Nothing)
                                        , (NTerm "B", Nothing) ]
                                  , Map.fromList
                                        [ (Term "a", [])
                                        , (Term "b", [])
                                        , (End, [Reduce $ GrammarRule { lhs = NTerm "S", rhs = [NTerm "A", NTerm "B"] }]) ] ))
                            , (5, ( Map.fromList
                                        [ (NTerm "S", Nothing)
                                        , (NTerm "A", Nothing)
                                        , (NTerm "B", Nothing) ]
                                  , Map.fromList
                                        [ (Term "a", [])
                                        , (Term "b", [])
                                        , (End, [Reduce $ GrammarRule { lhs = NTerm "B", rhs = [Term "b"] }]) ] ))
                            , (6, ( Map.fromList
                                        [ (NTerm "S", Nothing)
                                        , (NTerm "A", Nothing)
                                        , (NTerm "B", Nothing) ]
                                  , Map.fromList
                                        [ (Term "a", [])
                                        , (Term "b", [])
                                        , (End, [Reduce $ GrammarRule { lhs = NTerm "S", rhs = [Term "a", Term "b"] }]) ] )) ] }
        , testCase "build dirty" $
            test_build "testsuite/data/dirty-grammar.txt" $
                Transitions
                    { table =
                        Map.fromList
                            [ (0, ( Map.fromList
                                        [ (NTerm "A", Nothing)
                                        , (NTerm "S", Just 1) ]
                                  , Map.fromList
                                        [ (End, [])
                                        , (Term "a", [Shift 2])
                                        , (Term "d", []) ] ))
                            , (1, ( Map.fromList
                                        [ (NTerm "A", Nothing)
                                        , (NTerm "S", Nothing) ]
                                  , Map.fromList
                                        [ (End, [Reduce $ GrammarRule { lhs = NTerm "", rhs = [NTerm "S", End] }])
                                        , (Term "a", [])
                                        , (Term "d", []) ] ))
                            , (2, ( Map.fromList
                                        [ (NTerm "A", Just 3)
                                        , (NTerm "S", Nothing) ]
                                  , Map.fromList
                                        [ (End, [])
                                        , (Term "a", [])
                                        , (Term "d", [Shift 4]) ] ))
                            , (3, ( Map.fromList
                                        [ (NTerm "A", Nothing)
                                        , (NTerm "S", Nothing) ]
                                  , Map.fromList
                                        [ (End, [Reduce $ GrammarRule { lhs = NTerm "S", rhs = [Term "a", NTerm "A"] }])
                                        , (Term "a", [])
                                        , (Term "d", []) ] ))
                            , (4, ( Map.fromList
                                        [ (NTerm "A", Nothing)
                                        , (NTerm "S", Nothing) ]
                                  , Map.fromList
                                        [ (End, [Reduce $ GrammarRule { lhs = NTerm "A", rhs = [Term "d"] }])
                                        , (Term "a", [])
                                        , (Term "d", []) ] )) ] }] ]

main = defaultMain tests
