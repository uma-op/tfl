module TestingUtils where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )

import Paths_lab5

import Grammar

parseFromFile filename =
    do
        filepath <- getDataFileName filename
        input <- readFile filepath
        return $ parseGrammarRules input

testBuildAssert got expected = 
    do
        if got == expected then do
            return True
        else do
            putStrLn "===== TEST FAILED ====="
            putStrLn "Expected:"
            print expected
            putStrLn "Got:"
            print got
            return False

testEquality got expected = assertionPredicate (testBuildAssert got expected) @? ""

grammarTestFactory got filename expected = predicate @? ""
    where
        predicate = assertionPredicate $
            do
                parsed <- parseFromFile filename
                return (testBuildAssert (got parsed) expected)