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

grammarTestFactory got filename expected = predicate @? ""
    where
        predicate = assertionPredicate $
            do
                parsed <- parseFromFile filename
                let got' = got parsed
                if got' == expected then do
                    return True
                else do
                    putStrLn "Test failed"
                    putStrLn "Expected: "
                    print expected
                    putStrLn "Got: "
                    print got'
                    return False