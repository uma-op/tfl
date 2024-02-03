module Main where

import System.Environment (getArgs)

import qualified Data.List as List

import qualified TestingUtils as TU
import qualified SLRAnalyzer as SA
import qualified Transitions as T
import qualified Grammar as G

import Numeric

parseFromStdIn = undefined
main =
    do
        args <- getArgs

        (step, [input, word]) <-
            case args of
                [a1, a2, a3] -> return (read a1, [a2, a3])
                _ -> error "Wrong usage"

        parsed <- TU.parseFromFile input

        case parsed of
            Left _ -> error "Bad grammar"
            Right raw ->
                do
                    let result =
                            SA.analyze word step
                            $ SA.initSLRAnalyzer
                            $ T.buildTransitions
                            $ G.cleanGrammarRules
                            $ G.shapeGrammarRules raw

                    print result
                    return ()