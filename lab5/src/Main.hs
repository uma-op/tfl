module Main where

import SLRAnalyzer
import Grammar

readFromStream readed =
    do
        line <- getLine
        case line of
            "\n" -> return readed
            _ -> do readFromStream (readed ++ line)

main =
    do
        readed <- readFromStream ""
        let rules = parseGrammarRules readed
        case rules of
            Left e -> undefined
            Right r -> undefined
        return ()