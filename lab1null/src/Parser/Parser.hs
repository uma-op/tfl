module Parser where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

parseFromFile filename =
    do
        input <- readFile filename
        return $ parse parseInput "(unknown)" input

parseInput = endBy parseDomino (char '\n')
parseDomino =
    do
        char '('
        lhs <- many $ noneOf "(,)\n"
        char ','
        rhs <- many $ noneOf "(,)\n"
        char ')'
        return (lhs, rhs)

