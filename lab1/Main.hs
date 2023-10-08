module Main where

import Ordinal
    ( uniqCoefficients,
      composeLinearPolynomial,
      LinearPolynomial(..),
      SimpleFactor(SimpleFactor),
      unition )

import SmtPrint (
    smtLinearPolynomialGT,
    smtDefine,
    smtAssert, smtGEZero)

makePolynomial :: Char -> LinearPolynomial
makePolynomial name = LinearPolynomial [[[SimpleFactor name 2]], [[SimpleFactor name 3]]] [[[SimpleFactor name 0]], [[SimpleFactor name 1]]]

parseCompose :: String -> LinearPolynomial
parseCompose [] = LinearPolynomial [] []
parseCompose (h:t) = foldl composeLinearPolynomial (makePolynomial h) (map makePolynomial t)

data Rule = Rule !String !String

parseRule :: String -> Rule
parseRule rule = split rule " -> "
    where
        split text sep = split' [] text sep
        split' buf [] _ = Rule buf []
        split' buf (h:t) sep =
            case removed of
                (Just x) -> Rule buf x
                Nothing -> split' (buf ++ [h]) t sep 
            where
                removed = removeStart (h:t) sep
                removeStart x "" = Just x
                removeStart "" _ = Nothing
                removeStart (h1:t1) (h2:t2)
                    | h1 == h2 = removeStart t1 t2
                    | otherwise = Nothing


parseRules :: [String] -> [Rule]
parseRules = map parseRule

putStrLns :: [String] -> IO ()
putStrLns [] = return ()
putStrLns (h:t) =
    do
        putStrLn h
        putStrLns t

lab1 statements =
    do
        statement <- getLine
        case statement of
            "" -> do
                    putStrLn "(set-logic QF_NIA)"
                    let uniq = foldl unition [] . map (\ (Rule lhs rhs) -> uniqCoefficients [parseCompose lhs, parseCompose rhs]) . parseRules $ statements
                    putStrLns . map smtDefine $ uniq
                    putStrLns . map (smtAssert . smtGEZero) $ uniq
                    putStrLns . map (smtAssert . (\ (Rule lhs rhs) -> smtLinearPolynomialGT (parseCompose lhs) (parseCompose rhs))) . parseRules $ statements
                    putStrLn "(check-sat)"
                    putStrLn "(get-model)"
                    putStrLn "(exit)"
            _ -> lab1 (statement:statements)

main = lab1 []
        
