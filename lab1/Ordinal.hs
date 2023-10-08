module Ordinal where

data SimpleFactor = SimpleFactor !Char !Int deriving (Show, Eq)
type ProductFactor = [SimpleFactor]
type SumFactor = [ProductFactor]

mulFactor :: SumFactor -> SumFactor -> SumFactor
mulFactor [] _ = []
mulFactor _ [] = []
mulFactor (xh:xt) y = mulFactor' xh y ++ mulFactor xt y
    where
        mulFactor' :: ProductFactor -> SumFactor -> SumFactor
        mulFactor' _ [] = []
        mulFactor' v (h:t) = (v ++ h) : mulFactor' v t

type Ordinal = [SumFactor] 
sumOrdinal :: Ordinal -> Ordinal -> Ordinal
sumOrdinal a [] = a
sumOrdinal [] b = b
sumOrdinal (a0:a) [b0] = (a0 ++ b0) : a
sumOrdinal (_:a) (b0:b) = b0 : sumOrdinal a b

mulOrdinal :: Ordinal -> Ordinal -> Ordinal
mulOrdinal [a0] [b0, b1] = [mulFactor a0 b0, b1]
mulOrdinal (a0:a) [b0, b1] = a0 : mulOrdinal a [b0, b1]
mulOrdinal _ _ = error "[mulOrdinal] bad ordinals"

data LinearPolynomial = LinearPolynomial !Ordinal !Ordinal deriving Show

composeLinearPolynomial :: LinearPolynomial -> LinearPolynomial -> LinearPolynomial
composeLinearPolynomial
    (LinearPolynomial major1 minor1)
    (LinearPolynomial major2 minor2) = 
        LinearPolynomial
            (mulOrdinal major1 major2)
            (sumOrdinal
                (mulOrdinal major1 minor2)
                minor1)

unition :: Eq a => [a] -> [a] -> [a]
unition lset [] = lset
unition lset (elem:t)
    | contains lset elem = unition lset t
    | otherwise = unition (elem:lset) t
    where
        contains :: Eq a => [a] -> a -> Bool
        contains [] elem = False
        contains (h:t) elem
            | h == elem = True
            | otherwise = contains t elem

uniqProductFactorCoefficients :: ProductFactor -> [SimpleFactor]
uniqProductFactorCoefficients = unition []

uniqSumFactorCoefficients :: SumFactor -> [SimpleFactor]
uniqSumFactorCoefficients = foldl unition [] . map uniqProductFactorCoefficients

uniqOrdinalCoefficients :: Ordinal -> [SimpleFactor]
uniqOrdinalCoefficients = foldl unition [] . map uniqSumFactorCoefficients

uniqPolynomialCoefficients :: LinearPolynomial -> [SimpleFactor] 
uniqPolynomialCoefficients (LinearPolynomial major minor) =
    unition
        (uniqOrdinalCoefficients major)
        (uniqOrdinalCoefficients minor)

uniqCoefficients :: [LinearPolynomial] -> [SimpleFactor]
uniqCoefficients = foldl unition [] . map uniqPolynomialCoefficients