module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

data MEquation =
    MEquation
        { mlhs :: Map.Map Int Int
        , mrhs :: Map.Map Int Int }

data NEquation =
    NEquation
        { nlhs :: Map.Map (Int, Int) Int
        , nrhs :: Map.Map (Int, Int) Int }

getAlphabet :: [(String, String)] -> Set.Set Char
getAlphabet = List.foldr (Set.union . Set.fromList . uncurry (++) ) Set.empty

buildMEquations :: [(String, String)] -> Map.Map Char MEquation
buildMEquations = undefined

buildNEquations :: [(String, String)] -> Map.Map Char NEquation
buildNEquations = undefined

main = return ()
