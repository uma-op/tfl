{-# LANGUAGE StrictData #-}
 

module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Maybe

import qualified SMT
import qualified Parser as P
import System.Environment (getArgs)

data MEquation =
    MEquation
        { mlhs :: Map.Map Int Int
        , mrhs :: Map.Map Int Int }

data NEquation =
    NEquation
        { nlhs :: Map.Map (Int, Int) Int
        , nrhs :: Map.Map (Int, Int) Int }

getAlphabet :: [(String, String)] -> Set.Set Char
getAlphabet = Set.delete '_' . List.foldr (Set.union . Set.fromList . uncurry (++) ) Set.empty


buildEquations :: [(String, String)] -> ([MEquation], [NEquation])
buildEquations dominos = (Map.elems foldM, Map.elems foldN)
    where
        alphabet = getAlphabet dominos
        enumeratedEquations = Map.fromList (List.zip [0..] dominos)
        lastDomino = fst (Map.findMax enumeratedEquations)

        mes =
            Map.fromSet
                (const
                    MEquation
                        { mlhs = Map.fromList [(i, 0) | i <- [1..lastDomino]]
                        , mrhs = Map.fromList [(i, 0) | i <- [1..lastDomino]] })
                alphabet
        
        foldM = List.foldl' foldM' mes [1..lastDomino]
            where
                foldM' m d = List.foldl' foldRM' (List.foldl' foldLM' m l) r
                    where
                        (l, r) = enumeratedEquations Map.! d
                        foldLM' m l = Map.update (Just . (\eq -> eq { mlhs = Map.update (Just . (+ 1)) d (mlhs eq) })) l m
                        foldRM' m r = Map.update (Just . (\eq -> eq { mrhs = Map.update (Just . (+ 1)) d (mrhs eq) })) r m

        nes =
            Map.fromSet
                (const
                    NEquation
                        { nlhs = Map.fromList $ [((i, j), 0) | i <- [0..lastDomino], j <- [0..lastDomino]]
                        , nrhs = Map.fromList $ [((i, j), 0) | i <- [0..lastDomino], j <- [0..lastDomino]] })
                (Set.cartesianProduct alphabet alphabet)

        foldN = List.foldl' foldN' nes [(i, j) | i <- [0..lastDomino], j <- [0..lastDomino]]
            where
                foldN' m (d1, d2) = List.foldl' foldRN' (List.foldl' foldLN' m l) r
                    where
                        (l, r) =
                            Bifunctor.bimap
                                ((\x -> List.zip x (tail x)) . (fst (enumeratedEquations Map.! d1) ++) . List.singleton . head)
                                ((\x -> List.zip x (tail x)) . (snd (enumeratedEquations Map.! d1) ++) . List.singleton . head)
                                (enumeratedEquations Map.! d2)

                        foldLN' m l = Map.update (Just . (\eq -> eq { nlhs = Map.update (Just . (+ 1)) (d1, d2) (nlhs eq) })) l m
                        foldRN' m r = Map.update (Just . (\eq -> eq { nrhs = Map.update (Just . (+ 1)) (d1, d2) (nrhs eq) })) r m

makeSMT :: [MEquation] -> [NEquation] -> [String]
makeSMT ms ns =
    List.concat
        [ SMT.header
        , mDefinitions
        , mAssertions
        , mQuantityAssertion
        , mEquations
        , nDefinitions
        , nAssertions
        , nEquations
        , nExtraEquations
        , mnEquatinos
        , SMT.footer ]
    where
        mDefinitions = List.map SMT.defineM $ Map.keys $ mlhs $ head ms

        mAssertions = List.map (SMT.assertGeZero . SMT.m) $ Map.keys $ mlhs $ head ms

        mQuantityAssertion = [SMT.assert $ SMT.lt "0" $ SMT.addition $ List.map SMT.m $ Map.keys $ mlhs $ head ms]

        mEquations = List.map (SMT.assert . smtMEquation) ms
            where
                smtMEquation eq = SMT.eq (smtMPolynome $ Map.toList $ mlhs eq) (smtMPolynome $ Map.toList $ mrhs eq)
                smtMPolynome = SMT.addition . List.filter (not . List.null) . List.map (uncurry SMT.production . Bifunctor.first SMT.m)

        nDefinitions = List.map (uncurry SMT.defineN) $ Map.keys $ nlhs $ head ns

        nAssertions = List.map (SMT.assertGeZero . uncurry SMT.n) $ Map.keys $ nlhs $ head ns

        nEquations = List.map (SMT.assert . smtNEquation) ns
            where
                smtNEquation eq = SMT.eq (smtNPolynome $ Map.toList $ nlhs eq) (smtNPolynome $ Map.toList $ nrhs eq)
                smtNPolynome = SMT.addition . List.filter (not . List.null) . List.map (uncurry SMT.production . Bifunctor.first (uncurry SMT.n))

        nExtraEquations =
            [ SMT.assert $ SMT.eq "1" $ SMT.addition $ List.map (uncurry SMT.n) n0_
            , SMT.assert $ SMT.eq "1" $ SMT.addition $ List.map (uncurry SMT.n) n_0 ]
            where
                n0_ = List.filter ((== 0) . fst) $ Map.keys $ nlhs $ head ns
                n_0 = List.filter ((== 0) . snd) $ Map.keys $ nlhs $ head ns

        mnEquatinos = List.map SMT.assert (List.foldr ((:) . mEq) [] m_ ++ List.foldr ((:) . mEq') [] m_)
            where
                mEq m = SMT.eq (SMT.m m) (SMT.addition $ List.map (uncurry SMT.n) $ List.filter ((== m) . fst) n_)
                mEq' m = SMT.eq (SMT.m m) (SMT.addition $ List.map (uncurry SMT.n) $ List.filter ((== m) . snd) n_)
                m_ = Map.keys $ mlhs $ head ms
                n_ = Map.keys $ nlhs $ head ns

main =
    do
        args <- getArgs
        input <-
            case args of
                [a1] -> return a1
                _ -> error "Wrong usage"

        parsed <- P.parseFromFile input

        case parsed of
            Left e -> error ("Wrong file format: " ++ show e)
            Right p ->
                do
                    putStrLn $ unlines $ uncurry makeSMT $ buildEquations (("_", "_") : p)
                    return ()
        return ()
