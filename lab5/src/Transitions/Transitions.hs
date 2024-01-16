{-# LANGUAGE TupleSections #-}
module Transitions where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Grammar

data Action = Reduce Int | Shift Int

data Situation =
    Situation
        { symbol :: Grammar.Symbol
        , beforeDot :: [Grammar.Symbol]
        , afterDot :: [Grammar.Symbol] }

newtype Transitions = Transitions { table :: Map.Map Int (Map.Map Grammar.Symbol Int, Map.Map Grammar.Symbol Action) }

closure :: [Situation] -> [Situation] -> [Situation]
closure = undefined

goto :: Situation -> Grammar.Symbol -> Maybe Situation
goto  = undefined

first rules =
    first' $
    Map.fromList $
    rules >>= (\r -> (Grammar.lhs r, []) : List.map (\s -> if Grammar.isTerm s then (s, [s]) else (s, [])) (Grammar.rhs r) )
    where
        first' result =
            case List.foldr firstOne (Either.Left result) rules of
                Either.Left newResult -> newResult
                Either.Right newResult -> first' newResult
            where
                firstOne rule result =
                    if List.null newTerms then
                        result
                    else
                        Either.Right (Map.update (Just . (newTerms ++)) (Grammar.lhs rule) unpackedResult)
                    where
                        unpackedResult = Either.fromLeft (Either.fromRight undefined result) result
                        rhsFirst = head $ Grammar.rhs rule
                        newTerms = (unpackedResult Map.! rhsFirst) List.\\ (unpackedResult Map.! Grammar.lhs rule)

follow rules =
    follow' $
    Map.fromList $
    rules >>= (\r -> (Grammar.lhs r, []) : List.map (, []) (Grammar.rhs r) )
    where
        first' = first rules

        follow' result =
            case List.foldr followOne (Either.Left result) rules of
                Either.Left newResult -> newResult
                Either.Right newResult -> follow' newResult
            where
                followOne (Grammar.GrammarRule { Grammar.rhs = [] }) result = result
                followOne rule result =
                    if List.null newTerms then
                        result
                    else
                        followOne
                            (rule { Grammar.rhs = tail $ Grammar.rhs rule })
                            (Either.Right (Map.update (Just . (newTerms ++)) rhsFirst unpackedResult))
                    where
                        unpackedResult = Either.fromLeft (Either.fromRight undefined result) result
                        rhsFirst = head $ Grammar.rhs rule
                        newTerms = newTerms' List.\\ (unpackedResult Map.! rhsFirst)
                            where
                                newTerms' =
                                    case Grammar.rhs rule of
                                        [Grammar.End] -> []
                                        [_] -> unpackedResult Map.! Grammar.lhs rule
                                        (_ : s : _) -> first' Map.! s

goByNTerm ::
    Grammar.Symbol ->  -- nterm that we are following
    Int ->             -- current state
    Transitions ->     -- transitions table
    Int                -- go to action
goByNTerm symbol state (Transitions table) = fst (table Map.! state) Map.! symbol

goByTerm ::
    Grammar.Symbol ->  -- term that we are following
    Int ->             -- current state
    Transitions ->     -- transitions table
    Action               -- go to action
goByTerm symbol state (Transitions table) = snd (table Map.! state) Map.! symbol
