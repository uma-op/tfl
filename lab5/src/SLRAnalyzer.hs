{-# LANGUAGE TupleSections #-}
module SLRAnalyzer where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Either as Either

import qualified Grammar
import qualified GraphStack

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


follow rules = undefined
    where
        first = first' $ Map.fromList $ List.map ((, []) . Grammar.lhs) rules
            where
                first' result =
                    case first'' of
                        Either.Left newResult -> newResult
                        Either.Right newResult -> first' newResult
                    where
                        first'' = List.foldr firstOne (Either.Left result) rules
                            where
                                firstOne rule result =
                                    if List.null newTerms
                                    then result
                                    else Either.Right (Map.update (Just . (newTerms ++)) (Grammar.lhs rule) unpackedResult)
                                    where
                                        unpackedResult = Either.fromLeft (Either.fromRight undefined result) result
                                        rhsFirst = head $ Grammar.rhs rule
                                        newTerms = newTerms' List.\\ (unpackedResult Map.! Grammar.lhs rule)
                                            where
                                                newTerms'
                                                    | Grammar.isTerm rhsFirst = [rhsFirst]
                                                    | otherwise = unpackedResult Map.! rhsFirst

        follow' result =
            case follow'' of
                Either.Left newResult -> newResult
                Either.Right newResult -> follow' newResult
            where
                follow'' = List.foldr followOne (Either.Left result) rules
                    where
                        followOne (Grammar.GrammarRule { Grammar.rhs = [] }) result = result
                        followOne rule result =
                            if List.null newTerms
                            then result
                            else
                                followOne
                                    (rule { Grammar.rhs = tail $ Grammar.rhs rule })
                                    (Either.Right (Map.update (Just . (newTerms ++)) (head $ Grammar.rhs rule) unpackedResult))
                            where
                                unpackedResult = Either.fromLeft (Either.fromRight undefined result) result
                                newTerms = newTerms' List.\\ (unpackedResult Map.! head (Grammar.rhs rule))
                                    where
                                        newTerms' =
                                            case Grammar.rhs rule of
                                                [_] -> unpackedResult Map.! Grammar.lhs rule
                                                (_ : s : _) -> first Map.! s

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

data SLRAnalyzer =
    SLRAnalyzer
        { transitions :: Transitions
        , stack :: GraphStack.GraphStack }

analyze ::
    String ->
    SLRAnalyzer ->
    Bool
analyze word analyzer
    | condition = undefined
    | otherwise = undefined
    where
        condition = undefined

        resolve ::
            [Action] ->               -- conflicting actions
            GraphStack.Node ->        -- top node
            GraphStack.GraphStack ->  -- input stack
            GraphStack.GraphStack     -- output stack
        resolve actions topNode gs = undefined
            where
                deletingNode = topNode

                shift = GraphStack.push

                reduce ::
                    GraphStack.Node ->        -- top node
                    Grammar.GrammarRule ->    -- rule by which reducing
                    GraphStack.GraphStack ->  -- input stack
                    GraphStack.GraphStack     -- output stack
                reduce topNode rule gs =
                    List.foldr
                        (GraphStack.push
                            (GraphStack.Node
                                { GraphStack.state = goByNTerm nterm (GraphStack.state topNode) (transitions analyzer)
                                , GraphStack.position = GraphStack.position topNode })
                            nterm)
                        gs
                        newTops
                    where
                        nterm = Grammar.lhs rule
                        newTops = undefined :: [GraphStack.Node]

                resolve' = undefined