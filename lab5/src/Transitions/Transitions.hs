{-# LANGUAGE TupleSections #-}
module Transitions where

import qualified Data.Either as Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Function as Function
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Maybe

import qualified Grammar

data Action = Reduce Int | Shift Int

data Situation =
    Situation
        { symbol :: Grammar.Symbol
        , beforeDot :: [Grammar.Symbol]
        , afterDot :: [Grammar.Symbol] }
    deriving (Eq, Show)

instance Ord Situation where
    compare x y =
        compare
            (symbol x, beforeDot x, afterDot x)
            (symbol y, beforeDot y, afterDot y)

nextSituation :: Situation -> Maybe Situation
nextSituation (Situation { afterDot = [] }) = Nothing
nextSituation s = Just $ s
    { beforeDot = head (afterDot s) : beforeDot s
    , afterDot = tail (afterDot s) }

situationsFromGrammarRule :: Grammar.GrammarRule -> Set.Set Situation
situationsFromGrammarRule rule =
    Set.fromList $
    List.zipWith
        (\a b -> Situation
            { symbol = Grammar.lhs rule
            , beforeDot = List.reverse a
            , afterDot = b } )
        (List.inits $ Grammar.rhs rule)
        (List.tails $ Grammar.rhs rule)

startSituationFromGrammarRule :: Grammar.GrammarRule -> Situation
startSituationFromGrammarRule rule =
    Situation
        { symbol = Grammar.lhs rule
        , beforeDot = []
        , afterDot = Grammar.rhs rule }

finalSituationFromGrammarRule :: Grammar.GrammarRule -> Situation
finalSituationFromGrammarRule rule =
    Situation
        { symbol = Grammar.lhs rule
        , beforeDot = List.reverse $ Grammar.rhs rule
        , afterDot = [] }

newtype Transitions = Transitions { table :: Map.Map Int (Map.Map Grammar.Symbol Int, Map.Map Grammar.Symbol Action) }

closure :: Set.Set Situation -> Set.Set Grammar.GrammarRule -> Set.Set Situation
closure ss rs =
    Set.fold
        (Set.union . closure')
        ss ss
    where
        closure' (Situation { afterDot = (Grammar.NTerm nt:_) }) =
            Set.map
                startSituationFromGrammarRule
                (Set.filter ((== Grammar.NTerm nt) . Grammar.lhs) rs)
        closure' s = Set.empty

goto ::
    Set.Set Situation ->            -- set of 'from' situations
    Grammar.Symbol ->               -- transition symbol
    Set.Set Grammar.GrammarRule ->  -- rules
    Set.Set Situation               -- set of 'to' situations
goto ss ts =
    closure
        (Set.map
            (Maybe.fromJust . nextSituation)
            (Set.filter filterTransitable ss))
    where
        filterTransitable (Situation { afterDot = (ts':_) }) = ts' == ts
        filterTransitable _ = False

first rules =
    first' $
    Map.fromList $
    rules >>= (\r -> (Grammar.lhs r, Set.empty) : List.map (\s -> if Grammar.isTerm s then (s, Set.singleton s) else (s, Set.empty)) (Grammar.rhs r) )
    where
        first' result =
            case List.foldr firstOne (Either.Left result) rules of
                Either.Left newResult -> newResult
                Either.Right newResult -> first' newResult
            where
                firstOne rule result =
                    if Set.null newTerms then
                        result
                    else
                        Either.Right (Map.update (Just . Set.union newTerms) (Grammar.lhs rule) unpackedResult)
                    where
                        unpackedResult = Either.fromLeft (Either.fromRight undefined result) result
                        rhsFirst = head $ Grammar.rhs rule
                        newTerms = (unpackedResult Map.! rhsFirst) Set.\\ (unpackedResult Map.! Grammar.lhs rule)

follow rules =
    follow' $
    Map.fromList $
    rules >>= (\r -> (Grammar.lhs r, Set.empty) : List.map (, Set.empty) (Grammar.rhs r) )
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
                            (Either.Right (Map.update (Just . Set.union newTerms) rhsFirst unpackedResult))
                    where
                        unpackedResult = Either.fromLeft (Either.fromRight undefined result) result
                        rhsFirst = head $ Grammar.rhs rule
                        newTerms = newTerms' Set.\\ (unpackedResult Map.! rhsFirst)
                            where
                                newTerms' =
                                    case Grammar.rhs rule of
                                        [Grammar.End] -> Set.empty
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
