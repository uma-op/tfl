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

data Action = Reduce Grammar.GrammarRule | Shift Int deriving (Eq, Show)

data Situation =
    Situation
        { symbol :: Grammar.Symbol
        , beforeDot :: [Grammar.Symbol]
        , afterDot :: [Grammar.Symbol] }
    deriving Eq

instance Show Situation where
    show s =
        show (symbol s)
        ++ " ->" ++ (List.reverse (beforeDot s) >>= show)
        ++ " ." ++ (afterDot s >>= show)

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

isStartSituation (Situation { beforeDot = []}) = True
isStartSituation _ = False

startSituationFromGrammarRule :: Grammar.GrammarRule -> Situation
startSituationFromGrammarRule rule =
    Situation
        { symbol = Grammar.lhs rule
        , beforeDot = []
        , afterDot = Grammar.rhs rule }

isFinalSituation (Situation { afterDot = []}) = True
isFinalSituation (Situation { afterDot = [Grammar.End]}) = True
isFinalSituation _ = False

finalSituationFromGrammarRule :: Grammar.GrammarRule -> Situation
finalSituationFromGrammarRule rule =
    Situation
        { symbol = Grammar.lhs rule
        , beforeDot = List.reverse $ Grammar.rhs rule
        , afterDot = [] }

newtype Transitions =
    Transitions
        { table :: Map.Map Int ( Map.Map Grammar.Symbol (Maybe.Maybe Int)
                               , Map.Map Grammar.Symbol [Action] ) } deriving (Eq, Show)

closure :: Set.Set Situation -> Set.Set Grammar.GrammarRule -> Set.Set Situation
closure ss rs = closure' ss (Set.map startSituationFromGrammarRule rs) Set.empty
    where
        closure' situations rules result =
            if Set.null situations then
                result
            else
                closure' (Set.union (closure'' situation) otherSituations) (Set.delete situation rules) (Set.insert situation result)
            where
                (situation, otherSituations) = Set.deleteFindMin situations

                closure'' (Situation { afterDot = (Grammar.NTerm nt:_) }) =
                    Set.map
                        startSituationFromGrammarRule
                        (Set.filter ((== Grammar.NTerm nt) . Grammar.lhs) rs)
                closure'' _ = Set.empty

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

first :: Set.Set Grammar.GrammarRule -> Map.Map Grammar.Symbol (Set.Set Grammar.Symbol)
first rules =
    first' $
    Map.fromList $
    Set.fold
        ((++) . (\r -> (Grammar.lhs r, Set.empty) : List.map (\s -> if Grammar.isTerm s then (s, Set.singleton s) else (s, Set.empty)) (Grammar.rhs r)))
        [] rules
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

follow :: Set.Set Grammar.GrammarRule -> Map.Map Grammar.Symbol (Set.Set Grammar.Symbol)
follow rules =
    Map.insert
        (Grammar.NTerm "")
        (Set.singleton Grammar.End) $
            follow' $
                Map.fromList $
                    Set.fold
                        ((++) . (\r -> (Grammar.lhs r, Set.empty) : List.map (, Set.empty) (Grammar.rhs r)))
                        [] rules
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
                        followOne (rule { Grammar.rhs = tail $ Grammar.rhs rule }) result
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
    Maybe.Maybe Int                -- go to action
goByNTerm symbol state (Transitions table) = fst (table Map.! state) Map.! symbol

goByTerm ::
    Grammar.Symbol ->  -- term that we are following
    Int ->             -- current state
    Transitions ->     -- transitions table
    [Action]           -- go to actions
goByTerm symbol state (Transitions table) = snd (table Map.! state) Map.! symbol

buildTransitions :: Set.Set Grammar.GrammarRule -> Transitions
buildTransitions rules =
    buildTransitions'
        (Set.singleton 0)
        states
        (Transitions { table = Map.empty })
    where
        terms = Grammar.getTerms rules
        nterms = Set.delete (Grammar.NTerm "") $ Grammar.getNTerms rules
        allSymbols = Set.union terms nterms

        states = Set.singleton (0, closure (Set.map startSituationFromGrammarRule $ Set.filter ((== Grammar.NTerm "") . Grammar.lhs) rules) rules)

        follow' = follow rules

        buildTransitions' statesToBuild states buildedTransitions =
            if Set.null statesToBuild then
                buildedTransitions
            else
                buildTransitions' (Set.union newStatesToBuild otherStatesToBuild) newStates newBuildedTransitions
            where
                (stateToBuild, otherStatesToBuild) = Set.deleteFindMin statesToBuild
                (newStatesToBuild, newStates, newBuildedTransitions) = buildState stateToBuild states buildedTransitions

                buildState :: Int -> Set.Set (Int, Set.Set Situation) -> Transitions -> (Set.Set Int, Set.Set (Int, Set.Set Situation), Transitions)
                buildState stateToBuild states buildedTransitions =
                    ( Set.map fst onlyNewStates
                    , newStates
                    , buildedTransitions { table = Map.insert stateToBuild (gotos, actions) (table buildedTransitions) })

                    where
                        stateSituations = snd $ Maybe.fromJust $ List.find ((== stateToBuild) . fst) states
                        gotoTransitions = Map.fromList $ List.foldr ((:) . (\nterm -> (nterm, goto stateSituations nterm rules))) [] nterms

                        (finalSituations, intermediateSituation) = Set.partition isFinalSituation stateSituations

                        reduceTransitions :: Map.Map Grammar.Symbol [Either Situation (Set.Set Situation)]
                        reduceTransitions =
                            Map.fromList $ Set.fold (\t r -> (t, Set.fold (getReduce t) [] finalSituations) : r) [] terms
                            where
                                getReduce term situation result =
                                    if Set.member term (follow' Map.! symbol situation) then
                                        Left situation : result
                                    else
                                        result

                        shiftTransitions :: Map.Map Grammar.Symbol [Either Situation (Set.Set Situation)]
                        shiftTransitions = Map.fromList $ Set.foldr ((:) . shiftTransition) [] terms
                            where
                                shiftTransition t = (t, [Right goto' | not (Set.null goto')])
                                    where
                                        goto' = goto intermediateSituation t rules

                        actionTransitions = Map.unionWith (++) shiftTransitions reduceTransitions

                        getState :: Set.Set Situation -> Set.Set (Int, Set.Set Situation) -> (Int, Set.Set (Int, Set.Set Situation))
                        getState s ss =
                            case List.find ((== s) . snd) ss of
                                Just (x, _) -> (x, ss)
                                Nothing ->
                                    let newState = fst (Set.findMax ss) + 1
                                    in (newState, Set.insert (newState, s) ss)

                        getActions ::
                            Map.Map Grammar.Symbol [Either Situation (Set.Set Situation)] ->
                            Map.Map Grammar.Symbol [Action] ->
                            Set.Set (Int, Set.Set Situation) ->
                            (Map.Map Grammar.Symbol [Action], Set.Set (Int, Set.Set Situation))
                        getActions actionTransitions actions states =
                            if Map.null actionTransitions then
                                (actions, states)
                            else
                                let
                                    ((transitionSymbol, rawActions), ts) = Map.deleteFindMin actionTransitions
                                    (convertedActions, newStates) = List.foldl folding ([], states) rawActions
                                        where
                                            folding result (Left r) =
                                                Bifunctor.first
                                                    (Reduce
                                                            Grammar.GrammarRule
                                                                { Grammar.lhs = symbol r
                                                                , Grammar.rhs = List.reverse (beforeDot r) ++ afterDot r } :)
                                                    result
                                            folding result (Right s) = Bifunctor.first ((: fst result) . Shift) $ getState s (snd result)

                                in
                                    if List.null rawActions then
                                        getActions ts (Map.insert transitionSymbol [] actions) states
                                    else
                                        getActions ts (Map.insert transitionSymbol convertedActions actions) newStates

                        getGotos ::
                            Map.Map Grammar.Symbol (Set.Set Situation) ->
                            Map.Map Grammar.Symbol (Maybe.Maybe Int) ->
                            Set.Set (Int, Set.Set Situation) ->
                            (Map.Map Grammar.Symbol (Maybe.Maybe Int), Set.Set (Int, Set.Set Situation))
                        getGotos gotoTransitions gotos states =
                            if Map.null gotoTransitions then
                                (gotos, states)
                            else
                                let
                                    ((transitionSymbol, rawGoto), ts) = Map.deleteFindMin gotoTransitions
                                    (convertedGoto, newStates) = getState rawGoto states
                                in
                                    if Set.null rawGoto then
                                        getGotos ts (Map.insert transitionSymbol Nothing gotos) states
                                    else
                                        getGotos ts (Map.insert transitionSymbol (Maybe.Just convertedGoto) gotos) newStates


                        (gotos, (actions, newStates)) = Bifunctor.second (getActions actionTransitions Map.empty) (getGotos gotoTransitions Map.empty states)
                        onlyNewStates = newStates Set.\\ states