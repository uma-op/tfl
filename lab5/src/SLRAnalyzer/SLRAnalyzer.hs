module SLRAnalyzer where

import qualified Data.List as List

import qualified Grammar
import qualified GraphStack
import qualified Transitions

data SLRAnalyzer =
    SLRAnalyzer
        { transitions :: Transitions.Transitions
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
            [Transitions.Action] ->               -- conflicting actions
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
                                { GraphStack.state = Transitions.goByNTerm nterm (GraphStack.state topNode) (transitions analyzer)
                                , GraphStack.position = GraphStack.position topNode })
                            nterm)
                        gs
                        newTops
                    where
                        nterm = Grammar.lhs rule
                        newTops = undefined :: [GraphStack.Node]

                resolve' = undefined