module SLRAnalyzer where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map

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
    | Set.null $ GraphStack.tops $ stack analyzer = False
    | List.any
        ( List.any
            ((== Grammar.NTerm "") . snd)
        . ((GraphStack.nodes $ stack analyzer) Map.!) )
        (GraphStack.tops $ stack analyzer) = True
    | otherwise = analyze word (analyzer { stack = List.foldl resolve (stack analyzer) (GraphStack.tops (stack analyzer)) })
    where
        failCondition = not (Set.null $ GraphStack.tops $ stack analyzer)

        -- resolve every top
        -- delete resolved top

        resolve ::
            GraphStack.GraphStack ->  -- input graph stack
            GraphStack.Node ->        -- top node
            GraphStack.GraphStack     -- output graph stack
        resolve gs topNode = GraphStack.deleteTop topNode (resolve' actions topNode gs)
            where
                inputTerm = Grammar.Term [word List.!! GraphStack.position topNode]
                actions = Transitions.goByTerm inputTerm (GraphStack.state topNode) (transitions analyzer)

                resolve' ::
                    [Transitions.Action] ->   -- conflicting actions
                    GraphStack.Node ->        -- top node
                    GraphStack.GraphStack ->  -- input stack
                    GraphStack.GraphStack     -- output stack
                resolve' actions topNode gs = resolve'' actions gs
                    where
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
                                        { GraphStack.state = Maybe.fromJust $ Transitions.goByNTerm nterm (GraphStack.state topNode) (transitions analyzer)
                                        , GraphStack.position = GraphStack.position topNode })
                                    nterm)
                                gs
                                newTops
                            where
                                nterm = Grammar.lhs rule
                                newTops = newTops' (Grammar.rhs rule) [topNode]
                                    where
                                        newTops' [] nodes = nodes
                                        newTops' symbols [] = []
                                        newTops' symbols nodes =
                                            newTops'
                                                (tail symbols)
                                                (nodes >>= ( List.map fst . List.filter ((== head symbols) . snd) . (GraphStack.nodes gs Map.!)))

                        resolve'' [] gs = gs
                        resolve'' (Transitions.Shift s : otherActions) gs =
                            resolve''
                                otherActions $
                                shift
                                    ( topNode { GraphStack.state = s } )
                                    inputTerm
                                    topNode gs
                        resolve'' (Transitions.Reduce r : otherActions) gs =
                            resolve''
                                otherActions $
                                reduce topNode r gs