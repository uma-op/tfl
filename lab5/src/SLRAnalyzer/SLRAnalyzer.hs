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

initSLRAnalyzer table =
    SLRAnalyzer
        { transitions = table
        , stack =
            GraphStack.GraphStack
                { GraphStack.nodes = Map.singleton node []
                , GraphStack.degrees = Map.singleton node 0
                , GraphStack.lastBlocked = 0 } }
    where
        node =
            GraphStack.Node
                { GraphStack.state = 0
                , GraphStack.position = 0
                , GraphStack.block = 0 }

analyze ::
    String ->
    Int ->
    SLRAnalyzer ->
    Bool
analyze word iter analyzer
    | iter == 0 = undefined
    | Set.null $ GraphStack.tops $ stack analyzer = False
    | List.any
        ( List.any
            ((== Grammar.NTerm "") . snd)
        . ((GraphStack.nodes $ stack analyzer) Map.!))
        (GraphStack.tops $ stack analyzer) = True
    | otherwise =
        analyze word (iter - 1) (analyzer { stack = List.foldl resolve (stack analyzer) (GraphStack.tops (stack analyzer)) })
    where
        failCondition = not (Set.null $ GraphStack.tops $ stack analyzer)

        resolve ::
            GraphStack.GraphStack ->  -- input graph stack
            GraphStack.Node ->        -- top node
            GraphStack.GraphStack     -- output graph stack
        resolve gs topNode =
            GraphStack.unblockNode blockedTopNode $
            GraphStack.deleteTop blockedTopNode $
            resolve' actions blockedTopNode stackWithBlockedNode
            where
                blockedTopNode = topNode { GraphStack.block = GraphStack.lastBlocked stackWithBlockedNode }
                stackWithBlockedNode = GraphStack.blockNode topNode gs

                inputTerm = if List.null inputStack then Grammar.End else Grammar.Term [head inputStack] 
                    where
                        inputStack = List.drop (GraphStack.position topNode) word
                actions = Transitions.goByTerm inputTerm (GraphStack.state topNode) (transitions analyzer)

                resolve' ::
                    [Transitions.Action] ->   -- conflicting actions
                    GraphStack.Node ->        -- top node
                    GraphStack.GraphStack ->  -- input stack
                    GraphStack.GraphStack     -- output stack
                resolve' actions topNode gs =
                    resolve'' actions gs
                    where
                        shift = GraphStack.push

                        reduce ::
                            GraphStack.Node ->        -- top node
                            Grammar.GrammarRule ->    -- rule by which reducing
                            GraphStack.GraphStack ->  -- input stack
                            GraphStack.GraphStack     -- output stack
                        reduce topNode rule gs =
                            List.foldr
                                (\nt -> GraphStack.push (node nt) nterm nt)
                                gs newTops
                            where
                                node newTop =
                                    GraphStack.Node
                                        { GraphStack.state =
                                            if nterm == Grammar.NTerm "" then -1
                                            else Maybe.fromJust $ Transitions.goByNTerm nterm (GraphStack.state newTop) (transitions analyzer)
                                        , GraphStack.position = GraphStack.position topNode
                                        , GraphStack.block = 0 }
                                nterm = Grammar.lhs rule
                                newTops = newTops' (List.reverse $ Grammar.rhs rule) [blockedTopNode]
                                    where
                                        newTops' [] nodes = nodes
                                        newTops' (Grammar.End : otherSymbols) nodes = newTops' otherSymbols nodes
                                        newTops' symbols [] = []
                                        newTops' symbols nodes =
                                            newTops'
                                                (tail symbols)
                                                (nodes >>= ( List.map fst
                                                           . List.filter ((== head symbols) . snd)
                                                           . (GraphStack.nodes gs Map.!)))

                        resolve'' [] gs = gs
                        resolve'' (Transitions.Shift s : otherActions) gs =
                            resolve''
                                otherActions $
                                shift
                                    ( topNode
                                        { GraphStack.state = s
                                        , GraphStack.position = GraphStack.position topNode + 1
                                        , GraphStack.block = 0 } )
                                    inputTerm
                                    topNode gs
                        resolve'' (Transitions.Reduce r : otherActions) gs =
                            resolve''
                                otherActions $
                                reduce topNode r gs