module GraphStack where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe

import qualified Grammar

data Node = Node
    { state :: Int
    , position :: Int}
    deriving Eq

instance Ord Node where
    compare = Function.on compare (\n -> (state n, position n))

data GraphStack = GraphStack
    { tops :: Set.Set Node
    , nodes :: Map.Map Node [(Node, Grammar.Symbol)]
    , degrees :: Map.Map Node Int }

push ::
    Node ->           -- state of new node
    Grammar.Symbol -> -- symbol to new node
    Node ->           -- top node
    GraphStack ->     -- input stack
    GraphStack        -- output stack
push newNode symbol topNode gs =
    GraphStack
        { tops = newTops
        , nodes = newNodes
        , degrees = newDegrees }
    where
        nodeInStack = Map.member newNode $ nodes gs

        newTops
            | nodeInStack = nodesWithouTop
            | otherwise = Set.insert newNode nodesWithouTop
            where
                nodesWithouTop = Set.delete topNode $ tops gs

        newNodes
            | nodeInStack = Map.update (Just . List.insert (topNode, symbol)) newNode $ nodes gs
            | otherwise = Map.insert newNode [(topNode, symbol)] $ nodes gs

        newDegrees
            | nodeInStack = updatedDegrees
            | otherwise = Map.insert newNode 0 updatedDegrees
            where
                updatedDegrees = Map.update (Just . (+ 1)) topNode $ degrees gs

-- addTop ::
--     Node ->        -- node that will be new top
--     GraphStack ->  -- input stack
--     GraphStack     -- output stack
-- addTop topNode gs =
--     GraphStack
--         { tops = Set.insert topNode $ tops gs
--         , nodes = nodes gs
--         , degrees = degrees gs }

deleteTop ::
    Node ->        -- top node of stack to be deleted
    GraphStack ->  -- input stack
    GraphStack     -- output stack
deleteTop t gs = 
        GraphStack
            { tops = Set.delete t $ tops deletedTop
            , nodes = nodes deletedTop
            , degrees = degrees deletedTop}
    where
        deletedTop = deleteTop' [t] gs
        deleteTop' [] gs = gs
        deleteTop' ts gs
            | degrees gs Map.! head ts /= 0 = deleteTop' (tail ts) gs
            | otherwise =
                let
                    prevNodes = List.map fst (nodes gs Map.! head ts)
                    topsToDelete = List.filter ((== 1) . (degrees gs Map.!)) prevNodes
                    newNodes = Map.delete (head ts) $ nodes gs
                    newDegrees = List.foldr (Map.update (Just . flip (-) 1)) (Map.delete (head ts) (degrees gs)) prevNodes
                in
                    deleteTop' (topsToDelete ++ tail ts)
                    GraphStack
                        { tops = tops gs
                        , nodes = newNodes
                        , degrees = newDegrees }

-- pop ::
--     Node ->        -- top node
--     Maybe Node ->  -- previous node, needed if top is common for several stacks, else ignored
--     GraphStack ->  -- input stack
--     GraphStack     -- output stack
-- pop topNode prevNode gs
--     | degrees gs Map.! topNode /= 0 = error "GraphStack.pop: can't pop from stack whose top lies under another elements"
--     | otherwise =
--         GraphStack
--             { tops = newTops
--             , nodes = newNodes
--             , degrees = newDegrees }
--     where
--         prevNodes = List.map fst $ nodes gs Map.! topNode
--         (newTops, newNodes, newDegrees) =
--             case prevNodes of
--                 [] -> (Set.empty, Map.empty, Map.empty)  -- bottom of stack reached
--                 [p] ->
--                     ( Set.insert p $ Set.delete topNode $ tops gs
--                     , Map.delete topNode $ nodes gs
--                     , Map.update (Just . flip (-) 1) p $ Map.delete topNode $ degrees gs )
--                 _ ->
--                     let unpackedPrevNode =
--                             case prevNode of
--                                 Just n -> if n `List.elem` prevNodes then n else error "GraphStack.pop: no such previous node"
--                                 Nothing -> error "GraphStack.pop: ambiguous pop must be determined with previous vertex"
--                     in
--                         ( Set.insert unpackedPrevNode $ tops gs
--                         , Map.update (Just . List.deleteBy (Function.on (==) fst) (unpackedPrevNode, "")) topNode $ nodes gs
--                         , Map.update (Just . flip (-) 1) unpackedPrevNode $ degrees gs )
