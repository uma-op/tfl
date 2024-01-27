module GraphStack where

import qualified Data.Map as Map
import qualified Data.Map.Internal.Debug as MapDebug
import qualified Data.Set as Set
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bifunctor

import qualified Grammar


data Node = Node
    { state :: Int
    , position :: Int
    , block :: Int }
    deriving Eq

instance Show Node where
    show n = show (state n, position n, block n)

instance Ord Node where
    compare = Function.on compare (\n -> (state n, position n, block n))

data GraphStack = GraphStack
    { nodes :: Map.Map Node [(Node, Grammar.Symbol)]
    , degrees :: Map.Map Node Int
    , lastBlocked :: Int }

instance Eq GraphStack where
    (==) x y = nodes x == nodes y && degrees x == degrees y

instance Show GraphStack where
    show gs = "GraphStack.degrees:\n"
        ++ showMap (degrees gs)
        ++ "\nGraphStack.nodes:\n"
        ++ showMap (nodes gs)
        where
            showMap m = List.foldl (\r (k, a) -> show k ++ " : " ++ show a ++ '\n' : r) "" (Map.toList m)

tops :: GraphStack -> Set.Set Node
tops = Map.foldlWithKey (\r k a -> if a == 0 then Set.insert k r else r) Set.empty . degrees

successors :: Node -> GraphStack ->  [(Node, Grammar.Symbol)]
successors n gs = nodes gs Map.! n

predecessors :: Node -> GraphStack -> [(Node, Grammar.Symbol)]
predecessors n gs =
    Set.toList $
    Map.foldlWithKey
        (\s k a ->
            List.foldl (\s t ->
                if fst t == n then Set.insert (k, snd t) s
                else s) s a) Set.empty (nodes gs)

replaceNode :: Node -> Node -> GraphStack -> GraphStack
replaceNode node newNode gs =
    if node == newNode then
        gs
    else
        gs
            { nodes =
                Map.map
                    (List.nub . List.map (\x -> if fst x == node then (newNode, snd x) else x)) $
                Map.delete node $
                Map.insert
                    newNode
                    (successors node gs) $
                nodes gs
            , degrees =
                Map.delete node $
                Map.insert newNode (List.length $ List.union predecessorsNew' predecessorsOld') $
                degrees gs }
    where
        predecessorsOld' = predecessors node gs
        predecessorsNew' = predecessors newNode gs


push ::
    Node ->           -- state of new node
    Grammar.Symbol -> -- symbol to new node
    Node ->           -- top node
    GraphStack ->     -- input stack
    GraphStack        -- output stack
push newNode symbol topNode gs =
    if not needToPush then
        gs
    else
        gs
            { nodes = newNodes
            , degrees = newDegrees }
    where
        nodeInStack = Map.member newNode $ nodes gs
        needToPush = not (nodeInStack && ((topNode, symbol) `List.elem` successors newNode gs))

        newNodes
            | nodeInStack = Map.update (Just . List.insert (topNode, symbol)) newNode $ nodes gs
            | otherwise = Map.insert newNode [(topNode, symbol)] $ nodes gs

        newDegrees
            | nodeInStack = updatedDegrees
            | otherwise = Map.insert newNode 0 updatedDegrees
            where
                updatedDegrees = Map.update (Just . (+ 1)) topNode $ degrees gs

deleteTop ::
    Node ->        -- top node of stack to be deleted
    GraphStack ->  -- input stack
    GraphStack     -- output stack
deleteTop t gs =
    gs
        { nodes = nodes deletedTop
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
                    gs
                        { nodes = newNodes
                        , degrees = newDegrees }

blockNode ::
    Node ->        -- node to block
    GraphStack ->  -- input stack
    GraphStack     -- output stack
blockNode n gs =
    (replaceNode n n { block = blocked } gs) {lastBlocked = blocked }
    where
        blocked = lastBlocked gs + 1
        blockedNode = n { block = blocked }

unblockNode ::
    Node ->
    GraphStack ->
    GraphStack
unblockNode n gs
    | n `Map.member` nodes gs =
        let
            unblockedNode = n { block = 0 }
            successorsBlocked' = Maybe.fromMaybe [] (Map.lookup n $ nodes gs)
            nodes' = Map.delete unblockedNode $ Map.update (Just . (++ successorsBlocked')) n $ nodes gs
        in
            replaceNode n unblockedNode gs { nodes = nodes' }

    | otherwise = gs
    