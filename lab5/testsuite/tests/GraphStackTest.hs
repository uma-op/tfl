module Main where

import Test.Framework ( defaultMain, testGroup )
import Test.HUnit ( (@?), AssertionPredicable (assertionPredicate) )
import Paths_lab5
import Test.Framework.Providers.HUnit (testCase)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import TestingUtils
import GraphStack
import Grammar

unblockedNode =
    Node
        { state = 0
        , position = 0
        , block = 0 }

graphStack = GraphStack
    { nodes =
        Map.fromList
            [ (unblockedNode { state = 0, position = 0 }, [])
            , (unblockedNode { state = 1, position = 1 }, [(unblockedNode { state = 0, position = 0 }, End)])
            , (unblockedNode { state = 2, position = 1 }, [(unblockedNode { state = 0, position = 0 }, End)])
            , (unblockedNode { state = 3, position = 2 }, [(unblockedNode { state = 1, position = 1 }, End), (unblockedNode { state = 2, position = 1 }, End)])
            , (unblockedNode { state = 4, position = 1 }, [(unblockedNode { state = 0, position = 0 }, End)]) ]
    , degrees =
        Map.fromList
            [ (unblockedNode { state = 0, position = 0 }, 3)
            , (unblockedNode { state = 1, position = 1 }, 1)
            , (unblockedNode { state = 2, position = 1 }, 1)
            , (unblockedNode { state = 3, position = 2 }, 0)
            , (unblockedNode { state = 4, position = 1 }, 0) ]
    , lastBlocked = 0 }

graphStackShift5 =
    graphStack
        { nodes =
            Map.insert
                (unblockedNode { state = 5, position = 2 })
                [(unblockedNode { state = 4, position = 1 }, End)] $
                nodes graphStack
        , degrees =
            Map.insert
                (unblockedNode { state = 5, position = 2 }) 0 $
            Map.insert
                (unblockedNode { state = 4, position = 1 }) 1 $
                degrees graphStack }

graphStackShiftAnother5 =
    graphStackShift5
        { nodes = 
            Map.update
                (Just . List.insert (unblockedNode { state = 2, position = 1 }, End))
                (unblockedNode { state = 5, position = 2 })
                (nodes graphStackShift5)
        , degrees =
            Map.update
                (Just . (+1))
                (unblockedNode { state = 2, position = 1 })
                (degrees graphStackShift5) }

graphStackDelete3 =
    GraphStack
        { nodes =
            Map.fromList
                [ (unblockedNode { state = 0, position = 0 }, [])
                , (unblockedNode { state = 4, position = 1 }, [(unblockedNode { state = 0, position = 0 }, End)])]
        , degrees = 
            Map.fromList
                [ (unblockedNode { state = 0, position = 0 }, 1)
                , (unblockedNode { state = 4, position = 1 }, 0) ]
        , lastBlocked = 0 }

graphStackShiftAnother5Delete3 =
    GraphStack
        { nodes =
            Map.fromList
                [ (unblockedNode { state = 0, position = 0}, [])
                , (unblockedNode { state = 2, position = 1}, [(unblockedNode { state = 0, position = 0 }, End)])
                , (unblockedNode { state = 4, position = 1}, [(unblockedNode { state = 0, position = 0 }, End)])
                , (unblockedNode { state = 5, position = 2}, [(unblockedNode { state = 2, position = 1 }, End), (unblockedNode { state = 4, position = 1 }, End)]) ]
        , degrees =
            Map.fromList
                [ (unblockedNode { state = 0, position = 0}, 2)
                , (unblockedNode { state = 2, position = 1}, 1)
                , (unblockedNode { state = 4, position = 1}, 1)
                , (unblockedNode { state = 5, position = 2}, 0) ]
        , lastBlocked = 0 }


tests = 
    [ testGroup "Shift"
        [ testCase "shift new state" $
            testEquality
                (push
                    (unblockedNode { state = 5, position = 2 })
                    End (unblockedNode { state = 4, position = 1 })
                    graphStack)
                graphStackShift5
        , testCase "shift existing state" $
            testEquality
                (push
                    (unblockedNode { state = 5, position = 2 })
                    End (unblockedNode { state = 2, position = 1 })
                    graphStackShift5)
                graphStackShiftAnother5 ]
    , testGroup "Delete top"
        [ testCase "delete top" $
            testEquality
                (deleteTop
                    ( unblockedNode { state = 3, position = 2 } )
                    graphStack)
                graphStackDelete3
        , testCase "delete another top" $
            testEquality
                (deleteTop
                    ( unblockedNode { state = 3, position = 2 } )
                    graphStackShiftAnother5)
                graphStackShiftAnother5Delete3 ] ]

main = defaultMain tests
