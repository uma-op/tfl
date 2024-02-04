module SMT where

import qualified Data.List as List

define v = "(declare-fun " ++ v ++ " () Int)"

m i = 'm' : show i
n i j = 'n' : show i ++ '_' : show j

defineM i = define (m i)
defineN i j = define (n i j)

assert v = "(assert " ++ v ++ ")"

binaryOp op v1 v2 = "(" ++ op ++  " " ++ v1 ++ " " ++ v2 ++ ")" 

eq = binaryOp "="
ge = binaryOp ">="
gt = binaryOp ">"
lt = binaryOp "<"

production v 0 = ""
production v k = binaryOp "*" v (show k)

addition [] = ""
addition vs = parens $ List.concat $ "+" : List.map (' ':) vs

nPolynome = undefined
mPolynome = undefined

parens v = "(" ++ v ++  ")"

assertGeZero v = assert $ ge v "0"
assertGtZero v = assert $ gt v "0"
assertEqZero v = assert $ eq v "0"
assertEqOne v = assert $ eq v "1"

header = ["(set-logic QF_NIA)"]
footer = ["(check-sat)", "(get-model)"]
