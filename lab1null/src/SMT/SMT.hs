module SMT where

define v = "(declare-fun " ++ v ++ " () Int)"

m i = 'm' : show i
n i j = 'n' : show i ++ '_' : show j

defineM i = define (m i)
defineN i j = define (n i j)

assert v = "(assert " ++ v ++ ")"

binaryOp op v1 v2 = "(" ++ op ++  " " ++ v1 ++ " " ++ v2 ++ ")" 
parens v = "(" ++ v ++  ")"

assertGeZero v = assert $ binaryOp ">=" v "0"
assertGtZero v = assert $ binaryOp ">" v "0"
assertEqZero v = assert $ binaryOp "=" v "0"
assertEqOne v = assert $ binaryOp "=" v "1"

header = ["(set-logic QF_NIA)"]
footer = ["(check-sat)", "(get-model)"]
