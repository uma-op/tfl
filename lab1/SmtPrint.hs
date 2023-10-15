module SmtPrint where

import Ordinal

smtSimple :: SimpleFactor -> String
smtSimple (SimpleFactor name index) = name : show index

smtProd :: ProductFactor -> String
smtProd [] = ""
smtProd [h] = smtSimple h
smtProd (h:t) = "(* " ++ smtSimple h ++ " " ++ smtProd t ++ ")"

smtSum :: SumFactor -> String
smtSum [] = ""
smtSum [h] = smtProd h
smtSum (h:t) = "(+ " ++ smtProd h ++ " " ++ smtSum t ++ ")"

smtNEZero :: SumFactor -> String
smtNEZero x = "(not (= " ++ smtSum x ++ " 0))"

smtEQZero :: SumFactor -> String
smtEQZero x = "(= " ++ smtSum x ++ " 0)"

smtGEZero :: SimpleFactor -> String
smtGEZero x = "(>= " ++ smtSimple x ++ " 0)"

smtGT :: SumFactor -> SumFactor -> String
smtGT x y = "(> " ++ smtSum x ++ " " ++ smtSum y ++ ")"

smtOrdinalGT :: Ordinal -> Ordinal -> String
smtOrdinalGT [] [] = "false"
smtOrdinalGT x [] = "(or" ++ concatMap ((" " ++) . smtNEZero) x  ++ ")"
smtOrdinalGT [] x = "false"
smtOrdinalGT [xh] [yh] = smtGT xh yh
smtOrdinalGT (xh:xt) (yh:yt) = "(or (and " ++ smtGT xh yh ++ " " ++ "(not " ++ smtOrdinalGT yt xt ++ ")) " ++ smtOrdinalGT xt yt ++ ")"

smtLinearPolynomialGT :: LinearPolynomial -> LinearPolynomial -> String
smtLinearPolynomialGT (LinearPolynomial mj1 mn1) (LinearPolynomial mj2 mn2) =
    "(or " ++
    "(and (not " ++ smtOrdinalGT mn2 mn1 ++ ") " ++ smtOrdinalGT mj1 mj2 ++ ") " ++
    "(and (not " ++ smtOrdinalGT mj2 mj1 ++ ") " ++ smtOrdinalGT mn1 mn2 ++ "))"


smtDefine :: SimpleFactor -> String
smtDefine f = "(declare-fun " ++ smtSimple f ++ " () Int)"

smtAssert :: String -> String
smtAssert x = "(assert " ++ x ++ " )"
