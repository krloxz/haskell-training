module RpnCalculator (solveRpn) where

-- |
-- A mathematical expression in Reverse Polish Notation.
type RpnExpression = String

-- |
-- Solves an RPN expression.
solveRpn :: RpnExpression -> Float
solveRpn = head . foldl processWord [] . words
    where
        processWord (x:y:ys) "+" = (y + x) : ys
        processWord (x:y:ys) "-" = (y - x) : ys
        processWord (x:y:ys) "*" = (y * x) : ys
        processWord (x:y:ys) "/" = (y / x) : ys
        processWord (x:y:ys) "^" = (y ** x) : ys
        processWord (x:xs) "ln" = (log x) : xs
        processWord xs "sum" = [sum xs]
        processWord xs number = read number : xs

-- >>> solveRpn "10 4 3 + 2 * -"
-- -4.0
--
