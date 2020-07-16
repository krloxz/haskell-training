module RpnCalculator (solveRpn) where

import Text.Read
import Control.Monad

-- | A mathematical expression in Reverse Polish Notation.
type RpnExpression = String

-- | Solves an RPN expression.
solveRpn :: RpnExpression -> Maybe Double
solveRpn expression = do
    [result] <- foldM processWord [] $ words expression
    return result

processWord :: [Double] -> String -> Maybe [Double]
processWord (x:y:ys) "+" = Just $ (y + x) : ys
processWord (x:y:ys) "-" = Just $ (y - x) : ys
processWord (x:y:ys) "*" = Just $ (y * x) : ys
processWord (x:y:ys) "/" = Just $ (y / x) : ys
processWord (x:y:ys) "^" = Just $ (y ** x) : ys
processWord (x:xs) "ln" = Just $ (log x) : xs
processWord xs "sum" = Just $ [sum xs]
processWord xs number = liftM (:xs) (readMaybe number)

-- >>> solveRpn "10 4 3 + 2 * -"
-- >>> solveRpn "1 2 * 4 +"
-- >>> solveRpn "1 2 * 4 + 5 *"
-- >>> solveRpn "1 2 * 4"
-- >>> solveRpn "1 8 wharglbllargh"
-- Just (-4.0)
-- Just 6.0
-- Just 30.0
-- Nothing
-- Nothing
--
