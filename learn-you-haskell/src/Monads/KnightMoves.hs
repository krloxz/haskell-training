module KnightMoves where

import Control.Monad

type KnightPosition = (Int, Int)

knightMoves :: KnightPosition -> [KnightPosition]
knightMoves (column, row) = do
    (nextColumn, nextRow) <- [
        (column + 2, row + 1), (column + 2, row - 1), (column + 1, row + 2), (column + 1, row -2),
        (column - 2, row + 1), (column - 2, row - 1), (column - 1, row + 2), (column - 1, row -2)]
    guard $ onBoard nextColumn && onBoard nextRow
    return (nextColumn, nextRow)
    where
        onBoard x = x `elem` [1..8]

in3 :: KnightPosition -> [KnightPosition]
in3 start = pure start >>= knightMoves >>= knightMoves >>= knightMoves

canReach :: KnightPosition -> KnightPosition -> Bool
canReach start end = end `elem` in3 start

reach :: KnightPosition -> KnightPosition -> [[KnightPosition]]
reach start end = do
    m1 <- knightMoves start
    m2 <- knightMoves m1
    m3 <- knightMoves m2
    guard (m3 == end)
    return [m1, m2, m3]

-- >>> (6,2) `reach` (6,1)
-- [[(8,1),(7,3),(6,1)],[(7,4),(8,2),(6,1)],[(7,4),(5,3),(6,1)],[(4,1),(5,3),(6,1)],[(5,4),(7,3),(6,1)],[(5,4),(4,2),(6,1)]]
--
