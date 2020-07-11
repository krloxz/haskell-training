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

-- Implementation using bind (less readable)
knightMoves' :: KnightPosition -> [KnightPosition]
knightMoves' (c, r) =
    pure (c, r)
    >>= \(c,r) -> [(c + 2, r + 1), (c + 2, r - 1), (c + 1, r + 2), (c + 1, r -2), (c - 2, r + 1), (c - 2, r - 1), (c - 1, r + 2), (c - 1, r -2)]
    >>= \(c,r) -> guard (c `elem` [1..8] && r `elem` [1..8]) >> return (c,r)
    where
        onBoard x = x `elem` [1..8]

-- >>> knightMoves (6, 1)
-- >>> knightMoves' (6, 1)
-- [(8,2),(7,3),(4,2),(5,3)]
-- [(8,2),(7,3),(4,2),(5,3)]
--

-- Bind is simpler when lambdas are replaced by functions
in3 :: KnightPosition -> [KnightPosition]
in3 start = pure start >>= knightMoves >>= knightMoves >>= knightMoves

-- Some times Do Notation is more verbose than bind
in3' :: KnightPosition -> [KnightPosition]
in3' start = do
    start' <- knightMoves start
    start'' <- knightMoves start'
    knightMoves start''

-- >>> length $ in3 (6,2)
-- >>> length $ in3' (6,2)
-- 202
-- 202
--

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
