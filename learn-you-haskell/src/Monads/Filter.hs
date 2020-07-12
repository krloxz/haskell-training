module Monads.Filter where

import Control.Monad

powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs  

-- >>> powerset [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
--

-- Explanation
-- [[1],[]]
-- [[2],[]]
-- [[3],[]]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]