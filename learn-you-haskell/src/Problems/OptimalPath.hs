module OptimalPath where

data Section = Section { costToA :: Int
                       , costToB :: Int
                       , costOfCrossing :: Int
                       } deriving (Show)

type RoadSystem = [Section]

data Step = TakeA | TakeB | Cross
            deriving (Show)

data Solution = Solution { steps :: [Step]
                         , cost :: Int
                         } deriving (Show)

type Solutions = (Solution, Solution)

solveSection :: Solutions -> Section -> Solutions
solveSection
        ((Solution stepsToA currentCostToA), (Solution stepsToB currentCostToB))
        (Section costToA costToB costOfCrossing)
    = (solutionToA, solutionToB)
    where
        solutionToA
            | forwardCostToA <= crossingCostToA = Solution (stepsToA ++ [TakeA]) forwardCostToA
            | otherwise = Solution (stepsToB ++ [TakeB, Cross]) crossingCostToA
        solutionToB
            | forwardCostToB <= crossingCostToB = Solution (stepsToB ++ [TakeB]) forwardCostToB
            | otherwise = Solution (stepsToA ++ [TakeA, Cross]) crossingCostToB
        forwardCostToA = currentCostToA + costToA
        forwardCostToB = currentCostToB + costToB
        crossingCostToA = forwardCostToB + costOfCrossing
        crossingCostToB = forwardCostToA + costOfCrossing

optimalPath :: RoadSystem -> Solution
optimalPath roadSystem = bestSolution solutions
    where
        solutions = foldl solveSection ((Solution [] 0), (Solution [] 0)) roadSystem
        bestSolution (solutionToA@(Solution _ costToA), solutionToB@(Solution _ costToB))
            | costToA <= costToB = solutionToA
            | otherwise = solutionToB

heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 100]

-- >>> optimalPath heathrowToLondon
-- Solution {steps = [TakeB,Cross,TakeA,Cross,TakeB,TakeB], cost = 75}
--
