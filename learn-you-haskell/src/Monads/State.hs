module Monads.State where

import Control.Monad.State

type Stack = [Int]

pop :: State Stack Int
pop = state $ \stack -> case stack of
                        [] -> error "Not pop for empty stacks"
                        (x:newStack) -> (x, newStack)

push :: Int -> State Stack ()
push x = state $ \stack -> ((), x:stack)

-- Do notation is sintactic sugar for bind chaining.
useStack :: State Stack Int
useStack = do
    push 3
    a <- pop
    push a
    push a
    pop

-- >>> runState useStack [2,1]
-- (3,[3,2,1])
--

-- Using bind, lambdas are required to meet the definition of bind.
-- The state can be hidden as it's relevant for the State Monad only.
useStack' :: State Stack Int
useStack' = push 3
    >>= \_ -> pop
    >>= \a -> push a
    >>= \_ -> push a
    >>= \_ -> pop

-- >>> runState useStack' [2,1]
-- (3,[3,2,1])
--

-- In Do notation a variable is assigned or bind (<-) the result of a Monadic action.
stackyStack :: State Stack ()
stackyStack = do
    stackNow <- get
    if stackNow == [1,2,3]
        then put [8,3,1]
        else put [9,2,1]

-- >>> runState stackyStack [1,2]
-- ((),[9,2,1])
--

-- Using the bind operator the second Monadic action is a function
-- which parameter is the result of a the first Monadic action.
stackyStack' :: State Stack ()
stackyStack' = get
    >>= \stackNow -> if stackNow == [1,2,3]
                        then put [8,3,1]
                        else put [9,2,1]

-- >>> runState stackyStack' [1,2]
-- ((),[9,2,1])
--
