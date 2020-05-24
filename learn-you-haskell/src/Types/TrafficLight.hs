-----------------------------------------------------------------------------
-- |
-- Defines the states of a traffic light.
-- Useful to demonstrate class instantiation.
-- |
-----------------------------------------------------------------------------
module Types.TrafficLight where

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where  
    Red == Red = True  
    Green == Green = True  
    Yellow == Yellow = True  
    _ == _ = False

instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"

-- >>> Red `elem` [Yellow, Green, Red]
-- True
--
