-----------------------------------------------------------------------------
-- |
-- ...
-- Useful to demonstrate type classes.
-- |
-----------------------------------------------------------------------------

module Types.YesNo where

import Types.Tree
import Types.TrafficLight

class YesNo a where
    -- |
    -- Takes one value of a type that can be considered to hold some concept of
    -- true-ness and tells us for sure if it's true or not.
    -- |
    yesno :: a -> Bool

instance YesNo Int where  
    yesno 0 = False  
    yesno _ = True

instance YesNo [a] where  
    yesno [] = False  
    yesno _ = True 

instance YesNo Bool where  
    yesno = id

instance YesNo (Maybe a) where  
    yesno (Just _) = True  
    yesno Nothing = False

instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True 

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True 

-- |
-- Mimics if statements using YesNo values.
-- |
yesnoIf :: (YesNo y) => y -> a -> a -> a  
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult 

-- >>> yesnoIf True "YEAH!" "NO!"
-- "YEAH!"
--
