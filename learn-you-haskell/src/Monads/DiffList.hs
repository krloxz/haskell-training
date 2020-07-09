module Monads.DiffList where

import Data.Monoid
import Control.Monad.Writer

-- |
-- Difference lists, a data structure for O(1) append on lists.
-- More info: https://hackage.haskell.org/package/dlist-0.8.0.8/docs/Data-DList.html
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

instance Semigroup (DiffList a) where
    (DiffList f) <> (DiffList g) = DiffList (\xs -> f (g xs))

instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

-- >>> fromDiffList $ toDiffList [1,2,3,4] `mappend` toDiffList [5,6,7]
-- [1,2,3,4,5,6,7]
--

-- A DiffList can be used to efficiently append logs in inverted order
gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
    | b == 0 = do
        tell (toDiffList ["Finished with " ++ show a])
        return a
    | otherwise = do
        result <- gcd' b (a `mod` b)
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
        return result

-- >>> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd' 20 15
-- Finished with 5
-- 15 mod 5 = 0
-- 20 mod 15 = 5
--

-- To compare performance of DiffList vs List run the following functions using ghci
-- This command prints out very fast: mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 50000
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
    tell (toDiffList ["0"])
finalCountDown x = do
    finalCountDown (x-1)
    tell (toDiffList [show x])

-- This command prints out really slow: mapM_ putStrLn . snd . runWriter $ finalCountDown' 50000
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
    tell ["0"]
finalCountDown' x = do
    finalCountDown' (x-1)
    tell [show x]
