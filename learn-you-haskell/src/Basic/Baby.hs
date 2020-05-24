module Basic.Baby where

doubleMe :: (Num a) => a -> a
doubleMe x = x + x

doubleUs :: (Num a) => a -> a -> a
doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if x > 100  
                        then x  
                        else x * 2

conanO'Brien :: String
conanO'Brien = "It's a-me, Conan O'Brien!"

boomBangs :: [Int] -> [String]
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

-- length using list comprehension
length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

removeNonUppercase :: String -> String
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

rightTriangles :: [(Int, Int, Int)]
rightTriangles = [
  (a,b,c) 
  | c <- [1..10], b <- [1..c], a <- [1..b]
  , a ^ (2 :: Int) + b ^ (2 :: Int) == c ^ (2 :: Int), a + b + c == 24
  ]

-- length using pattern matching
length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

firstLetter :: String -> String
firstLetter "" = "Empty string"
firstLetter string@(c:_) = "The first letter of " ++ string ++ " is " ++ [c]

-- Guards
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ** 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- Nested wheres
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / square height
            where square x = x ** 2

-- Case expressions are the generalization of pattern matching
-- These two functions are the same:
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x

head'' :: [a] -> a  
head'' xs = case xs of []    -> error "No head for empty lists!"  
                       (x:_) -> x

repeat' :: a -> [a]
repeat' x = x:repeat' x 

-- >>> take 5 (repeat' 3)
-- [3,3,3,3,3]
--

-- Recursion
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)  
    in  smallerSorted ++ [x] ++ biggerSorted
-- >>> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- [1,2,2,3,3,4,4,5,6,7,8,9,10]
--
-- >>> quicksort "the quick brown fox jumps over the lazy dog" 
-- "        abcdeeefghhijklmnoooopqrrsttuuvwxyz"
--

-- Higher order functions
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
-- >>> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]
--
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f y x = f x y 
-- >>> flip'' zip [1,2,3,4,5] "hello"
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]
--

-- Map
-- >>> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- [[1,4],[9,16,25,36],[49,64]]
--

-- Folding (reducing)
sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0

-- >>> sum'' [3,5,2,1]
-- 11
--
