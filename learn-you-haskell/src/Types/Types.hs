module Types.Types where

import qualified Data.Map as Map
import Text.Printf

-- Record syntax
data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)

tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

-- >>> tellCar Car {year=1967, model="Mustang", company="Ford"}
-- "This Ford Mustang was made in 1967"
--


-- Don't constraint types but functions instead
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n 

-- >>> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)
-- Vector 148 666 222
--


-- Deriving instances
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Eq, Show, Read)

-- >>> let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}  
-- >>> let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}  
-- >>> let mca = Person {firstName = "Adam", lastName = "Yauch", age = 44}  
-- >>> mikeD == mikeD
-- True
--
-- >>> read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
-- Person {firstName = "Michael", lastName = "Diamond", age = 43}
--
-- >>> read "Just 't'" :: Maybe Char
-- Just 't'
--


-- Awesome enums with Algebraic Data Types
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- >>> Saturday == Sunday
-- False
--
-- >>> Monday `compare` Wednesday
-- LT
--
-- >>> succ Monday 
-- Tuesday
--
-- >>> [minBound .. maxBound] :: [Day]
-- [Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday]
--


-- Use synonyms to reveal purpose
type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name, PhoneNumber)]

phoneBook :: PhoneBook 
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]

-- Synonyms work with partially applied type constructors
type IntMap = Map.Map Int

-- >>> phoneBook
-- [("betty","555-2938"),("bonnie","452-2928"),("patsy","493-2928"),("lucille","205-2928"),("wendy","939-8282"),("penny","853-2492")]
--

-- An example: a high-school has lockers so that students have some place to put their Guns'n'Roses posters. Each
-- locker has a code combination. When a student wants a new locker, they tell the locker supervisor which locker
-- number they want and he gives them the code. However, if someone is already using that locker, he can't tell
-- them the code for the locker and they have to pick a different one.
data LockerState = Taken | Free deriving (Show, Eq)  

type Code = String  
  
type LockerMap = Map.Map Int (LockerState, Code)

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case Map.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!" 

-- >>> lockerLookup 100 lockers
-- Left "Locker 100 is already taken!"
--

-- Recursive data structures
infixr 5 :~
data List a = (:*) | a :~ (List a) deriving (Read, Eq, Ord)

instance (Show a) => Show (List a) where
    show (:*) = "~"
    show (x :~ (:*)) = show x
    show (x :~ xs) = show x ++ "~" ++ show xs

infixr 5 +++  
(+++) :: List a -> List a -> List a   
(:*) +++ ys = ys  
(x :~ xs) +++ ys = x :~ (xs +++ ys)

-- >>> 1:~2:~(:*) +++ 3:~(:*)
-- 1~2~3
--

