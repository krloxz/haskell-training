module Basic.Persons where

data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)

-- >>> :t phoneNumber
-- phoneNumber :: Person -> String
--

-- >>> Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- Person {firstName = "Buddy", lastName = "Finklestein", age = 43, height = 184.2, phoneNumber = "526-2928", flavor = "Chocolate"}
--
