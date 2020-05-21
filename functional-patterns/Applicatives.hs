module Applicatives where

import Text.Read
import Data.Either
import Data.Validation
import Control.Lens

-- Transforming values and structures together
stringA = Just "a" :: Maybe String
stringB = Just "b" :: Maybe String

spaceSeparate :: String -> String -> String
spaceSeparate x y = x ++ " " ++ y

apply :: Applicative f => f (a -> b) -> f a -> f b
apply f a = f <*> a

map' :: Functor f => (a -> b) -> f a -> f b
map' = fmap

combinedString :: Maybe String
combinedString = apply (apply (pure spaceSeparate) stringA) stringB

combinedString1 :: Maybe String
combinedString1 = pure spaceSeparate <*> stringA <*> stringB

combinedString2 :: Maybe String
combinedString2 = spaceSeparate `map'` stringA <*> stringB

combinedString3 :: Maybe String
combinedString3 = spaceSeparate <$> stringA <*> stringB

combinedString4 :: Maybe String
combinedString4 = (spaceSeparate <$> stringA) <*> stringB

-- Using apply + pure
a = apply (apply (pure spaceSeparate) stringA) stringB
a1 = apply ((pure spaceSeparate) `apply` stringA) stringB
a2 = ((pure spaceSeparate) `apply` stringA) `apply` stringB
a3 = pure spaceSeparate `apply` stringA `apply` stringB
a4 = pure spaceSeparate <*> stringA <*> stringB

-- Using apply + map
b = apply (map' spaceSeparate stringA) stringB
b2 = apply (spaceSeparate `map'` stringA) stringB
b3 = (spaceSeparate `map'` stringA) `apply` stringB
b4 = spaceSeparate `map'` stringA `apply` stringB
b5 = spaceSeparate <$> stringA <*> stringB

-- >>> b5
-- Just "a b"
--


-- Validation example
data Person = Person
    { firstName :: String
    , lastName  :: String
    , age       :: Maybe Int
    } deriving (Show)

type Error = String

-- Without Applicatives
required :: Maybe a -> Either [Error] a
required (Just a) = Right a
required Nothing = Left ["Value is required"]

integer :: Maybe String -> Either [Error] (Maybe Int)
integer (Just s) = case (readMaybe s :: Maybe Int) of
                        Just i  -> Right (Just i)
                        Nothing -> Left ["Not an integer: " ++ s]
integer Nothing = Right Nothing

validatePerson :: Maybe String -> Maybe String -> Maybe String -> Either [Error] Person
validatePerson firstName lastName age =
  let firstNameValue = required firstName
      lastNameValue = required lastName
      ageValue = integer age
  in case (firstNameValue, lastNameValue, ageValue) of
    ((Right f), (Right l), (Right a)) -> Right (Person f l a)
    _ ->
      let fError = fromLeft [] firstNameValue
          lError = fromLeft [] lastNameValue
          aError = fromLeft [] ageValue
      in Left(fError ++ lError ++ aError)

-- With Applicatives
required' :: Maybe a -> Validation [Error] a
required' (Just a) = _Success # a
required' Nothing = _Failure # ["Value is required"]

integer' :: Maybe String -> Validation [Error] (Maybe Int)
integer' (Just s) = case (readMaybe s :: Maybe Int) of
                         Just i  -> _Success # (Just i)
                         Nothing -> _Failure # [("Not an integer: " ++ s)]
integer' Nothing = _Success # Nothing

validatePerson' :: Maybe String -> Maybe String -> Maybe String -> Validation [Error] Person
validatePerson' firstName lastName age =
  Person
    <$> required' firstName
    <*> required' lastName
    <*> integer' age

-- A longer version
validatePerson'' :: Maybe String -> Maybe String -> Maybe String -> Validation [Error] Person
validatePerson'' firstName lastName age =
  apply
    (
      apply
        -- (apply (pure Person) (required' firstName))
        (map' Person (required' firstName))
        (required' lastName)
    )
    (integer' age)

-- >>> validatePerson' Nothing Nothing (Just "1z")
-- Failure ["Value is required","Value is required","Not an integer: 1z"]
--
