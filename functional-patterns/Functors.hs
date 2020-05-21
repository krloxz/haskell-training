-- Without Functors
maybeIntToString :: Maybe Int -> Maybe String
maybeIntToString (Just i) = Just(show i)
maybeIntToString Nothing = Nothing

eitherIntToString :: Either String Int -> Either String String
eitherIntToString (Right i) = Right(show i)
eitherIntToString (Left s) = Left s

listIntToString :: [Int] -> [String]
listIntToString (x:xs) = show x : listIntToString xs
listIntToString [] = []

-- These types are functors and support fmap!
-- map :: Functor f => (a -> b) -> f a -> f b
maybeIntToString' :: Maybe Int -> Maybe String
maybeIntToString' m = fmap show m

eitherIntToString' :: Either String Int -> Either String String
eitherIntToString' e = fmap show e

listIntToString' :: [Int] -> [String]
listIntToString' l = fmap show l

-- Applying 'eta reduce' to remove irrelevant parameters
maybeIntToString'' :: Maybe Int -> Maybe String
maybeIntToString'' = fmap show

eitherIntToString'' :: Either String Int -> Either String String
eitherIntToString'' = fmap show

listIntToString'' :: [Int] -> [String]
listIntToString'' = fmap show
