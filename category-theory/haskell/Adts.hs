
module Adts where

toEither :: Maybe a -> Either () a
toEither Nothing = Left ()
toEither (Just a) = Right a

toMaybe :: Either () a -> Maybe a
toMaybe (Left _) = Nothing
toMaybe (Right a) = Just a

-- >>> (Just 1) == (toMaybe (Right 1))
-- >>> (Right 1) == (toEither (Just 1))
-- True
-- True
--

data Shape = Circle Float
           | Rect Float Float
           | Square Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rect d h) = d * h
area (Square s) = s * s

circ :: Shape -> Float
circ (Circle r) = 2.0 * pi * r
circ (Rect d h) = 2.0 * (d + h)
circ (Square s) = 4.0 * s
