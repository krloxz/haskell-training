-----------------------------------------------------------------------------
-- |
-- Example of a coproduct of Int and Bool that is inferior than Either
-- because it allows multiple acceptable morphisms from it to Either.
-- |
-----------------------------------------------------------------------------
module Coproduct where

data Order a b c = Initial a | Middle b | Terminal c deriving (Show, Eq)

intToOrder :: Int -> Order Int Int Bool
intToOrder = Initial

boolToOrder :: Bool -> Order Int Int Bool
boolToOrder = Terminal

eitherFactorizer :: Either Int Bool -> Order Int Int Bool
eitherFactorizer (Left x) = intToOrder x
eitherFactorizer (Right b) = boolToOrder b

orderFactorizer :: Order Int Int Bool -> Either Int Bool
orderFactorizer (Initial x) = Left x
orderFactorizer (Middle x) = Left x
orderFactorizer (Terminal b) = Right b

orderFactorizer' :: Order Int Int Bool -> Either Int Bool
orderFactorizer' (Initial x) = Left x
orderFactorizer' (Middle _) = Left 1
orderFactorizer' (Terminal b) = Right b

-- >>> intToOrder 1 == (eitherFactorizer . Left $ 1)
-- >>> boolToOrder True == (eitherFactorizer . Right $ True)
-- True
-- True
--

-- >>> Left 1 == (orderFactorizer . intToOrder $ 1)
-- >>> Left 1 == (orderFactorizer' . intToOrder $ 1)
-- True
-- True
--

-- >>> Right True == (orderFactorizer . boolToOrder $ True)
-- >>> Right True == (orderFactorizer' . boolToOrder $ True)
-- True
-- True
--
