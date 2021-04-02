-----------------------------------------------------------------------------
-- |
-- Monoid examples.
-- |
-----------------------------------------------------------------------------
module Monoids where

newtype And = And Bool deriving (Show, Eq)

instance Semigroup And where
    And x <> And y = And (x && y)

instance Monoid And where
    mempty = And True

-- >>> -- Identity:
-- >>> And True <> mempty == And True
-- True
--
-- >>> -- Associativity:
-- >>> And True <> (And True <> And False) == (And True <> And True) <> And False
-- True
--


newtype Or = Or Bool deriving (Show, Eq)

instance Semigroup Or where
    Or x <> Or y = Or (x || y)

instance Monoid Or where
    mempty = Or False

-- >>> -- Identity:
-- >>> Or True <> mempty == Or True
-- True
--
-- >>> -- Associativity:
-- >>> Or True <> (Or True <> Or False) == (Or True <> Or True) <> Or False
-- True
--


newtype SumMod3 a = SumMod3 a deriving (Show, Eq)

sumMod3 :: Integral a => a -> SumMod3 a
sumMod3 x = SumMod3 (x `mod` 3)

instance Integral a => Semigroup (SumMod3 a) where
    SumMod3 x <> SumMod3 y = SumMod3 ((x + y) `mod` 3)

instance Integral a => Monoid (SumMod3 a) where
    mempty = SumMod3 0

-- >>> -- Identity:
-- >>> sumMod3 25 <> mempty == sumMod3 25
-- True
--
-- >>> -- Associativity
-- >>> sumMod3 2 <> (sumMod3 1 <> sumMod3 4) == (sumMod3 2 <> sumMod3 1) <> sumMod3 4
-- True
--
