module Functoriality where

import Data.Bifunctor
import Data.Char()
import Data.Functor.Const
import Data.Functor.Identity

data Pair a b = Pair a b deriving Show

instance Bifunctor Pair where
  -- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g (Pair x y) = Pair (f x) (g y)
  -- first :: (a -> b) -> p a c -> p b c
  first f (Pair x y) = Pair (f x) y
  -- second :: (b -> c) -> p a b -> p a c
  second g (Pair x y) = Pair x (g y)

-- >>> bimap (+1) (+2) (Pair 1 2)
-- >>> bimap (+1) (+2) (1,2)
-- Pair 2 4
-- (2,4)
--
-- >>> first (+1) (Pair 1 2)
-- Pair 2 2
--

-- >>> second (+2) (Pair 1 2)
-- Pair 1 4
--

type Maybe' a = Either (Const () a) (Identity a)

maybeToMaybe' :: Maybe a -> Maybe' a
maybeToMaybe' Nothing = Left (Const ())
maybeToMaybe' (Just x) = Right (Identity x)

maybe'ToMaybe :: Maybe' a -> Maybe a
maybe'ToMaybe (Left (Const ())) = Nothing
maybe'ToMaybe (Right (Identity x)) = Just x


-- >>> maybe'ToMaybe (maybeToMaybe' (Just 1))
-- Just 1
--
