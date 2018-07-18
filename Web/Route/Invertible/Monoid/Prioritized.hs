-- |
-- A monoid that combines according to priorities, allowing some values to take precedence over others.
module Web.Route.Invertible.Monoid.Prioritized
  ( Prioritized(..)
  ) where

import Data.Semigroup (Semigroup((<>)))

-- |A trival monoid allowing each item to be given a priority when combining.
data Prioritized a = Prioritized
  { priority :: !Int -- ^ The priority this value, where larger numeric values have higher priority and take precedence over lower priorities.
  , prioritized :: !a -- ^ The prioritized value.
  } deriving (Eq, Show)

instance Functor Prioritized where
  fmap f (Prioritized p x) = Prioritized p (f x)

instance Semigroup a => Semigroup (Prioritized a) where
  a1@(Prioritized p1 x1) <> a2@(Prioritized p2 x2) = case compare p1 p2 of
    LT -> a2
    GT -> a1
    EQ -> Prioritized p1 (x1 <> x2)

-- |Combining two values with the same priority combines the values, otherwise it discards the value with a smaller priority.
instance Monoid a => Monoid (Prioritized a) where
  mempty = Prioritized minBound mempty
  mappend a1@(Prioritized p1 x1) a2@(Prioritized p2 x2) = case compare p1 p2 of
    LT -> a2
    GT -> a1
    EQ -> Prioritized p1 (mappend x1 x2)

