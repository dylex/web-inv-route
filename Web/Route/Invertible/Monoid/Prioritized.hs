module Web.Route.Invertible.Monoid.Prioritized
  ( Prioritized(..)
  ) where

import Data.Monoid ((<>))

-- |A trival monoid allowing each item to be given a priority when combining.
data Prioritized a = Prioritized
  { priority :: !Int -- ^ The priority this value, where larger numeric values have higher priority and take precedence over lower priorities.
  , prioritized :: !a -- ^ The prioritized value.
  }

instance Functor Prioritized where
  fmap f (Prioritized p x) = Prioritized p (f x)
-- |Combining two values with the same priority combines the values, otherwise it discards the value with a smaller priority.
instance Monoid a => Monoid (Prioritized a) where
  mempty = Prioritized minBound mempty
  mappend a1@(Prioritized p1 x1) a2@(Prioritized p2 x2) = case compare p1 p2 of
    LT -> a2
    GT -> a1
    EQ -> Prioritized p1 (x1 <> x2)
