-- |
-- A map transformer that allows unknown keys to map to a default value.
{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Default
  ( DefaultMap(..)
  , defaultingMap
  , defaultingValue
  , withDefaultMap
  , lookupDefault
  ) where

import Control.Applicative ((<|>))
import Data.Monoid ((<>))

-- |A map that also provides a default value, for when a key is not found in the underlying map, parameterized over the type of the map.
data DefaultMap m v = DefaultMap
  { defaultMap :: !(m v)
  , defaultValue :: !(Maybe v)
  }

instance Functor m => Functor (DefaultMap m) where
  fmap f (DefaultMap m d) = DefaultMap (fmap f m) (fmap f d)

instance (Monoid v, Monoid (m v)) => Monoid (DefaultMap m v) where
  mempty = DefaultMap mempty Nothing
  mappend (DefaultMap m1 d1) (DefaultMap m2 d2) = DefaultMap (m1 <> m2) (d1 <> d2)

-- |A simple map with no default value.
defaultingMap :: m v -> DefaultMap m v
defaultingMap m = DefaultMap m Nothing

-- |A trivial map with only a default value.
defaultingValue :: Monoid (m v) => v -> DefaultMap m v
defaultingValue = DefaultMap mempty . Just

-- |Transform the underlying map.
withDefaultMap :: (m v -> n v) -> DefaultMap m v -> DefaultMap n v
withDefaultMap f (DefaultMap m v) = DefaultMap (f m) v

-- |Given a lookup function for the underlying map, return the default value instead if the value is not in the map.
lookupDefault :: (m v -> Maybe v) -> DefaultMap m v -> Maybe v
lookupDefault l (DefaultMap m d) = l m <|> d
