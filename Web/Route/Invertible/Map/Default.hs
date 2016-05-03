-- |
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

data DefaultMap m v = DefaultMap
  { defaultMap :: !(m v)
  , defaultValue :: !(Maybe v)
  }

instance Functor m => Functor (DefaultMap m) where
  fmap f (DefaultMap m d) = DefaultMap (fmap f m) (fmap f d)

instance (Monoid v, Monoid (m v)) => Monoid (DefaultMap m v) where
  mempty = DefaultMap mempty Nothing
  mappend (DefaultMap m1 d1) (DefaultMap m2 d2) = DefaultMap (m1 <> m2) (d1 <> d2)

defaultingMap :: m v -> DefaultMap m v
defaultingMap m = DefaultMap m Nothing

defaultingValue :: Monoid (m v) => v -> DefaultMap m v
defaultingValue = DefaultMap mempty . Just

withDefaultMap :: (m v -> m v) -> DefaultMap m v -> DefaultMap m v
withDefaultMap f (DefaultMap m v) = DefaultMap (f m) v

lookupDefault :: (m v -> Maybe v) -> DefaultMap m v -> Maybe v
lookupDefault l (DefaultMap m d) = l m <|> d
