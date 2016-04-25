-- |
{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Default
  ( DefaultMap(..)
  , lookupDefault
  ) where

import Control.Applicative ((<|>))
import Data.Monoid ((<>))

data DefaultMap m v = DefaultMap
  { defaultMap :: m v
  , defaultValue :: Maybe v
  }

instance Functor m => Functor (DefaultMap m) where
  fmap f (DefaultMap m d) = DefaultMap (fmap f m) (fmap f d)

instance (Monoid v, Monoid (m v)) => Monoid (DefaultMap m v) where
  mempty = DefaultMap mempty Nothing
  mappend (DefaultMap m1 d1) (DefaultMap m2 d2) = DefaultMap (m1 <> m2) (d1 <> d2)

lookupDefault :: (m v -> Maybe v) -> DefaultMap m v -> Maybe v
lookupDefault l (DefaultMap m d) = l m <|> d
