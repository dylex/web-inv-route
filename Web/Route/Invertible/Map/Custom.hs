{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Web.Route.Invertible.Map.Custom
  ( CustomMap
  , singletonCustom
  , lookupCustom
  ) where

import Data.Maybe (mapMaybe)
import Data.Semigroup (Semigroup)
import Text.Show.Functions ()

newtype CustomMap q a b = CustomMap [(q -> Maybe a, b)]
  deriving (Show, Semigroup, Monoid)

instance Functor (CustomMap q a) where
  fmap f (CustomMap l) = CustomMap $ map (fmap f) l

singletonCustom :: (q -> Maybe a) -> b -> CustomMap q a b
singletonCustom f x = CustomMap [(f, x)]

lookupCustom :: q -> CustomMap q a b -> [(a, b)]
lookupCustom q (CustomMap l) = mapMaybe (\(f, r) -> (, r) <$> f q) l
