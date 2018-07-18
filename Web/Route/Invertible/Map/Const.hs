-- |
-- A map transformer that allows all keys to (additionally) map to a constant value.
{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Const
  ( ConstMap(..)
  , withConstMap
  , constantMap
  , constantValue
  , lookupConst
  , flattenConstMap
  , ConstDefaultMap
  , flattenConstDefaultMap
  ) where

import Data.Semigroup (Semigroup((<>)))

import Web.Route.Invertible.Map.Default

-- |A monoid map where every key (additionally) maps to the same constant value, parameterized over the type of the map.
data ConstMap m v = ConstMap
  { constMap :: !(m v) -- ^The underlying map.
  , constValue :: !v } -- ^The constant value to return for any key.
  deriving (Show)

instance Functor m => Functor (ConstMap m) where
  fmap f (ConstMap m v) = ConstMap (fmap f m) (f v)

instance (Semigroup v, Semigroup (m v)) => Semigroup (ConstMap m v) where
  ConstMap m1 v1 <> ConstMap m2 v2 = ConstMap (m1 <> m2) (v1 <> v2)

instance (Monoid v, Monoid (m v)) => Monoid (ConstMap m v) where
  mempty = ConstMap mempty mempty
  mappend (ConstMap m1 v1) (ConstMap m2 v2) = ConstMap (mappend m1 m2) (mappend v1 v2)

-- |Transform the underlying map.
withConstMap :: (m v -> n v) -> ConstMap m v -> ConstMap n v
withConstMap f (ConstMap m v) = ConstMap (f m) v

-- |A simple map that has no constant value (or rather, has a constant value of 'mempty') so acts just like the given monoid map.
constantMap :: Monoid v => m v -> ConstMap m v
constantMap m = ConstMap m mempty

-- |A trivial map that maps all keys to the same value.
constantValue :: Monoid (m v) => v -> ConstMap m v
constantValue = ConstMap mempty

-- |Given a lookup function for the underlying map, add the constant value to the result (using '<>').
lookupConst :: Semigroup v => (m v -> v) -> ConstMap m v -> v
lookupConst l (ConstMap m v) = l m <> v

-- |Convert a 'ConstMap' to an equivalent but more efficient 'DefaultMap'.
-- Although the resulting map will return the same value for lookups, combining it with other maps will have different results (this operation is not distributive).
flattenConstMap :: (Functor m, Semigroup v) => ConstMap m v -> DefaultMap m v
flattenConstMap (ConstMap m v) = DefaultMap (fmap (<> v) m) (Just v)

-- |A 'DefaultMap' wrapped in a 'ConstMap', for when you want both a constant and default value.
type ConstDefaultMap m = ConstMap (DefaultMap m)

-- |Do the same as 'flattenConstMap' but for 'ConstDefaultMap' by merging the resulting 'DefaultMap' layers.
flattenConstDefaultMap :: (Functor m, Semigroup v) => ConstDefaultMap m v -> DefaultMap m v
flattenConstDefaultMap (ConstMap (DefaultMap m d) v) = DefaultMap (fmap (<> v) m) (d <> Just v)
