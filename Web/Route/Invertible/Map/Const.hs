-- |
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

import Data.Monoid ((<>))

import Web.Route.Invertible.Map.Default

data ConstMap m v = ConstMap
  { constMap :: !(m v)
  , constValue :: !v }

instance Functor m => Functor (ConstMap m) where
  fmap f (ConstMap m v) = ConstMap (fmap f m) (f v)

instance (Monoid v, Monoid (m v)) => Monoid (ConstMap m v) where
  mempty = ConstMap mempty mempty
  mappend (ConstMap m1 v1) (ConstMap m2 v2) = ConstMap (m1 <> m2) (v1 <> v2)

withConstMap :: (m v -> m v) -> ConstMap m v -> ConstMap m v
withConstMap f (ConstMap m v) = ConstMap (f m) v

constantMap :: Monoid v => m v -> ConstMap m v
constantMap m = ConstMap m mempty

constantValue :: Monoid (m v) => v -> ConstMap m v
constantValue = ConstMap mempty

lookupConst :: Monoid v => (m v -> v) -> ConstMap m v -> v
lookupConst l (ConstMap m v) = l m <> v

flattenConstMap :: (Functor m, Monoid v) => ConstMap m v -> DefaultMap m v
flattenConstMap (ConstMap m v) = DefaultMap (fmap (<> v) m) (Just v)

type ConstDefaultMap m = ConstMap (DefaultMap m)

flattenConstDefaultMap :: (Functor m, Monoid v) => ConstDefaultMap m v -> DefaultMap m v
flattenConstDefaultMap (ConstMap (DefaultMap m d) v) = DefaultMap (fmap (<> v) m) (d <> Just v)
