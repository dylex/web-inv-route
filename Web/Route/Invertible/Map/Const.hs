-- |
{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Const
  ( ConstMap(..)
  , lookupConst
  , defaultConstMap
  , flattenConstMap
  ) where

import Data.Monoid ((<>))

import Web.Route.Invertible.Map.Default

data ConstMap m v = ConstMap
  { constMap :: m v
  , constValue :: v }

instance Functor m => Functor (ConstMap m) where
  fmap f (ConstMap m v) = ConstMap (fmap f m) (f v)

instance (Monoid v, Monoid (m v)) => Monoid (ConstMap m v) where
  mempty = ConstMap mempty mempty
  mappend (ConstMap m1 v1) (ConstMap m2 v2) = ConstMap (m1 <> m2) (v1 <> v2)

lookupConst :: Monoid v => (m v -> v) -> ConstMap m v -> v
lookupConst l (ConstMap m v) = l m <> v

defaultConstMap :: (Functor m, Monoid v) => ConstMap m v -> DefaultMap m v
defaultConstMap (ConstMap m v) = DefaultMap (fmap (<> v) m) (Just v)

flattenConstMap :: (Functor m, Monoid v) => ConstMap (DefaultMap m) v -> DefaultMap m v
flattenConstMap (ConstMap (DefaultMap m d) v) = DefaultMap (fmap (<> v) m) (d <> Just v)
