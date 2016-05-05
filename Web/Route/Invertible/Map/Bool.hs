-- |
{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Bool
  ( BoolMap(..)
  , emptyBoolMap
  , singletonBool
  , lookupBool
  ) where

import Data.Monoid ((<>))

data BoolMap v = BoolMap
  { boolMapFalse :: !(Maybe v)
  , boolMapTrue :: !(Maybe v)
  }

instance Functor BoolMap where
  fmap f (BoolMap a b) = BoolMap (fmap f a) (fmap f b)

instance (Monoid v) => Monoid (BoolMap v) where
  mempty = emptyBoolMap
  mappend (BoolMap a1 b1) (BoolMap a2 b2) = BoolMap (a1 <> a2) (b1 <> b2)

emptyBoolMap :: BoolMap a
emptyBoolMap = BoolMap Nothing Nothing

singletonBool :: Maybe Bool -> a -> BoolMap a
singletonBool Nothing a = BoolMap (Just a) (Just a)
singletonBool (Just False) a = BoolMap (Just a) Nothing
singletonBool (Just True) a = BoolMap Nothing (Just a)

lookupBool :: Bool -> BoolMap a -> Maybe a
lookupBool False (BoolMap a _) = a
lookupBool True (BoolMap _ a) = a
