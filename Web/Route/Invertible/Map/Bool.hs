-- |Trivial map from boolean-valued keys to values, which just stores both possible values.
{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Bool
  ( BoolMap(..)
  , emptyBoolMap
  , singletonBool
  , lookupBool
  ) where

import Data.Monoid ((<>))

-- |A trivial, flat representation of a 'Bool'-keyed map.
-- Value existance (but not the values themselves) is strict.
data BoolMap v = BoolMap
  { boolMapFalse :: !(Maybe v) -- ^The value associated with 'False'.
  , boolMapTrue :: !(Maybe v) -- ^The value associated with 'True'.
  }

instance Functor BoolMap where
  fmap f (BoolMap a b) = BoolMap (fmap f a) (fmap f b)

instance (Monoid v) => Monoid (BoolMap v) where
  mempty = emptyBoolMap
  mappend (BoolMap a1 b1) (BoolMap a2 b2) = BoolMap (a1 <> a2) (b1 <> b2)

-- |The empty map.
emptyBoolMap :: BoolMap a
emptyBoolMap = BoolMap Nothing Nothing

-- |A map with a single element, or if the key is @Nothing@, with both elements with same value.
singletonBool :: Maybe Bool -> a -> BoolMap a
singletonBool Nothing a = BoolMap (Just a) (Just a)
singletonBool (Just False) a = BoolMap (Just a) Nothing
singletonBool (Just True) a = BoolMap Nothing (Just a)

-- |Lookup the value at a key in the map.
lookupBool :: Bool -> BoolMap a -> Maybe a
lookupBool False (BoolMap a _) = a
lookupBool True (BoolMap _ a) = a
