-- |
-- A newtyped version of "Data.Map.Strict" with a 'Monoid' instance providing @'mappend' = 'M.unionWith' 'mappend'@.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Route.Map
  ( MonoidMap(..)
  , empty
  , singleton
  , insert
  , fromList
  , lookup
  , lookupParameter'
  , lookupParameter
  , reduce
  , lookupExactly
  , fallback
  ) where

import Prelude hiding (lookup)

import Data.Foldable (fold)
import qualified Data.Map.Strict as M

import Web.Route.Resolver
import Web.Route.Parameter

-- |A specialized version of 'M.Map'.
newtype MonoidMap k a = MonoidMap { monoidMap :: M.Map k a }
  deriving (Foldable)

-- |'mappend' is equivalent to @'M.unionWith' 'mappend'@.
instance (Ord k, Monoid a) => Monoid (MonoidMap k a) where
  mempty = empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap $ M.unionWith mappend a b
  mconcat = MonoidMap . M.unionsWith mappend . map monoidMap

instance Functor (MonoidMap k) where
  fmap f (MonoidMap m) = MonoidMap $ M.map f m

-- |The empty map.
empty :: MonoidMap k a
empty = MonoidMap M.empty

-- |A map with a single element.
singleton :: k -> a -> MonoidMap k a
singleton k = MonoidMap . M.singleton k

-- |Insert a new key and value in the map. If the key is already present in the map, the associated value is combined with the supplied value. Equivalent to @'M.insertWith' 'mappend'@.
insert :: (Ord k, Monoid a) => k -> a -> MonoidMap k a -> MonoidMap k a
insert k a (MonoidMap m) = MonoidMap $ M.insertWith mappend k a m

-- |Build a map from a list of key/value pairs. If the list contains more than one value for the same key, the values are combined. Equivalent to @'M.fromListWith' 'mappend'@.
fromList :: (Ord k, Monoid a) => [(k, a)] -> MonoidMap k a
fromList = MonoidMap . M.fromListWith mappend

-- |Lookup the value at a key in the map, returning 'mempty' if the key isn't in the map.
lookup :: (Ord k, Monoid a) => k -> MonoidMap k a -> a
lookup k (MonoidMap m) = fold $ M.lookup k m

-- |Combine 'parseParameter' and 'M.lookup'.
lookupParameter' :: (Ord p, Parameter s p) => s -> MonoidMap p a -> Maybe a
lookupParameter' s (MonoidMap m) = do
  p <- parseParameter s
  M.lookup p m

-- |Combine 'parseParameter' and 'lookup'.
lookupParameter :: (Ord p, Parameter s p, Monoid a) => s -> MonoidMap p a -> a
lookupParameter s = fold . lookupParameter' s

-- |Eliminate 'Blank' values, and (strictly) produce an error for 'Conflict' values.
reduce :: MonoidMap k (Exactly a) -> MonoidMap k a
reduce (MonoidMap m) = MonoidMap $ M.mapMaybe exactlyToMaybe m

-- |Combine 'exactlyToMaybe' and 'lookup'.
lookupExactly :: Ord k => k -> MonoidMap k (Exactly a) -> Maybe a
lookupExactly k = exactlyToMaybe . lookup k

-- |When the first key is not in the map, produce a new map that falls back to the second key instead.
-- That is, create a new map where the second key's value is copied to the first key without overwriting.
fallback :: Ord k => k -> k -> MonoidMap k a -> MonoidMap k a
fallback from to (MonoidMap m)
  | Just v <- M.lookup to m = MonoidMap $ M.insertWith (\_ -> id) from v m
  | otherwise = MonoidMap m
