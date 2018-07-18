-- |
-- A newtyped version of "Data.Map.Strict" with a 'Monoid' instance providing @'mappend' = 'M.unionWith' 'mappend'@.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Route.Invertible.Map.Monoid
  ( MonoidMap(..)
  , insertMonoid
  , fromMonoidList
  , lookupMonoid
  ) where

import Prelude hiding (lookup)

import Data.Foldable (fold)
import qualified Data.Map.Strict as M
import Data.Semigroup (Semigroup((<>)))

-- |A specialized version of 'M.Map'.
newtype MonoidMap k a = MonoidMap { monoidMap :: M.Map k a }
  deriving (Eq, Foldable, Show)

instance (Ord k, Semigroup a) => Semigroup (MonoidMap k a) where
  MonoidMap a <> MonoidMap b = MonoidMap $ M.unionWith (<>) a b

-- |'mappend' is equivalent to @'M.unionWith' 'mappend'@.
instance (Ord k, Monoid a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap M.empty
  mappend (MonoidMap a) (MonoidMap b) = MonoidMap $ M.unionWith mappend a b
  mconcat = MonoidMap . M.unionsWith mappend . map monoidMap

instance Functor (MonoidMap k) where
  fmap f (MonoidMap m) = MonoidMap $ M.map f m

-- |Insert a new key and value in the map. If the key is already present in the map, the associated value is combined with the supplied value. Equivalent to @'M.insertWith' 'mappend'@.
insertMonoid :: (Ord k, Monoid a) => k -> a -> MonoidMap k a -> MonoidMap k a
insertMonoid k a (MonoidMap m) = MonoidMap $ M.insertWith mappend k a m

-- |Build a map from a list of key/value pairs. If the list contains more than one value for the same key, the values are combined. Equivalent to @'M.fromListWith' 'mappend'@.
fromMonoidList :: (Ord k, Monoid a) => [(k, a)] -> MonoidMap k a
fromMonoidList = MonoidMap . M.fromListWith mappend

-- |Lookup the value at a key in the map, returning 'mempty' if the key isn't in the map.
lookupMonoid :: (Ord k, Monoid a) => k -> MonoidMap k a -> a
lookupMonoid k (MonoidMap m) = fold $ M.lookup k m

