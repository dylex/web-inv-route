-- |
-- A newtyped version of "Data.HashMap.Strict" with a 'Monoid' instance providing @'mappend' = 'M.unionWith' 'mappend'@.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Route.Invertible.Map.MonoidHash
  ( MonoidHashMap(..)
  , insertMonoidHash
  , fromMonoidHashList
  , lookupMonoidHash
  ) where

import Prelude hiding (lookup)

import Data.Foldable (fold)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as M

-- |A specialized version of 'M.HashMap'.
newtype MonoidHashMap k a = MonoidHashMap { monoidHashMap :: M.HashMap k a }
  deriving (Eq, Foldable, Show)

-- |'mappend' is equivalent to @'M.unionWith' 'mappend'@.
instance (Eq k, Hashable k, Monoid a) => Monoid (MonoidHashMap k a) where
  mempty = MonoidHashMap M.empty
  mappend (MonoidHashMap a) (MonoidHashMap b) = MonoidHashMap $ M.unionWith mappend a b

instance Functor (MonoidHashMap k) where
  fmap f (MonoidHashMap m) = MonoidHashMap $ M.map f m

-- |Insert a new key and value in the map. If the key is already present in the map, the associated value is combined with the supplied value. Equivalent to @'M.insertWith' 'mappend'@.
insertMonoidHash :: (Eq k, Hashable k, Monoid a) => k -> a -> MonoidHashMap k a -> MonoidHashMap k a
insertMonoidHash k a (MonoidHashMap m) = MonoidHashMap $ M.insertWith mappend k a m

-- |Build a map from a list of key/value pairs. If the list contains more than one value for the same key, the values are combined. Equivalent to @'M.fromListWith' 'mappend'@.
fromMonoidHashList :: (Eq k, Hashable k, Monoid a) => [(k, a)] -> MonoidHashMap k a
fromMonoidHashList = MonoidHashMap . M.fromListWith mappend

-- |Lookup the value at a key in the map, returning 'mempty' if the key isn't in the map.
lookupMonoidHash :: (Eq k, Hashable k, Monoid a) => k -> MonoidHashMap k a -> a
lookupMonoidHash k (MonoidHashMap m) = fold $ M.lookup k m

