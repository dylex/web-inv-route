-- |
{-# LANGUAGE GADTs #-}
module Web.Route.Invertible.Map.Placeholder
  ( PlaceholderMap(..)
  , emptyPlaceholderMap
  , unionPlaceholderWith
  , singletonPlaceholder
  , insertPlaceholder
  , lookupPlaceholder
  ) where

import Prelude hiding (lookup)

import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import Web.Route.Invertible.String
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.ParameterType

data PlaceholderMap s a = PlaceholderMap
  { placeholderMapFixed :: !(HM.HashMap s a)
  , placeholderMapParameter :: !(ParameterTypeMap s a)
  }

-- |Values are combined using 'mappend'.
instance (RouteString s, Monoid a) => Monoid (PlaceholderMap s a) where
  mempty = emptyPlaceholderMap
  mappend = unionPlaceholderWith mappend

instance Functor (PlaceholderMap s) where
  fmap f (PlaceholderMap s p) = PlaceholderMap (fmap f s) (fmap f p)

emptyPlaceholderMap :: PlaceholderMap s a
emptyPlaceholderMap = PlaceholderMap HM.empty (MonoidMap M.empty)

unionPlaceholderWith :: RouteString s => (a -> a -> a) -> PlaceholderMap s a -> PlaceholderMap s a -> PlaceholderMap s a
unionPlaceholderWith f (PlaceholderMap s1 (MonoidMap p1)) (PlaceholderMap s2 (MonoidMap p2)) =
  PlaceholderMap (HM.unionWith f s1 s2) (MonoidMap $ M.unionWith f p1 p2)

singletonPlaceholder :: RouteString s => Placeholder s p -> a -> PlaceholderMap s a
singletonPlaceholder (PlaceholderFixed t) v = PlaceholderMap (HM.singleton t v) (MonoidMap M.empty)
singletonPlaceholder t@PlaceholderParameter v = PlaceholderMap HM.empty (singletonParameterType t v)

insertPlaceholder :: RouteString s => Placeholder s p -> a -> PlaceholderMap s a -> PlaceholderMap s a
insertPlaceholder (PlaceholderFixed t) v (PlaceholderMap s p) = PlaceholderMap (HM.insert t v s) p
insertPlaceholder t@PlaceholderParameter v (PlaceholderMap s p) = PlaceholderMap s (insertParameterType t v p)

lookupPlaceholder :: RouteString s => s -> PlaceholderMap s a -> Either a [(Dynamic, a)]
lookupPlaceholder t (PlaceholderMap s p) = maybe (Right $ lookupParameterType t p) Left $ HM.lookup t s
