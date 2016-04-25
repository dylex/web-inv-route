-- |
{-# LANGUAGE GADTs #-}
module Web.Route.Invertible.Map.Placeholder
  ( PlaceholderMap(..)
  , empty
  , unionWith
  , singleton
  , insert
  , lookup
  ) where

import Prelude hiding (lookup)

import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import Web.Route.Invertible.String
import Web.Route.Invertible.Placeholder
import qualified Web.Route.Invertible.Map.Monoid as MM
import qualified Web.Route.Invertible.Map.ParameterType as PM

data PlaceholderMap s a = PlaceholderMap
  { placeholderMapFixed :: !(HM.HashMap s a)
  , placeholderMapParameter :: !(PM.ParameterTypeMap s a)
  }

-- |Values are combined using 'mappend'.
instance (RouteString s, Monoid a) => Monoid (PlaceholderMap s a) where
  mempty = empty
  mappend = unionWith mappend

instance Functor (PlaceholderMap s) where
  fmap f (PlaceholderMap s p) = PlaceholderMap (fmap f s) (fmap f p)

empty :: PlaceholderMap s a
empty = PlaceholderMap HM.empty (MM.MonoidMap M.empty)

unionWith :: RouteString s => (a -> a -> a) -> PlaceholderMap s a -> PlaceholderMap s a -> PlaceholderMap s a
unionWith f (PlaceholderMap s1 (MM.MonoidMap p1)) (PlaceholderMap s2 (MM.MonoidMap p2)) =
  PlaceholderMap (HM.unionWith f s1 s2) (MM.MonoidMap $ M.unionWith f p1 p2)

singleton :: RouteString s => Placeholder s p -> a -> PlaceholderMap s a
singleton (PlaceholderFixed t) v = PlaceholderMap (HM.singleton t v) (MM.MonoidMap M.empty)
singleton t@PlaceholderParameter v = PlaceholderMap HM.empty (PM.singleton t v)

insert :: RouteString s => Placeholder s p -> a -> PlaceholderMap s a -> PlaceholderMap s a
insert (PlaceholderFixed t) v (PlaceholderMap s p) = PlaceholderMap (HM.insert t v s) p
insert t@PlaceholderParameter v (PlaceholderMap s p) = PlaceholderMap s (PM.insert t v p)

lookup :: RouteString s => s -> PlaceholderMap s a -> Either a [(Dynamic, a)]
lookup t (PlaceholderMap s p) = maybe (Right $ PM.lookup t p) Left $ HM.lookup t s
