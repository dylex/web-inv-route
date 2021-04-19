-- |
-- Hybrid maps keyed on 'Placeholder', using "Data.HashMap.Strict" and "Web.Route.Invertible.Map.ParameterType".
{-# LANGUAGE GADTs #-}
module Web.Route.Invertible.Map.Placeholder
  ( PlaceholderMap(..)
  , emptyPlaceholderMap
  , unionPlaceholderWith
  , singletonPlaceholder
  , singletonPlaceholderState
  , insertPlaceholder
  , lookupPlaceholder
  , lookupPlaceholderWith
  ) where

import Prelude hiding (lookup)

import Control.Arrow (first)
import Data.Dynamic (Dynamic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import Web.Route.Invertible.String
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Dynamics
import Web.Route.Invertible.Map.Monoid
import Web.Route.Invertible.Map.ParameterType

-- |A map from 'Placeholder' keys to values, allowing lookups on @s@ strings.
-- 'PlaceholderFixed' keys represent normal keys, always take precedence, and provide /O(log n)/ operations.
-- 'PlaceholderParameter' keys represent types and allow multiple, dynamic parser-based lookups, at most /O(n)/.
data PlaceholderMap s a = PlaceholderMap
  { placeholderMapFixed :: !(HM.HashMap s a)
  , placeholderMapParameter :: !(ParameterTypeMap s a)
  } deriving (Eq, Show)

instance (RouteString s, Semigroup a) => Semigroup (PlaceholderMap s a) where
  (<>) = unionPlaceholderWith (<>)

-- |Values are combined using 'mappend'.
instance (RouteString s, Monoid a) => Monoid (PlaceholderMap s a) where
  mempty = emptyPlaceholderMap
  mappend = unionPlaceholderWith mappend

instance Functor (PlaceholderMap s) where
  fmap f (PlaceholderMap s p) = PlaceholderMap (fmap f s) (fmap f p)

-- |The empty map.
emptyPlaceholderMap :: PlaceholderMap s a
emptyPlaceholderMap = PlaceholderMap HM.empty (MonoidMap M.empty)

-- |Union with a combining function.
unionPlaceholderWith :: RouteString s => (a -> a -> a) -> PlaceholderMap s a -> PlaceholderMap s a -> PlaceholderMap s a
unionPlaceholderWith f (PlaceholderMap s1 (MonoidMap p1)) (PlaceholderMap s2 (MonoidMap p2)) =
  PlaceholderMap (HM.unionWith f s1 s2) (MonoidMap $ M.unionWith f p1 p2)

-- |A map with a single element.
singletonPlaceholder :: RouteString s => Placeholder s p -> a -> PlaceholderMap s a
singletonPlaceholder (PlaceholderFixed t) v = PlaceholderMap (HM.singleton t v) (MonoidMap M.empty)
singletonPlaceholder t@PlaceholderParameter v = PlaceholderMap HM.empty (singletonParameterType t v)

placeholderState :: Placeholder s a -> DynamicState a
placeholderState (PlaceholderFixed _) = pure ()
placeholderState PlaceholderParameter = getDynamic

singletonPlaceholderState :: RouteString s => Placeholder s a -> PlaceholderMap s (DynamicState a)
singletonPlaceholderState p = singletonPlaceholder p $ placeholderState p

-- |Insert a new key and value in the map.
insertPlaceholder :: RouteString s => Placeholder s p -> a -> PlaceholderMap s a -> PlaceholderMap s a
insertPlaceholder (PlaceholderFixed t) v (PlaceholderMap s p) = PlaceholderMap (HM.insert t v s) p
insertPlaceholder t@PlaceholderParameter v (PlaceholderMap s p) = PlaceholderMap s (insertParameterType t v p)

-- |Lookup a string in the map, returning either the value associated with a 'PlaceholderFixed' key, or the list of matching 'PlaceholderParameter' keys, parsed into a 'Dynamic' representation (the result of 'parseParameter') and the associated value, if any.
-- If no keys match, the result is @Right []@.
lookupPlaceholder :: RouteString s => s -> PlaceholderMap s a -> Either a [(Dynamic, a)]
lookupPlaceholder t (PlaceholderMap s p) =
  maybe (Right $ lookupParameterType t p) Left $ HM.lookup t s

lookupPlaceholderWith :: RouteString s => s -> PlaceholderMap s a -> (a -> [DynamicResult b]) -> [DynamicResult b]
lookupPlaceholderWith t (PlaceholderMap s p) f =
  maybe (concatMap (\(x,n) -> first (x:) <$> f n) $ lookupParameterType t p) f $ HM.lookup t s
