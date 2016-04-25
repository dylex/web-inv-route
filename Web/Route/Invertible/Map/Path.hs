-- |
-- An efficient routing map for URL paths.
-- This is the core of the routing infrastructure.
-- If you have a set of actions represented as (or convertable to) 'PathRoute's, you can create a routing table using 'mconcat' and 'singleton':
-- 
-- >>> :set -XOverloadedStrings
-- >>> import Control.Invertible.Monoidal
-- >>> let p1 = "item" *< parameter :: Path Int
-- >>> let p2 = "object" *< parameter :: Path String
-- >>> let r = mconcat [singletonApp p1 [Left], singletonApp p2 [Right] {- ... -}] :: PathMapApp [] (Either Int String)
-- >>> lookupApp ["object", "foo"] r
-- [Right "foo"]
-- >>> lookupApp ["item", "123"] r
-- [Left 123]
-- >>> lookupApp ["item", "bar"] r
-- []
--
{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables #-}
module Web.Route.Invertible.Map.Path
  ( PathMap(..)
  , unionWith
  , union
  , singleton
  , lookup
  , PathMapApp
  , singletonApp
  , lookupApp
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Arrow (first, (&&&))
import Control.Monad (mzero)
import Data.Dynamic (Dynamic, fromDyn)
import qualified Data.Invertible as I

import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Path.Types
import qualified Web.Route.Invertible.Map.Placeholder as PM

-- |A routing map for 'Path' parsers.
data PathMap a = PathMap
  { pathMapPlaceholder :: PM.PlaceholderMap PathString (PathMap a)
  , pathMapValue :: !(Maybe a)
  }

-- |Values are combined using 'mappend'.
instance Monoid a => Monoid (PathMap a) where
  mempty = leaf Nothing
  mappend = unionWith mappend

instance Functor PathMap where
  fmap f (PathMap m v) = PathMap (fmap f <$> m) (f <$> v)

leaf :: Maybe a -> PathMap a
leaf = PathMap PM.empty

unionWith :: (Maybe a -> Maybe a -> Maybe a) -> PathMap a -> PathMap a -> PathMap a
unionWith f (PathMap m1 v1) (PathMap m2 v2) =
  PathMap (PM.unionWith (unionWith f) m1 m2) (f v1 v2)

union :: PathMap a -> PathMap a -> PathMap a
union = unionWith (<|>)

placeholderValue :: Placeholder s a -> [Dynamic] -> a
placeholderValue (PlaceholderFixed _) _ = ()
placeholderValue PlaceholderParameter ~(x:_) = fromDyn x (error "PathMap: type error")

singleton :: Path a -> PathMap ([Dynamic] -> a)
singleton PathEmpty = leaf $ Just $ const ()
singleton (PathPlaceholder p) =
  PathMap (PM.singleton p $ leaf $ Just $ placeholderValue p) Nothing
singleton (PathTransform f m) = fmap (I.biTo f) <$> singleton m
singleton (PathJoin p q) = jq 0 $ singleton p where
  q' = singleton q
  jq i (PathMap m Nothing) = jm i m
  jq i (PathMap m (Just v)) = (mv i v <$> q') `union` jm i m
  jm i (PM.PlaceholderMap s t) =
    PathMap (PM.PlaceholderMap (jq i <$> s) (jq (succ i) <$> t)) Nothing
  mv i v o = v &&& o . drop i
singleton (PathChoose p q) =
  (fmap Left  <$> singleton p) `union`
  (fmap Right <$> singleton q)

lookup :: [PathString] -> PathMap a -> [([Dynamic], a)]
lookup (s:l) (PathMap m _) =
  either (lookup l) (concatMap $ \(x,n) -> first (x:) <$> lookup l n) $ PM.lookup s m
lookup [] (PathMap _ Nothing) = mzero
lookup [] (PathMap _ (Just x)) = return ([], x)

type PathMapApp m a = PathMap (m ([Dynamic] -> a))

-- |Create a map from a single 'Path' parser.  Since this abstracts the type of the path @p@ (but not @a@), paths with different underlying types can be combined in the same map.
singletonApp :: Functor m => Path a -> m (a -> b) -> PathMapApp m b
singletonApp p m = (\f -> fmap (. f) m) <$> singleton p

-- |Lookup a path in the map and return the value, combining ambiguous paths using the 'Monoid' instance on their values.
-- Generally /O(log n)/ in the total number of paths, except /O(n)/ in the length of the path and the number of different (ambiguous) 'PathParameter' types at each level (from 'PM.lookup').
-- However, it also incurs the cost of an 'fmap' on @m@, which it may be better to defer pending later lookups.
lookupApp :: (Functor m, Monoid (m a)) => [PathString] -> PathMapApp m a -> m a
lookupApp l = foldMap (\(x, f) -> fmap ($ x) f) . lookup l
