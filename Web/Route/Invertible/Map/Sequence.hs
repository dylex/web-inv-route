-- |
-- An efficient map for sequences.
-- This is the core of the routing infrastructure.
-- If you have a set of routes represented as 'Sequence's, you can create a routing table using 'mconcat' and 'singletonSequenceApp':
-- 
-- >>> :set -XOverloadedStrings
-- >>> import Control.Invertible.Monoidal
-- >>> let p1 = "item" *< parameter :: Sequence String Int
-- >>> let p2 = "object" *< parameter :: Sequence String String
-- >>> let r = mconcat [singletonSequenceApp p1 [Left], singletonSequenceApp p2 [Right] {- ... -}] :: SequenceMapApp [] (Either Int String)
-- >>> lookupSequenceApp ["object", "foo"] r
-- [Right "foo"]
-- >>> lookupSequenceApp ["item", "123"] r
-- [Left 123]
-- >>> lookupSequenceApp ["item", "bar"] r
-- []
--
{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables #-}
module Web.Route.Invertible.Map.Sequence
  ( SequenceMap(..)
  , SequencePlaceholderValue
  , singletonSequence
  , lookupSequence
  , SequenceMapApp
  , singletonSequenceApp
  , lookupSequenceApp
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Control.Arrow (first, (&&&))
import Control.Invertible.Monoidal.Free
import Control.Monad (mzero)
import Data.Dynamic (Dynamic, fromDyn)
import qualified Data.Invertible as I

import Web.Route.Invertible.String
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Map.Placeholder

-- |A routing map for 'Sequence' parsers.
data SequenceMap s a = SequenceMap
  { sequenceMapPlaceholder :: PlaceholderMap s (SequenceMap s a)
  , sequenceMapValue :: !(Maybe a)
  }

-- |Values are combined using 'mappend'.
instance (RouteString s, Monoid a) => Monoid (SequenceMap s a) where
  mempty = leaf Nothing
  mappend = unionSequenceWith mappend

instance Functor (SequenceMap s) where
  fmap f (SequenceMap m v) = SequenceMap (fmap f <$> m) (f <$> v)

leaf :: Maybe a -> SequenceMap s a
leaf = SequenceMap emptyPlaceholderMap

unionSequenceWith :: RouteString s => (Maybe a -> Maybe a -> Maybe a) -> SequenceMap s a -> SequenceMap s a -> SequenceMap s a
unionSequenceWith f (SequenceMap m1 v1) (SequenceMap m2 v2) =
  SequenceMap (unionPlaceholderWith (unionSequenceWith f) m1 m2) (f v1 v2)

union :: RouteString s => SequenceMap s a -> SequenceMap s a -> SequenceMap s a
union = unionSequenceWith (<|>)

type SequencePlaceholderValue = [Dynamic]

placeholderValue :: Placeholder s a -> SequencePlaceholderValue -> a
placeholderValue (PlaceholderFixed _) _ = ()
placeholderValue PlaceholderParameter ~(x:_) = fromDyn x (error "SequenceMap: type error")

singletonFree :: RouteString s => Free (Placeholder s) a -> SequenceMap s (SequencePlaceholderValue -> a)
singletonFree Empty = leaf $ Just $ const ()
singletonFree (Free p) =
  SequenceMap (singletonPlaceholder p $ leaf $ Just $ placeholderValue p) Nothing
singletonFree (Transform f m) = fmap (I.biTo f) <$> singletonFree m
singletonFree (Join p q) = jq 0 $ singletonFree p where
  q' = singletonFree q
  jq i (SequenceMap m Nothing) = jm i m
  jq i (SequenceMap m (Just v)) = (mv i v <$> q') `union` jm i m
  jm i (PlaceholderMap s t) =
    SequenceMap (PlaceholderMap (jq i <$> s) (jq (succ i) <$> t)) Nothing
  mv i v o = v &&& o . drop i
singletonFree (Choose p q) =
  (fmap Left  <$> singletonFree p) `union`
  (fmap Right <$> singletonFree q)

singletonSequence :: RouteString s => Sequence s a -> SequenceMap s (SequencePlaceholderValue -> a)
singletonSequence = singletonFree . freeSequence

lookupSequence :: RouteString s => [s] -> SequenceMap s a -> [(SequencePlaceholderValue, a)]
lookupSequence (s:l) (SequenceMap m _) =
  either (lookupSequence l) (concatMap $ \(x,n) -> first (x:) <$> lookupSequence l n) $ lookupPlaceholder s m
lookupSequence [] (SequenceMap _ Nothing) = mzero
lookupSequence [] (SequenceMap _ (Just x)) = return ([], x)

type SequenceMapApp s m a = SequenceMap s (m (SequencePlaceholderValue -> a))

-- |Create a map from a single 'Sequence' parser.  Since this abstracts the type of the sequence @p@ (but not @a@), sequences with different underlying types can be combined in the same map.
singletonSequenceApp :: (RouteString s, Functor m) => Sequence s a -> m (a -> b) -> SequenceMapApp s m b
singletonSequenceApp p m = (\f -> fmap (. f) m) <$> singletonSequence p

-- |Lookup a sequence in the map and return the value, combining ambiguous sequences using the 'Monoid' instance on their values.
-- Generally /O(log n)/ in the total number of sequences, except /O(n)/ in the length of the sequence and the number of different (ambiguous) 'SequenceParameter' types at each level (from 'PM.lookup').
-- However, it also incurs the cost of an 'fmap' on @m@, which it may be better to defer pending later lookups.
lookupSequenceApp :: (RouteString s, Functor m, Monoid (m a)) => [s] -> SequenceMapApp s m a -> m a
lookupSequenceApp l = foldMap (\(x, f) -> fmap ($ x) f) . lookupSequence l
