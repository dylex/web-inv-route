-- |
-- An efficient map for sequences.
-- This is the core of the routing infrastructure.
-- If you have a set of routes represented as 'Sequence's, you can create a routing table using 'mconcat' and 'singletonSequenceApp':
-- 
-- >>> :set -XOverloadedStrings
-- >>> import Control.Invertible.Monoidal
-- >>> import Web.Route.Invertible.Parameter
-- >>> let p1 = "item" *< parameter :: Sequence String Int
-- >>> let p2 = "object" *< parameter :: Sequence String String
-- >>> let r = mconcat [singletonSequenceApp p1 [Left], singletonSequenceApp p2 [Right] {- ... -}] :: SequenceMapApp String [] (Either Int String)
-- >>> lookupSequenceApp ["object", "foo"] r
-- [Right "foo"]
-- >>> lookupSequenceApp ["item", "123"] r
-- [Left 123]
-- >>> lookupSequenceApp ["item", "bar"] r
-- []
--
{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Web.Route.Invertible.Map.Sequence
  ( SequenceMap(..)
  , singletonSequence
  , lookupSequence
  -- * Example usage
  , SequenceMapApp
  , singletonSequenceApp
  , lookupSequenceApp
  ) where

import Prelude hiding (lookup)

import Control.Applicative (Alternative(..))
import Control.Invertible.Monoidal.Free
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State (evalState)

import Web.Route.Invertible.String
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Sequence
import Web.Route.Invertible.Dynamics
import Web.Route.Invertible.Map.Placeholder

-- |A routing map for 'Sequence' parsers.
-- Each joined ('Control.Invertible.Monoidal.>*<') component in the 'Sequence' becomes a level of the map.
data SequenceMap s a = SequenceMap
  { sequenceMapPlaceholder :: PlaceholderMap s (SequenceMap s a)
  , sequenceMapValue :: !(Maybe a)
  } deriving (Eq, Show)

unionSequenceWith :: RouteString s => (Maybe a -> Maybe a -> Maybe a) -> SequenceMap s a -> SequenceMap s a -> SequenceMap s a
unionSequenceWith f (SequenceMap m1 v1) (SequenceMap m2 v2) =
  SequenceMap (unionPlaceholderWith (unionSequenceWith f) m1 m2) (f v1 v2)

-- |Values are combined using 'mappend'.
instance (RouteString s, Monoid a) => Monoid (SequenceMap s a) where
  mempty = empty
  mappend = unionSequenceWith mappend

instance Functor (SequenceMap s) where
  fmap f (SequenceMap m v) = SequenceMap (fmap f <$> m) (f <$> v)

leaf :: Maybe a -> SequenceMap s a
leaf = SequenceMap emptyPlaceholderMap

instance RouteString s => Applicative (SequenceMap s) where
  pure = leaf . Just
  SequenceMap fm fv <*> a = maybe id (\f -> (f <$> a <|>)) fv
    $ SequenceMap ((<*> a) <$> fm) Nothing
  SequenceMap am Nothing *> b =
    SequenceMap ((*> b) <$> am) Nothing
  SequenceMap am (Just _) *> b = b <|>
    SequenceMap ((*> b) <$> am) Nothing

instance RouteString s => Alternative (SequenceMap s) where
  empty = leaf Nothing
  (<|>) = unionSequenceWith (<|>)

instance RouteString s => Monad (SequenceMap s) where
  SequenceMap mm mv >>= f = maybe id ((<|>) . f) mv
    $ SequenceMap ((>>= f) <$> mm) Nothing
  (>>) = (*>)

instance RouteString s => MonadPlus (SequenceMap s)

newtype SequenceMapP s a = SequenceMapP { sequenceMapP :: SequenceMap s (DynamicState a) }

instance Functor (SequenceMapP s) where
  fmap f (SequenceMapP m) = SequenceMapP $ fmap (fmap f) m

instance RouteString s => Applicative (SequenceMapP s) where
  pure = SequenceMapP . pure . pure
  SequenceMapP f <*> SequenceMapP m = SequenceMapP $ ((<*>) <$> f) <*> m
  SequenceMapP a  *> SequenceMapP b = SequenceMapP $ ( (*>) <$> a)  *> b

instance RouteString s => Alternative (SequenceMapP s) where
  empty = SequenceMapP empty
  SequenceMapP a <|> SequenceMapP b = SequenceMapP $ a <|> b

placeholderMap :: RouteString s => Placeholder s a -> SequenceMapP s a
placeholderMap p = SequenceMapP $
  SequenceMap (pure <$> singletonPlaceholderState p) Nothing

singletonSequenceP :: RouteString s => Sequence s a -> SequenceMapP s a
singletonSequenceP = runFree . mapFree placeholderMap . freeSequence

-- |A sequence representing a single 'Sequence', with underlying @s@ strings as keys mapping to functions that convert from the resulting parsed parameters to the associated 'Sequence' value.
-- Note that a single 'Sequence' can create multiple elements in the map, so this is not strictly a /singleton/.
singletonSequence :: RouteString s => Sequence s a -> SequenceMap s (DynamicState a)
singletonSequence = sequenceMapP . singletonSequenceP

-- |Lookup a list of strings in a 'SequenceMap', returning all the associated values as tuples of the parsed dynamic placeholders and the associated value.
-- Note that if this map was created by 'singletonSequence', those values are themselves functions, so applying the first element to the second result will produce the original sequence value.
-- This is the equivalent of 'parseSequence':
--
-- > parseSequence q s === map (uncurry ($)) (lookupSequence s (singletonSequence q))
--
-- Except that 'lookupSequence' is far more efficient, especially when there are large number of alternatives.
lookupSequence :: RouteString s => [s] -> SequenceMap s a -> [DynamicResult a]
lookupSequence (s:l) (SequenceMap m _) = lookupPlaceholderWith s m $ lookupSequence l
lookupSequence [] (SequenceMap _ Nothing) = mzero
lookupSequence [] (SequenceMap _ (Just x)) = return ([], x)

-- |An example way to use 'SequenceMap' to abstract over and thus union multiple heterogeneous sequences.
type SequenceMapApp s m a = SequenceMap s (m (Dynamics -> a))

-- |Create a map from a single 'Sequence' parser.  Since this abstracts the type of the sequence @p@ (but not @a@), sequences with different underlying types can be combined in the same map.
singletonSequenceApp :: (RouteString s, Functor m) => Sequence s a -> m (a -> b) -> SequenceMapApp s m b
singletonSequenceApp p m = (\f -> fmap (. evalState f) m) <$> singletonSequence p

-- |Lookup a sequence in the map and return the value, combining ambiguous sequences using the 'Monoid' instance on their values.
-- Generally /O(log n)/ in the total number of sequences, except /O(n)/ in the length of the sequence and the number of different (ambiguous) 'SequenceParameter' types at each level (from 'PM.lookup').
-- However, it also incurs the cost of an 'fmap' on @m@, which it may be better to defer pending later lookups.
lookupSequenceApp :: (RouteString s, Functor m, Monoid (m a)) => [s] -> SequenceMapApp s m a -> m a
lookupSequenceApp l = foldMap (\(x, f) -> fmap ($ x) f) . lookupSequence l
