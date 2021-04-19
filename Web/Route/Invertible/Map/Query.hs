{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Query
  ( QueryMap(..)
  , defaultQueryMap
  , singletonQuery
  , singletonQueryState
  , lookupQuery
  ) where

import qualified Data.HashMap.Lazy as HM

import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Map.Placeholder
import Web.Route.Invertible.Query
import Web.Route.Invertible.Dynamics

-- |A map for parsing query parameters as 'Placeholder's by name, kept in alphabetical order to ensure consistent unions.
data QueryMap a = QueryFinal (Maybe a) | QueryMap
  { queryParam :: !QueryString
  , queryMap :: !(PlaceholderMap QueryString (QueryMap a))
  , queryMissing :: !(QueryMap a)
  } deriving (Eq, Show)

instance Functor QueryMap where
  fmap f (QueryFinal v) = QueryFinal (fmap f v)
  fmap f (QueryMap n m d) = QueryMap n (fmap (fmap f) m) (fmap f d)

instance Semigroup a => Semigroup (QueryMap a) where
  QueryFinal a <> QueryFinal b = QueryFinal (a <> b)
  q@(QueryFinal _) <> (QueryMap n m d) = QueryMap n m (q <> d)
  (QueryMap n m d) <> q@(QueryFinal _) = QueryMap n m (d <> q)
  q1@(QueryMap n1 m1 d1) <> q2@(QueryMap n2 m2 d2) = case compare n1 n2 of
    LT -> QueryMap n1 m1 (d1 <> q2)
    EQ -> QueryMap n1 (m1 <> m2) (d1 <> d2)
    GT -> QueryMap n2 m2 (q1 <> d2)

instance Monoid a => Monoid (QueryMap a) where
  mempty = QueryFinal mempty
  mappend (QueryFinal a) (QueryFinal b) = QueryFinal (mappend a b)
  mappend q@(QueryFinal _) (QueryMap n m d) = QueryMap n m (mappend q d)
  mappend (QueryMap n m d) q@(QueryFinal _) = QueryMap n m (mappend d q)
  mappend q1@(QueryMap n1 m1 d1) q2@(QueryMap n2 m2 d2) = case compare n1 n2 of
    LT -> QueryMap n1 m1 (mappend d1 q2)
    EQ -> QueryMap n1 (mappend m1 m2) (mappend d1 d2)
    GT -> QueryMap n2 m2 (mappend q1 d2)

-- |The empty query map.
emptyQueryMap :: QueryMap a
emptyQueryMap = QueryFinal Nothing

-- |The constant query map, always returning the same value.
defaultQueryMap :: a -> QueryMap a
defaultQueryMap = QueryFinal . Just

-- |The query map with a single item, which maps queries containing the given query variable matching the placeholder to the specified @a@ value.
singletonQuery :: QueryString -> Placeholder QueryString p -> a -> QueryMap a
singletonQuery n p v = QueryMap n (singletonPlaceholder p $ defaultQueryMap v) emptyQueryMap

-- |A 'singletonQuery' map with a 'DynamicState' value to parse the placeholder.
singletonQueryState :: QueryString -> Placeholder QueryString p -> QueryMap (DynamicState p)
singletonQueryState n p = QueryMap n (defaultQueryMap <$> singletonPlaceholderState p) emptyQueryMap

-- |Lookup a URL query in the query map and return all matching results.
lookupQuery :: QueryParams -> QueryMap a -> [DynamicResult a]
lookupQuery _ (QueryFinal Nothing) = []
lookupQuery _ (QueryFinal (Just a)) = [([], a)]
lookupQuery q (QueryMap n m d)
  | Just qv <- HM.lookup n q = do
    s <- qv
    lookupPlaceholderWith s m $ lookupQuery q
  | otherwise = lookupQuery q d
