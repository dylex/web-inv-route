{-# LANGUAGE FlexibleContexts #-}
module Web.Route.Invertible.Map.Query
  ( QueryMap(..)
  , defaultQueryMap
  , singletonQuery
  , singletonQueryState
  , lookupQuery
  ) where

import qualified Data.HashMap.Lazy as HM
import Data.Monoid ((<>))

import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Map.Placeholder
import Web.Route.Invertible.Query
import Web.Route.Invertible.Dynamics

data QueryMap a = QueryFinal (Maybe a) | QueryMap
  { queryParam :: !QueryString
  , queryMap :: !(PlaceholderMap QueryString (QueryMap a))
  , queryMissing :: !(QueryMap a)
  } deriving (Eq, Show)

instance Functor QueryMap where
  fmap f (QueryFinal v) = QueryFinal (fmap f v)
  fmap f (QueryMap n m d) = QueryMap n (fmap (fmap f) m) (fmap f d)

instance Monoid a => Monoid (QueryMap a) where
  mempty = QueryFinal mempty
  mappend (QueryFinal a) (QueryFinal b) = QueryFinal (mappend a b)
  mappend q@(QueryFinal _) (QueryMap n m d) = QueryMap n m (q <> d)
  mappend (QueryMap n m d) q@(QueryFinal _) = QueryMap n m (d <> q)
  mappend q1@(QueryMap n1 m1 d1) q2@(QueryMap n2 m2 d2) = case compare n1 n2 of
    LT -> QueryMap n1 m1 (d1 <> q2)
    EQ -> QueryMap n1 (m1 <> m2) (d1 <> d2)
    GT -> QueryMap n2 m2 (q1 <> d2)

emptyQueryMap :: QueryMap a
emptyQueryMap = QueryFinal Nothing

defaultQueryMap :: a -> QueryMap a
defaultQueryMap = QueryFinal . Just

singletonQuery :: QueryString -> Placeholder QueryString p -> a -> QueryMap a
singletonQuery n p v = QueryMap n (singletonPlaceholder p $ defaultQueryMap v) emptyQueryMap

singletonQueryState :: QueryString -> Placeholder QueryString p -> QueryMap (DynamicState p)
singletonQueryState n p = QueryMap n (defaultQueryMap <$> singletonPlaceholderState p) emptyQueryMap

lookupQuery :: QueryParams -> QueryMap a -> [DynamicResult a]
lookupQuery _ (QueryFinal Nothing) = []
lookupQuery _ (QueryFinal (Just a)) = [([], a)]
lookupQuery q (QueryMap n m d)
  | Just qv <- HM.lookup n q = do
    s <- qv
    lookupPlaceholderWith s m $ lookupQuery q
  | otherwise = lookupQuery q d
