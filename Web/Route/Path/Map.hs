-- |
-- An efficient routing map for URL paths.
-- This is the core of the routing infrastructure.
-- If you have a set of actions represented as (or convertable to) 'PathRoute's, you can create a routing table using 'mconcat' and 'singleton':
-- 
-- >>> :set -XOverloadedStrings
-- >>> import Control.Invariant.Monoidal
-- >>> let a1 = PathRoute ("item" *< PathParameter :: Path String Int) (return . Left)
-- >>> let a2 = PathRoute ("object" *< PathParameter :: Path String String) (return . Right)
-- >>> let r = mconcat [singleton a1, singleton a2 {- ... -}] :: PathMap String [Either Int String]
-- >>> lookup ["object", "foo"] r
-- [Right "foo"]
-- >>> lookup ["item", "123"] r
-- [Left 123]
-- >>> lookup ["item", "bar"] r
-- []
--
{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables #-}
module Web.Route.Path.Map
  ( PathMap
  , unionWith
  , union
  , singleton
  , lookup
  , insert
  ) where

import Prelude hiding (lookup)

import Control.Applicative (Alternative, (<|>), liftA2)
import Control.Arrow (first)
import Control.Monad (mzero)
import Data.Dynamic (Dynamic, fromDynamic)
import qualified Data.HashMap.Strict as HM
import qualified Data.Isomorphism.Type as I
import qualified Data.Map.Strict as M

import Web.Route.String
import qualified Web.Route.Map as RM
import qualified Web.Route.Parameter.TypeMap as PM
import Web.Route.Path.Internal

-- |A routing map for 'Path' parsers.
data PathMap s a = PathMap
  { _pathMapEnd :: !([Dynamic] -> a)
  , _pathMapFixed :: HM.HashMap s (PathMap s a)
  , _pathMapParameter :: PM.ParameterTypeMap s (PathMap s a)
  }

-- |Values are combined using 'mappend'.
instance (RouteString s, Monoid a) => Monoid (PathMap s a) where
  mempty = leaf (const mempty)
  mappend = unionWith mappend

instance Functor (PathMap s) where
  fmap f = mapEnds (fmap f)

leaf :: ([Dynamic] -> a) -> PathMap s a
leaf l = PathMap l HM.empty PM.empty

mapEnds :: (([Dynamic] -> a) -> ([Dynamic] -> b)) -> PathMap s a -> PathMap s b
mapEnds f (PathMap e m p) = PathMap (f e) (mapEnds f <$> m) (mapEnds f <$> p)

unionWith :: RouteString s => (a -> a -> a) -> PathMap s a -> PathMap s a -> PathMap s a
unionWith f (PathMap e1 m1 (RM.MonoidMap p1)) (PathMap e2 m2 (RM.MonoidMap p2)) =
  PathMap (liftA2 f e1 e2) (HM.unionWith (unionWith f) m1 m2) (RM.MonoidMap $ M.unionWith (unionWith f) p1 p2)

-- |Left-biased union, equivalent to @'unionWith' 'const'@
union :: RouteString s => PathMap s a -> PathMap s a -> PathMap s a
union = unionWith const

unionAlt :: (RouteString s, Alternative m) => PathMap s (m a) -> PathMap s (m a) -> PathMap s (m a)
unionAlt = unionWith (<|>)

singleton' :: (RouteString s) => Path s a -> PathMap s [(a, [Dynamic])]
singleton' PathEmpty = leaf $ return . (,) ()
singleton' (PathFixed s) =
  PathMap (const mzero) (HM.singleton s $ leaf $ return . (,) ()) PM.empty
singleton' p@PathParameter =
  PathMap (const mzero) HM.empty (PM.singleton p $ leaf $ \(h:l) -> (, l) <$> maybeA (fromDynamic h))
singleton' (PathIso f m) = fmap (first $ I.isoTo f) <$> singleton' m
singleton' (PathJoin p q) = jq $ singleton' p where
  q' = singleton' q
  jq (PathMap e m n) = mapEnds (me e) q' `unionAlt`
    PathMap (const mzero) (jq <$> m) (jq <$> n)
  me e f l = do
    (a, al) <- e l
    (b, bl) <- f al
    return ((a, b), bl)
singleton' (PathChoose p q) =
  (fmap (first Left) <$> singleton' p) `unionAlt`
  (fmap (first Right) <$> singleton' q)

-- |Create a map from a single 'Path' parser.  Since this abstracts the type of the path @b@ (but not @a@), paths with different underlying types can be combined in the same map.
singleton :: (RouteString s, Monoid a) => PathRoute s a b -> PathMap s a
singleton (PathRoute p f) = foldMap (f . fst) <$> singleton' p

lookup' :: RouteString s => ([Dynamic] -> [Dynamic]) -> [s] -> PathMap s a -> [a]
lookup' p [] (PathMap e _ _) = [e (p [])]
lookup' p (s:l) (PathMap _ m n)
  | Just ms <- HM.lookup s m = lookup' p l ms
  | otherwise = do
    (x, ns) <- PM.lookup s n
    lookup' (p . (x:)) l ns

-- |Lookup a path in the map and return the value, combining ambiguous paths using the 'Monoid' instance on their values.
-- Generally /O(log n)/ in the total number of paths, except /O(n)/ in the length of the path and the number of different (ambiguous) 'PathParameter' types at each level (from 'PM.lookup').
lookup :: RouteString s => Monoid a => [s] -> PathMap s a -> a
lookup l p = mconcat $ lookup' id l p

-- |Add a 'Path' parser to a map.  Equvialent to @'mappend' . 'singleton'@.
insert :: forall s a b . (RouteString s, Monoid a) => PathRoute s a b -> PathMap s a -> PathMap s a
insert = mappend . singleton
