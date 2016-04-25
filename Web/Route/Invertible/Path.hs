-- |
-- Single-route path parsers.
-- The most important type here is 'Path', which can be used to represent a single path endpoint within your application, including placeholders.
-- For example, the following represents a path of @\/item\/$id@ where @$id@ is an integer placeholder:
--
-- >>> import Control.Invertible.Monoidal
-- >>> let p = fromString "item" *< parameter :: Path Int
-- >>> parsePath p $ map T.pack ["item", "123"]
-- 123
-- >>> renderPath p 123
-- ["item","123"]
--
-- When put in a 'PathRoute' or similar type, it can be used to represent complete actions.
-- These can then be combined into a routing table using "Web.Route.Invertible.Map.Path".
{-# LANGUAGE GADTs, TupleSections #-}
module Web.Route.Invertible.Path
  ( PathString
  , Path
  , normalizePath
  , parameter
  , pathSegments
  , renderPath
  , urlPathBuilder
  , parsePath
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Builder as B
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (MonadPlus, mzero, guard)
import qualified Data.Invertible as I
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types.URI as H

import Web.Route.Invertible.Parameter
import Web.Route.Invertible.Placeholder
import Web.Route.Invertible.Path.Types

-- |Remove double- and trailing-slashes (i.e., empty path segments).
normalizePath :: [PathString] -> [PathString]
normalizePath = filter (not . T.null)

-- |Realize a 'Path' as applied to a value to a sequence of 'PlaceholderValue's.
pathSegments :: Path a -> a -> [PlaceholderValue PathString]
pathSegments PathEmpty () = []
pathSegments (PathPlaceholder (PlaceholderFixed t)) () = [PlaceholderValueFixed t]
pathSegments (PathPlaceholder (PlaceholderParameter)) a = [PlaceholderValueParameter a]
pathSegments (PathTransform f p) a = pathSegments p $ I.biFrom f a
pathSegments (PathJoin p q) (a, b) = pathSegments p a ++ pathSegments q b
pathSegments (PathChoose p _) (Left a) = pathSegments p a
pathSegments (PathChoose _ p) (Right a) = pathSegments p a

-- |Render a 'Path' as applied to a value to a list of string segments.
renderPath :: Path a -> a -> [PathString]
renderPath p a = map renderPlaceholderValue $ pathSegments p a

-- |Build a 'Path' as applied to a value into a bytestring 'B.Builder' by encoding the segments with 'urlEncodePath' and joining them with \"/\".
urlPathBuilder :: Path a -> a -> B.Builder
urlPathBuilder p a = foldMap es $ pathSegments p a where
  es s = B.char7 '/' <> H.urlEncodeBuilder False (TE.encodeUtf8 (renderPlaceholderValue s))

-- |Attempt to parse path segments into a value and remaining (unparsed) segments, ala 'reads'.
readsPath :: MonadPlus m => Path a -> [PathString] -> m (a, [PathString])
readsPath PathEmpty l = return ((), l)
readsPath (PathPlaceholder (PlaceholderFixed t)) (a:l) = (, l) <$> guard (a == t)
readsPath (PathPlaceholder (PlaceholderParameter)) (a:l) = (, l) <$> maybe mzero return (parseParameter a)
readsPath (PathTransform f p) a = first (I.biTo f) <$> readsPath p a
readsPath (PathJoin p q) a = do
  (pr, a') <- readsPath p a
  first (pr, ) <$> readsPath q a'
readsPath (PathChoose p q) a = first Left <$> readsPath p a <|> first Right <$> readsPath q a
readsPath _ [] = mzero

-- |Parse a path into possible values.  Can return all possible values as a list or (usually) a single value as 'Maybe'.
parsePath :: MonadPlus m => Path a -> [PathString] -> m a
parsePath p l = do
  (a, []) <- readsPath p l
  return a

{-
instance Route PathRoute where
  type RouteMap PathRoute s a = M.PathMap s a
  route (PathRoute p f) = M.singleton $ PathRoute p $ Exactly . f

lookupPathRoute :: RouteString s => [s] -> RouteMap PathRoute s a -> Maybe a
lookupPathRoute l p = exactlyToMaybe (M.lookup l p)

newtype PathRouteT p r a = PathRouteT (PathRoute p (r a))
instance Functor r => Functor (PathRouteT p r) where
  fmap f (PathRouteT x) = PathRouteT (fmap (fmap f) x)
newtype PathRouteM r a = PathRouteM (M.PathMap (r a))
  deriving (Monoid)
instance Functor r => Functor (PathRouteM r) where
  fmap f (PathRouteM x) = PathRouteM (fmap (fmap f) x)

instance Route r => Route (PathRouteT p r) where
  type RouteMap (PathRouteT p r) = PathRouteM (RouteMap r)
  route (PathRouteT (PathRoute p f)) = PathRouteM $ M.singleton $ PathRoute p $ route . f
  routeRequest q = routeRequest q . route
instance Route r => Route (PathRouteM r) where
  routeRequest q (PathRouteM r) = M.lookup (routePath q) r
-}
