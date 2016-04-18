-- |
-- Single-route path parsers.
-- The most important type here is 'Path', which can be used to represent a single path endpoint within your application, including placeholders.
-- For example, the following represents a path of @\/item\/$id@ where @$id@ is an integer placeholder:
--
-- >>> import Control.Invariant.Monoidal
-- >>> let p = PathFixed "item" *< parameter (undefined :: Int)
-- >>> parsePath p ["item", "123"]
-- 123
-- >>> producePath p 123
-- ["item","123"]
--
-- When put in a 'PathRoute' or similar type, it can be used to represent complete actions.
-- These can then be combined into a routing table using "Web.Route.Path.Map".
{-# LANGUAGE GADTs, TupleSections, TypeFamilies #-}
module Web.Route.Path
  ( normalizePath
  , Path(PathFixed, PathParameter)
  , parameter
  , PathSegment(..)
  , pathSegments
  , producePath
  , urlPathBuilder
  , parsePath
  , PathRoute(..)
  ) where

import Prelude hiding (lookup)

import qualified Data.ByteString.Builder as B
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (MonadPlus, mzero, guard)
import qualified Data.Isomorphism.Type as I
import Data.Monoid ((<>))

import Web.Route.Resolver
import Web.Route.String
import Web.Route.Parameter
import Web.Route.Path.Internal
import qualified Web.Route.Path.Map as M
import Web.Route.Class

-- |Remove double- and trailing-slashes (i.e., empty path segments).
normalizePath :: RouteString s => [s] -> [s]
normalizePath = filter (not . nullString)

-- |Create a path placeholder parameter ('PathParameter') with the type of the argument, which is ignored.
parameter :: Parameter s a => a -> Path s a
parameter _ = PathParameter

-- |An abstract representation of a path segment, distinguishing fixed components ('PathSegmentFixed') from placeholders ('PathSegmentParameter').
data PathSegment s where
  PathSegmentFixed :: !s -> PathSegment s
  PathSegmentParameter :: Parameter s a => a -> PathSegment s

-- |Realize a 'Path' as applied to a value to a sequence of 'PathSegment's.
pathSegments :: Path s a -> a -> [PathSegment s]
pathSegments PathEmpty () = []
pathSegments (PathFixed t) () = [PathSegmentFixed t]
pathSegments PathParameter a = [PathSegmentParameter a]
pathSegments (PathIso f p) a = pathSegments p $ I.isoFrom f a
pathSegments (PathJoin p q) (a, b) = pathSegments p a ++ pathSegments q b
pathSegments (PathChoose p _) (Left a) = pathSegments p a
pathSegments (PathChoose _ p) (Right a) = pathSegments p a

renderPathSegment :: PathSegment s -> s
renderPathSegment (PathSegmentFixed s) = s
renderPathSegment (PathSegmentParameter a) = renderParameter a

-- |Render a 'Path' as applied to a value to a list of string segments.
producePath :: Path s a -> a -> [s]
producePath p a = map renderPathSegment $ pathSegments p a

-- |Build a 'Path' as applied to a value into a bytestring 'B.Builder' by encoding the segments with 'urlEncodePath' and joining them with \"/\".
urlPathBuilder :: RouteString s => Path s a -> a -> B.Builder
urlPathBuilder p a = foldMap es $ pathSegments p a where
  es s = B.char7 '/' <> urlEncodePath (renderPathSegment s)

-- |Attempt to parse path segments into a value and remaining (unparsed) segments, ala 'reads'.
readsPath :: (MonadPlus m, Eq s) => Path s a -> [s] -> m (a, [s])
readsPath PathEmpty l = return ((), l)
readsPath (PathFixed t) (a:l) = (, l) <$> guard (a == t)
readsPath PathParameter (a:l) = (, l) <$> maybeA (parseParameter a)
readsPath (PathIso f p) a = first (I.isoTo f) <$> readsPath p a
readsPath (PathJoin p q) a = do
  (pr, a') <- readsPath p a
  first (pr, ) <$> readsPath q a'
readsPath (PathChoose p q) a = first Left <$> readsPath p a <|> first Right <$> readsPath q a
readsPath _ [] = mzero

-- |Parse a path into possible values.  Can return all possible values as a list or (usually) a single value as 'Maybe'.
parsePath :: (MonadPlus m, Eq s) => Path s a -> [s] -> m a
parsePath p l = do
  (a, []) <- readsPath p l
  return a

{-
instance Route PathRoute where
  type RouteMap PathRoute s a = M.PathMap s a
  route (PathRoute p f) = M.singleton $ PathRoute p $ Exactly . f

lookupPathRoute :: RouteString s => [s] -> RouteMap PathRoute s a -> Maybe a
lookupPathRoute l p = exactlyToMaybe (M.lookup l p)
-}
